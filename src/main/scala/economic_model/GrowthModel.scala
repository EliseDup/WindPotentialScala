package economic_model

import utils._
import squants.energy._
import wind_solar._
import wind_energy._
import solar_energy._
import squants.time.Hours

object GrowthModel {

  import Helper._
  import PlotHelper._

  def main(args: Array[String]): Unit = {
    //Calibration.printTableCalibration(1990, List(0.25, 0.8), List(Calibration.delta_(15), Calibration.delta_(40)), List(0.05, 0.08), List(0.1 / 100))
    //Calibration.printTableCalibration_new(1990, List(10, 25), List(0.03, 0.07), List(0.03, 0.1), List(0.1 / 100))
    simulateTransition(0.25, Calibration.delta_(25), 0.05, 2017, 2050)
  }

  def simulateTransition(s: Double, delta: Double, alpha: Double, start_year: Int, end_year: Int) {

    val all_sites = Grid().cells
    val techs = List(OnshoreWindTechnology, OffshoreWindTechnology, PVMono) //, CSPTowerStorage12h)
    val delta = Calibration.delta_(15)

    // (eroi, qy, vy, ve, v)
    val calib = Calibration.calibration_results_work(2017, delta, 0.05, 0.05, 0.1 / 100)
    val qy = calib._2
    val techs_it = techs.map(tech => new TechnologyIterator(tech, all_sites, delta, qy))
    val ind = Calibration.index_year(start_year)

    val res = new GrowthModelResults(delta, qy); val res_re = new GrowthModelResults(delta, qy); val res_nre = new GrowthModelResults(delta, qy);
    res.updateProduction(start_year, Calibration.data.u(ind), Calibration.data.e(ind), Calibration.data.a(ind), calib._6)
    res_nre.updateProduction(start_year, Calibration.data.u(ind) - MegaTonOilEquivalent(45), Calibration.data.e(ind) - MegaTonOilEquivalent(45), Calibration.data.a(ind), calib._6)
    res_re.updateProduction(start_year, MegaTonOilEquivalent(45), MegaTonOilEquivalent(45), Joules(0), 0)

    // We want to reach 100% renewables by end_year: 
    val n_year = end_year - start_year;
    val qe_0 = Calibration.data.qe(ind); val ve_0 = calib._4; val ke_0 = calib._6
    val e_re0 = res_re.e.last; val e_0 = Calibration.data.e(ind)
    // If we consider an exponential growth (i.e. a constant growth rate): 
    // Ere,n = E,2017 & Ere,n = Ere,2017 * g^n => g = (E,2017/Ere,2017)^1/n
    val growth_rate = math.pow(e_0 / e_re0, 1.0 / n_year)
    println("Simulation " + n_year + ", exponential growth rate " + growth_rate)

    for (y <- 1 to n_year) {
      // The desired total production from RE sources by the end of the year.
      val target_re = List(e_re0 * math.pow(growth_rate, y) - res_re.e.last, Joules(0)).maxBy(_.value)
      println("Simulate year " + res.year.last + " with target " + target_re.to(MegaTonOilEquivalent))

      techs_it.map(it => it.simulate_year(y, target_re / techs.size))
      // Update params from RE sector
      res_re.sumResults(start_year + y, techs_it.map(_.results))
      // Remove from the total the energy that is now produced by the re sector
      // e_nre < 0 is not supposed to happen before the last year of the simulation !!
      val e_nre = List(e_0 - res_re.e.last, Joules(0)).maxBy(_.value)
      val u_nre = e_nre / (1 - qe_0)
      res_nre.updateProduction(start_year + y, u_nre, e_nre, u_nre * qe_0, u_nre.to(KilowattHours) * ve_0)
      res.sumResults(start_year + y, List(res_re, res_nre))
    }

    val year_double = res.year.toList.map(_.toDouble)
    val ys = List((res.ve.toList, "ve"), (res.qe.toList, "qe"), (res.eroi.toList, "EROI"))
    combinedPlots(year_double, ys)
    plotXY(List((year_double, res.e.toList.map(_.to(Exajoules)), "E"), (year_double, res_re.e.toList.map(_.to(Exajoules)), "E_RE"), (year_double, res_nre.e.toList.map(_.to(Exajoules)), "E_NRE")), legend = true)
    plotXY(List((year_double, res.u.toList.map(_.to(Exajoules)), "U"), (year_double, res_re.u.toList.map(_.to(Exajoules)), "U_RE"), (year_double, res_nre.u.toList.map(_.to(Exajoules)), "U_NRE")), legend = true)
    plotXY(List((year_double, res.ke.toList, "Ke"), (year_double, res_re.ke.toList, "Ke_RE"), (year_double, res_nre.ke.toList, "Ke_NRE")), legend = true)

  }

  class TechnologyIterator(tech: RenewableTechnology, sites: List[Cell], delta: Double, qy: Double, log: Boolean = false) {

    val sites_sorted = sites.filter(s => tech.suitabilityFactor(s) > 0).sortBy(tech.eroi(_, 1.0)).reverse.toIterator
    val init = ProductionFunction.initialValues(tech)

    val results = new GrowthModelResults(delta, qy)
    results.updateProduction(2017, init._2, init._2, Joules(0), 0)

    val tilde_ke = scala.collection.mutable.ArrayBuffer.empty[Energy]; tilde_ke += Joules(0)
    val tilde_ie = scala.collection.mutable.ArrayBuffer.empty[Energy]; tilde_ie += Joules(0)
    val installed_cap = scala.collection.mutable.ArrayBuffer.empty[Power]; installed_cap += init._1

    def simulate_year(y: Int, target: Energy) {
      // year += y
      var ended = false
      var newCap = Watts(0); var newProd = Joules(0); var newKe = Joules(0); var newOperationE = Joules(0)
      while (newProd <= target && !ended) {
        if (sites_sorted.hasNext) {
          val next_site = sites_sorted.next()
          val prod = tech.potential(next_site, 1.0) * Hours(365 * 24)
          newProd += prod
          newOperationE += tech.energyInputsOMYearly(next_site, 1.0) // + prod * tech.operation_variable 
          newKe += tech.energyInputsInstallation(next_site, 1.0) // + tech.energyInputsDecomissioning(next_site, 1.0)
          newCap += tech.ratedPower(next_site, 1.0)
        } else {
          ended = true
        }
      }
      if (log) println(tech.name + ", year " + y + "new production:" + newProd.to(MegaTonOilEquivalent))
      // Update parameters !
      installed_cap += (installed_cap.last + newCap)
      // Investment = new capacity + depreciation compensation of installed capacity
      tilde_ie += newKe + tilde_ke.last * delta
      // Energy sector capital stock: Capital stock(t-1) + new capital
      tilde_ke += tilde_ke.last + newKe
      results.updateProduction(y, results.u.last + newProd + newOperationE, results.e.last + newProd, results.a.last + newOperationE, tilde_ke.last.toKilowattHours / qy)
    }
  }
}

class GrowthModelResults(delta: Double, qy: Double) {
  val year = scala.collection.mutable.ArrayBuffer.empty[Int];
  val u = scala.collection.mutable.ArrayBuffer.empty[Energy];
  val a = scala.collection.mutable.ArrayBuffer.empty[Energy];
  val e = scala.collection.mutable.ArrayBuffer.empty[Energy];
  val ke = scala.collection.mutable.ArrayBuffer.empty[Double];
  val ve = scala.collection.mutable.ArrayBuffer.empty[Double];
  val qe = scala.collection.mutable.ArrayBuffer.empty[Double];
  val eroi = scala.collection.mutable.ArrayBuffer.empty[Double];

  def updateProduction(y: Int, newU: Energy, newE: Energy, newA: Energy, newKe: Double) {
    year += y
    u += newU
    e += newE
    a += newA
    ke += newKe
    updateParams
  }
  def updateParams {
    ve += ke.last / u.last.toKilowattHours
    qe += a.last / u.last
    eroi += 1 / (qe.last + delta * qy * ve.last)
  }
  def sumResults(y: Int, res: List[GrowthModelResults]) {
    year += y
    u += res.map(_.u.last).foldLeft(Joules(0))(_ + _)
    e += res.map(_.e.last).foldLeft(Joules(0))(_ + _)
    a += res.map(_.a.last).foldLeft(Joules(0))(_ + _)
    ke += res.map(_.ke.last).sum
    updateParams
  }
}