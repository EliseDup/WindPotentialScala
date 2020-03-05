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
    /* 
    val u = scala.collection.mutable.ArrayBuffer.empty[Energy]; u += Calibration.data.u(ind)
    val u_nre = scala.collection.mutable.ArrayBuffer.empty[Energy]; u_nre += Calibration.data.u(ind) - MegaTonOilEquivalent(45)
    val u_re = scala.collection.mutable.ArrayBuffer.empty[Energy]; u_re += MegaTonOilEquivalent(45)
    val a = scala.collection.mutable.ArrayBuffer.empty[Energy]; a += Calibration.data.a(ind)
    val a_nre = scala.collection.mutable.ArrayBuffer.empty[Energy]; a += Calibration.data.a(ind)
    val a_re = scala.collection.mutable.ArrayBuffer.empty[Energy]; a += Joules(0);
    // Wind and Solar production in 2017
    val e_re = scala.collection.mutable.ArrayBuffer.empty[Energy]; e_re += MegaTonOilEquivalent(45)
    val e_nre = scala.collection.mutable.ArrayBuffer.empty[Energy]; e_nre += Calibration.data.e(ind) - MegaTonOilEquivalent(45)
    val e = scala.collection.mutable.ArrayBuffer.empty[Energy]; e += e_re.last + e_nre.last
    val year = scala.collection.mutable.ArrayBuffer.empty[Int]; year += start_year

    val ve = scala.collection.mutable.ArrayBuffer.empty[Double]; ve += calib._4
    val ke = scala.collection.mutable.ArrayBuffer.empty[Double]; ke += calib._6
    val ke_nre = scala.collection.mutable.ArrayBuffer.empty[Double]; ke_nre += calib._6
    val ke_re = scala.collection.mutable.ArrayBuffer.empty[Double]; ke_re += 0

    val qe = scala.collection.mutable.ArrayBuffer.empty[Double]; qe += Calibration.data.qe(ind)
    val eroi = scala.collection.mutable.ArrayBuffer.empty[Double]; eroi += calib._1

*/
    // We want to reach 100% renewables by end_year: 
    val n_year = end_year - start_year; val e_re0 = res_re.e.last;
    val qe_0 = Calibration.data.qe(ind); val ve_0 = calib._4; val ke_0 = calib._6
    val growth_rate = math.pow(Calibration.data.e(ind) / e_re0, 1.0 / n_year)
    println("Simulation " + n_year + ", exponential growth rate " + growth_rate)

    // If we consider an exponential growth (i.e. a constant growth rate): 
    // Ere,n = Enre,2017 & Ere,n = Ere,2017 * g^n => g = (Enre,2017/Ere,2017)^1/n
    for (y <- 1 to n_year) {
      // year += start_year + y
      // The desired total production from RE sources by the end of the year.
      val target_total_re = e_re0 * math.pow(growth_rate, y)
      val target_new_re = List(target_total_re - res_re.e.last, Joules(0)).maxBy(_.to(MegaTonOilEquivalent))
      println("Simulate year " + res.year.last + " with target " + target_new_re)

      techs_it.map(it => it.simulate_year(y, target_new_re / techs.size))
      // Update params from RE sector
      res_re.updateProduction(start_year + y, techs_it.map(_.results.u.last).foldLeft(Joules(0))(_ + _),
        techs_it.map(_.results.e.last).foldLeft(Joules(0))(_ + _),
        techs_it.map(_.results.a.last).foldLeft(Joules(0))(_ + _),
        techs_it.map(_.results.ke.last).sum)
      val e_nre = List(res_nre.e.last - res_re.e.last, Joules(0)).maxBy(_.to(MegaTonOilEquivalent))
      val u_nre = e_nre * (1 + qe_0)
      res_nre.updateProduction(start_year + y, u_nre, e_nre, u_nre * qe_0, u_nre.to(KilowattHours) * ve_0)
      res.sumResults(start_year + y, List(res_re,res_nre))
      // res.updateProduction(start_year + y, res_re.u.last+res_nre.u.last, res_re.e.last+res_nre.e.last, res_re.a.last+res_nre.a.last, res_re.ke.last+res_nre.ke.last)
      
      /*e_re += techs_it.map(_.results.e.last).foldLeft(Joules(0))(_ + _)
      a_re += techs_it.map(_.results.a.last).foldLeft(Joules(0))(_ + _)
      u_re += techs_it.map(_.results.u.last).foldLeft(Joules(0))(_ + _)
      ke_re += techs_it.map(_.results.ke.last).sum

      // Update params from NRE sector, assuming technology is the same as in 2017 (i.e. same qe and ve)
      e_nre += List(e_nre.last - e_re.last, Joules(0)).maxBy(_.to(MegaTonOilEquivalent))
      u_nre += e_nre.last * (1 + qe_0)
      ke_nre += u_nre.last.toKilowattHours * ve_0 // The capital stock of the nre sector decreases : suppose a constant ve (units capital stock needed / unit of energy)
      a_nre += u_nre.last * qe_0

      // Update global params of the energy sector
      a += a_re.last + a_nre.last; e += e_re.last + e_nre.last; u += u_re.last + u_nre.last; ke += ke_re.last + ke_nre.last;
      ve += ke.last / u.last.toKilowattHours
      qe += a.last / u.last
      eroi += 1 / (qe.last + delta * qy * ve.last)*/
    }

    val year_double = res.year.toList.map(_.toDouble)
    val ys = List((res.ve.toList, "ve"), (res.qe.toList, "qe"), (res.u.toList.map(_.to(Exajoules)), "U [EJ/year]"), (res.e.toList.map(_.to(Exajoules)), "E [EJ/year]"),
      (res.eroi.toList, "EROI"))
    combinedPlots(year_double, ys)
    plotXY(List((year_double, res.e.toList.map(_.to(Exajoules)), "E"), (year_double, res_re.e.toList.map(_.to(Exajoules)), "E_RE"), (year_double, res_nre.e.toList.map(_.to(Exajoules)), "E_NRE")), legend = true)
    plotXY(List((year_double, res.u.toList.map(_.to(Exajoules)), "U"), (year_double, res_re.u.toList.map(_.to(Exajoules)), "U_RE"), (year_double, res_nre.u.toList.map(_.to(Exajoules)), "U_NRE")), legend = true)

  }

  class TechnologyIterator(tech: RenewableTechnology, sites: List[Cell], delta: Double, qy: Double) {

    val sites_sorted = sites.filter(s => tech.suitabilityFactor(s) > 0).sortBy(tech.eroi(_, 1.0)).reverse.toIterator
    val init = ProductionFunction.initialValues(tech)

    val results = new GrowthModelResults(delta, qy)
    results.updateProduction(2017, init._2, init._2, Joules(0), 0)

    val tilde_ke = scala.collection.mutable.ArrayBuffer.empty[Energy]; tilde_ke += Joules(0)
    val tilde_ie = scala.collection.mutable.ArrayBuffer.empty[Energy]; tilde_ie += Joules(0)
    val installed_cap = scala.collection.mutable.ArrayBuffer.empty[Power]; installed_cap += init._1

    //val u = scala.collection.mutable.ArrayBuffer.empty[Energy]; u += init._2
    //val a = scala.collection.mutable.ArrayBuffer.empty[Energy]; a += Joules(0)
    //val e = scala.collection.mutable.ArrayBuffer.empty[Energy]; e += init._2

    //val year = scala.collection.mutable.ArrayBuffer.empty[Int]; year += 2017

    //val ve = scala.collection.mutable.ArrayBuffer.empty[Double]; ve += 0;
    //val ke = scala.collection.mutable.ArrayBuffer.empty[Double]; ke += 0;
    //val qe = scala.collection.mutable.ArrayBuffer.empty[Double]; qe += 0;
    //val eroi = scala.collection.mutable.ArrayBuffer.empty[Double]; eroi += 0;

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
      println(tech.name + ", year " + y + "new production:" + newProd.to(MegaTonOilEquivalent))
      // Update parameters !
      installed_cap += (installed_cap.last + newCap)
      // Investment = new capacity + depreciation compensation of installed capacity
      tilde_ie += newKe + tilde_ke.last * delta
      // Energy sector capital stock: Capital stock(t-1) + new capital
      tilde_ke += tilde_ke.last + newKe
      results.updateProduction(y, results.u.last + newProd + newOperationE, results.e.last + newProd, results.a.last + newOperationE, tilde_ke.last.toKilowattHours / qy)
      //u += u.last + newProd + newOperationE
      //e += e.last + newProd
      //a += a.last + newOperationE

      //ke += tilde_ke.last.toKilowattHours / qy
      //ve += ke.last / u.last.toKilowattHours
      //qe += a.last / u.last
      //eroi += 1 / (qe.last + delta * qy * ve.last)
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
  def sumResults(y : Int, res : List[GrowthModelResults]){
    year += y
    u += res.map(_.u.last).foldLeft(Joules(0))(_ + _)
    e += res.map(_.e.last).foldLeft(Joules(0))(_ + _)
    a += res.map(_.a.last).foldLeft(Joules(0))(_ + _)
    ke += res.map(_.ke.last).sum
    updateParams
  }
}