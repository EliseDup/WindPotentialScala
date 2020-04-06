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

  def round(x: Double) = {
    math.round(100 * x) / 100.0
  }
  def main(args: Array[String]): Unit = {

    val years = (1990 to 2017).toList
    
    val cal = Calibration2017() // .units(MegaTonOilEquivalent,1E9,1E6)
    println(cal.ki + "\t" + cal.k + "\t" + cal.ks)
    println(cal.gi + "\t" + cal.g + "\t" + cal.gs)
    
    val cal2 = new calibration_results_work()
    println(cal2.ki + "\t" + cal2.k + "\t" + cal2.ks)
    println(cal2.gi + "\t" + cal2.g + "\t" + cal2.gs)
    
    println(cal.p + "\t" +cal2.p)
    println(cal.qf + "\t" + cal2.qf)
    
    /*
    val share = (1 to 10).map(_ * 0.1).toList
    val techs = List((OnshoreWindTechnology, 1.0 / 4), (OffshoreWindTechnology, 1.0 / 4), (PVMono, 1.0 / 2))
    val res = share.map(s => (s, calculate(Exajoules(400), 25, techs, s)))
    println("RE Share [%]" + "\t" + "EROI" + "\t" + " qe [%] " + "\t" + " ve " +  "\t" + " gi " +"\t" + "gs")
    
    res.map(r => {
      val intg = cal.interval_gk(cal.s, cal.n, r._2(1)._3, cal.vf, r._2(1)._2, cal.qf, cal.le, cal.lf)
      println(r._1 * 100 + " & " + round(r._2(1)._1) + " & " + round(r._2(1)._2 * 100) + " & " + round(r._2(1)._3) + "& " +
        intg._1 + "\t" + intg._2 + "\\" + "\\")})

    plotXY(List((share, res.map(_._2(1)._1), "")), xLabel = "RE Share", yLabel = "EROI")
    plotXY(List((share, res.map(_._2(1)._2 * 100), "")), xLabel = "RE Share", yLabel = "qe [%]")
    plotXY(List((share, res.map(_._2(1)._3), "")), xLabel = "RE Share", yLabel = "ve")

    combinedPlots(share.map(_ * 100), List((res.map(_._2(1)._1), "EROI"), (res.map(_._2(1)._2 * 100), "qe [%]"), (res.map(_._2(1)._3), "ve")), xLabel = "RE Share [%]")
    */
  }

  // For a given target (= final energy demand), and renewable energy share, gives a estimate of the technical parameters and EROI of the energy system
  def calculate(target: Energy, T: Int, techs: List[(RenewableTechnology, Double)], share_re: Double = 1.0) = {

    val all_sites = Grid().cells
    val calib = Calibration2017.T(T)
    val qf = calib.qf
    val delta = calib.delta

    // Initialise 
    val start_year = 2017; val ind = Calibration.index_year(start_year)
    val res = new GrowthModelResults(delta, qf); val res_re = new GrowthModelResults(delta, qf); val res_nre = new GrowthModelResults(delta, qf);
    res.updateProduction(start_year, calib.energyUnits(calib.Ye), calib.energyUnits(calib.E), calib.energyUnits(calib.Ee), calib.Ke * calib.pibUnits)
    res_nre.updateProduction(start_year, calib.energyUnits(calib.Ye) - MegaTonOilEquivalent(45), Calibration.data.e(ind) - MegaTonOilEquivalent(45), calib.energyUnits(calib.Ee), calib.Ke * calib.pibUnits)
    res_re.updateProduction(start_year, MegaTonOilEquivalent(45), MegaTonOilEquivalent(45), Joules(0), 0)

    // Iterate on each technology to produce at optimal eroi
    val techs_it = techs.map(tech => new TechnologyIterator(tech._1, all_sites, delta, qf))
    techs_it.map(t => t.simulate_year(2050, (target * share_re) * techs.find(_._1.equals(t.tech)).get._2))

    // End values
    res_re.sumResults(2050, techs_it.map(_.results))
    val tilde_ke = techs_it.map(t => t.tilde_ke.last.to(calib.energyUnits)).sum
    val u = res_re.u.last.to(calib.energyUnits)
    val a = res_re.a.last.to(calib.energyUnits)

    val qe_0 = Calibration.data.qe(ind); val ve_0 = calib.ve; val ke_0 = calib.Ke * calib.pibUnits
    val e_nre = target * (1.0 - share_re); val u_nre = e_nre / (1 - qe_0)
    res_nre.updateProduction(2050, u_nre, e_nre, u_nre * qe_0, u_nre.to(calib.energyUnits) * ve_0)

    res.sumResults(start_year, List(res_re, res_nre))

    // println(delta + "\t" + share_re + "\t" + u + "\t" + a + "\t" + tilde_ke + "\t" + u / (a + tilde_ke * delta) + "\t" + res_re.eroi.last + "\t" + res_nre.eroi.last + "\t" + res.eroi.last)
    // g = s*PIB/K - delta

    List((res.eroi(0), res.qe(0), res.ve(0)), (res.eroi(1), res.qe(1), res.ve(1)))
  }

  // For a given target (= final energy demand), and renewable energy share, gives a estimate of the technical parameters and EROI of the energy system
  def simulate(calib: Calibration2017, target: Energy, T: Int, techs: List[(RenewableTechnology, Double)], share_re: Double = 1.0) = {

    val delta = calib.delta

    val all_sites = Grid().cells

    // Initialise 
    val start_year = 2017; val ind = Calibration.index_year(start_year)
    val res = new GrowthModelResults(delta, calib.qf); val res_re = new GrowthModelResults(delta, calib.qf); val res_nre = new GrowthModelResults(delta, calib.qf);
    res.updateProduction(start_year, calib.energyUnits(calib.Ye), calib.energyUnits(calib.E), calib.energyUnits(calib.Ee), calib.Ke)
    res_nre.updateProduction(start_year, calib.energyUnits(calib.Ye) - MegaTonOilEquivalent(45), calib.energyUnits(calib.E) - MegaTonOilEquivalent(45), calib.energyUnits(calib.Ee), calib.Ke)
    res_re.updateProduction(start_year, MegaTonOilEquivalent(45), MegaTonOilEquivalent(45), Joules(0), 0)

    // Iterate on each technology to produce at optimal eroi
    val techs_it = techs.map(tech => new TechnologyIterator(tech._1, all_sites, delta, calib.qf))
    techs_it.map(t => t.simulate_year(2050, (target * share_re) * techs.find(_._1.equals(t.tech)).get._2))

    // End values
    res_re.sumResults(2050, techs_it.map(_.results))
    val tilde_ke = techs_it.map(t => t.tilde_ke.last.to(MegaTonOilEquivalent)).sum
    val u = res_re.u.last.to(MegaTonOilEquivalent)
    val a = res_re.a.last.to(MegaTonOilEquivalent)

    val qe_0 = Calibration.data.qe(ind); val ve_0 = calib.ve; val ke_0 = calib.Ke
    val e_nre = target * (1.0 - share_re); val u_nre = e_nre / (1 - qe_0)
    res_nre.updateProduction(2050, u_nre, e_nre, u_nre * qe_0, u_nre.to(KilowattHours) * ve_0)

    res.sumResults(start_year, List(res_re, res_nre))

    (res.eroi(1), res.qe(1), res.ve(1))
  }

  def simulateTransition(s: Double, delta: Double, alpha: Double, start_year: Int, end_year: Int) {

    val all_sites = Grid().cells
    val techs = List(OnshoreWindTechnology, OffshoreWindTechnology, PVMono) //, CSPTowerStorage12h)
    val delta = 1 / 20.0

    // (eroi, qy, vy, ve, v)
    val calib = new calibration_results_work(2017, delta, 0.05, 0.05, 0.1 / 100)
    val qy = calib.qf
    val techs_it = techs.map(tech => new TechnologyIterator(tech, all_sites, delta, qy))
    val ind = Calibration.index_year(start_year)

    val res = new GrowthModelResults(delta, qy); val res_re = new GrowthModelResults(delta, qy); val res_nre = new GrowthModelResults(delta, qy);
    res.updateProduction(start_year, Calibration.data.ye(ind), Calibration.data.e(ind), Calibration.data.ee(ind), calib.Ke)
    res_nre.updateProduction(start_year, Calibration.data.ye(ind) - MegaTonOilEquivalent(45), Calibration.data.e(ind) - MegaTonOilEquivalent(45), Calibration.data.ee(ind), calib.Ke)
    res_re.updateProduction(start_year, MegaTonOilEquivalent(45), MegaTonOilEquivalent(45), Joules(0), 0)

    // We want to reach 100% renewables by end_year: 
    val n_year = end_year - start_year;
    val qe_0 = Calibration.data.qe(ind); val ve_0 = calib.ve; val ke_0 = calib.Ke
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

  class TechnologyIterator(val tech: RenewableTechnology, sites: List[Cell], delta: Double, qy: Double, log: Boolean = false) {
    // Initialize with current installed capacity and energy produced
    val (cap0, e0) = ProductionFunction.initialValues(tech)
    val ke0 = tech.ee.energyInputsInstallation(cap0).toKilowattHours / qy;
    val oe0 = tech.ee.energyInputsOMYearly(cap0);
    val results = new GrowthModelResults(delta, qy)
    results.updateProduction(2017, e0 + oe0, e0, oe0, ke0)

    assert(math.abs(results.eroi.last - (e0 + oe0) / (oe0 + delta * tech.ee.energyInputsInstallation(cap0))) < 0.1)

    val tilde_ke = scala.collection.mutable.ArrayBuffer.empty[Energy]; tilde_ke += tech.ee.energyInputsInstallation(cap0)
    val tilde_ie = scala.collection.mutable.ArrayBuffer.empty[Energy]; tilde_ie += tilde_ke.last * delta
    val installed_cap = scala.collection.mutable.ArrayBuffer.empty[Power]; installed_cap += cap0

    val sites_sorted = sites.filter(s => tech.suitabilityFactor(s) > 0).sortBy(tech.eroi(_, 1.0)).reverse.toIterator
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