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
    simulateTransition(2017, 2050)

    /*    
    val e_units = KilowattHours; val pib_units = 1; val cal = new calibration_results_work(energy_units = e_units, pib_units = pib_units);
    val qf_f = cal.qf // * math.pow(1 - 2.0 / 100, 33);
    val pib_f = cal.pib // * math.pow(1 + 2.0 / 100, 33);
    val target = cal.data.e(cal.i) // e_units(pib_f * qf_f) + cal.data.ce(cal.i)
   
    val share = (0 to 10).map(_ * 0.1).toList
    val techs = List((OnshoreWindTechnology, 1.0 / 4), (OffshoreWindTechnology, 1.0 / 4), (PVPoly, 1.0 / 2))

    // println("RE Share [%]" + "\t" + "EROI" + "\t" + " qe [%] " + "\t" + " ve " + "\t" + " gi " + "\t" + "gs" + "\t" + "Ye" + "\t" + "E" + "\t" + "Ee")
    val res = share.map(s => {
      val r = (s, calculate(target, techs, s, cal, qf_f, pib_f, cal.vf * pib_f))
      val intg = cal.interval_gk(cal.s, cal.n, r._2(1)._3, cal.vf, r._2(1)._2, 0.3, cal.le, cal.lf)

      /*  println(r._1 * 100 + " & " + round(r._2(1)._1) + " & " + round(r._2(1)._2 * 100) + " & " + round(r._2(1)._3) + "& " +
        round(100 * intg._1) + " & " + round(100 * intg._2) +
        " & " + r._2(2)._1 + " & " + r._2(2)._2 + " & " + r._2(2)._3 + "\\" + "\\")*/
      r
    })
    plotXY(List((share, res.map(_._2(1)._1), "")), xLabel = "RE Share", yLabel = "EROI")
    plotXY(List((share, res.map(_._2(1)._2 * 100), "")), xLabel = "RE Share", yLabel = "qe [%]")
    plotXY(List((share, res.map(_._2(1)._3), "")), xLabel = "RE Share", yLabel = "ve")
*/
    // combinedPlots(share.map(_ * 100), List((res.map(_._2(1)._1), "EROI"), (res.map(_._2(1)._2 * 100), "qe [%]"), (res.map(_._2(1)._3), "ve")), xLabel = "RE Share [%]")
  }

  // For a given target (= final energy demand E), and renewable energy share, gives a estimate of the technical parameters and EROI of the energy system
  def calculate(target: Energy, techs: List[(RenewableTechnology, Double)], share_re: Double = 1.0,
    calib: calibration_results_work, qf_final: Double, pib_final: Double, Kf_final: Double) = {

    val all_sites = Grid().cells; val delta = calib.delta; val qf_0 = calib.qf;

    // Initialise 
    val start_year = 2017; val ind = Calibration.index_year(start_year)
    val res = new GrowthModelResults(delta); val res_re = new GrowthModelResults(delta); val res_nre = new GrowthModelResults(delta);
    res.updateProduction(start_year, calib.data.ye(calib.i), calib.data.ee(calib.i), calib.Ke, qf_0, calib.energy_units)
    res_nre.updateProduction(start_year, calib.data.ye(calib.i), calib.data.ee(calib.i), calib.Ke, qf_0, calib.energy_units)
    res_re.updateProduction(start_year, Joules(0), Joules(0), 0, qf_0, calib.energy_units)
    // res_nre.updateProduction(start_year, calib.data.ye(calib.i) - MegaTonOilEquivalent(45), calib.data.e(calib.i) - MegaTonOilEquivalent(45), calib.data.ee(calib.i), calib.Ke, qf_0, calib.energy_units)
    // res_re.updateProduction(start_year, MegaTonOilEquivalent(45), MegaTonOilEquivalent(45), Joules(0), 0, qf_0, calib.energy_units)

    // Iterate on each technology to produce at optimal eroi
    val techs_it = techs.map(tech => new TechnologyIterator(tech._1, all_sites, delta, qf_0, qf_final, energy_units = calib.energy_units, pib_units = calib.pib_units))
    techs_it.map(t => t.simulate_static((target * share_re) * techs.find(_._1.equals(t.tech)).get._2, true))

    // End values
    res_re.sumResults(2050, techs_it.map(_.results), qf_final, calib.energy_units)

    val qe_0 = Calibration.data.qe(ind); val ve_0 = calib.ve; // val ke_0 = calib.Ke
    val e_nre = target - res_re.e.last; val ye_nre = e_nre / (1 - qe_0)

    res_nre.updateProduction(2050, ye_nre, ye_nre * qe_0, ye_nre.to(calib.energy_units) * ve_0, qf_final, calib.energy_units)

    res.sumResults(start_year, List(res_re, res_nre), qf_final, calib.energy_units)
    import Helper._
    // println(delta + "\t" + share_re + "\t" + u + "\t" + a + "\t" + tilde_ke + "\t" + u / (a + tilde_ke * delta) + "\t" + res_re.eroi.last + "\t" + res_nre.eroi.last + "\t" + res.eroi.last)
    // g = s*PIB/K - delta
    //println(share_re * 100 + "\t" + res.ye.last.to(calib.energy_units) + "\t" + res.e.last.to(calib.energy_units) + "\t" + res.ee.last.to(calib.energy_units) + "\t" + res.ke.last + "\t" + res_re.ke.last + "\t" + res_nre.ke.last + "\t" + Kf_final + "\t" + pib_final + "\t" + qf_final)
    val (gi, gs) = calib.interval_gk(calib.s, calib.n, res.ve.last, calib.vf, res.qe.last, calib.qf, calib.le, calib.lf)
    val (mi, ms) = calib.interval_m(calib.s, calib.n, res.ve.last, calib.vf, res.qe.last, calib.qf, calib.le, calib.lf)
    val (di, ds) = calib.interval_delta_m(mi, ms)

    println(round(res.ye.last.to(MegaTonOilEquivalent), 0) + " &" + round(share_re * 100, 0) + " & " + round(res.eroi.last) + " & " + round(100 * res.qe.last) + " & " + round(res.ve.last) + "& " +
      round(gi * 100) + " & " + round(gs * 100) + " & " + round(mi * 100) + " & " + round(ms * 100) + " & " + round(100 * di) + " & " + round(100 * ds) + "\\" + "\\")

    List((res.eroi(0), res.qe(0), res.ve(0)), (res.eroi(1), res.qe(1), res.ve(1)), (res.ye.last.to(calib.energy_units), res.e.last.to(calib.energy_units), res.ee.last.to(calib.energy_units)))
  }

  def simulateTransition(start_year: Int, end_year: Int) {

    val all_sites = Grid().cells
    val techs = List((OnshoreWindTechnology, 0.25), (OffshoreWindTechnology, 0.25), (PVMono, 0.5)) //, CSPTowerStorage12h)

    val calib = new calibration_results_work
    val techs_it = techs.map(tech => new TechnologyIterator(tech._1, all_sites, calib.delta, calib.qf, calib.qf, energy_units = calib.energy_units, pib_units = calib.pib_units))
    val ind = Calibration.index_year(start_year)

    val res = new GrowthModelResults(calib.delta); val res_re = new GrowthModelResults(calib.delta); val res_nre = new GrowthModelResults(calib.delta);
    res.updateProduction(start_year, calib.data.ye(calib.i), calib.data.ee(calib.i), calib.Ke, calib.qf, calib.energy_units)
    res_nre.updateProduction(start_year, calib.data.ye(calib.i) - MegaTonOilEquivalent(45), calib.data.ee(calib.i), calib.Ke, calib.qf, calib.energy_units)
    res_re.updateProduction(start_year, MegaTonOilEquivalent(45), Joules(0), 0, calib.qf, calib.energy_units)

    // We want to reach 100% renewables by end_year: 
    val n_year = end_year - start_year;
    val qe_0 = Calibration.data.qe(ind); val ve_0 = calib.ve; val ke_0 = calib.Ke
    val e_re0 = res_re.e.last; val e_0 = Calibration.data.e(ind)
    // If we consider an exponential growth (i.e. a constant growth rate): 
    // Ere,n = E,2017 & Ere,n = Ere,2017 * g^n => g = (E,2017/Ere,2017)^1/n
    val growth_rate = math.pow(e_0 / e_re0, 1.0 / n_year)
    println("Simulation " + n_year + ", exponential growth rate " + growth_rate)

    for (y <- 1 to n_year + 50) {
      // The desired total production from RE sources by the end of the year.
      val target_re = if (res.year.last >= end_year) Joules(0) else List(e_re0 * math.pow(growth_rate, y) - res_re.e.last, Joules(0)).maxBy(_.value)
      println("Simulate year " + res.year.last + " with target " + target_re.to(MegaTonOilEquivalent))
      techs_it.map(it => it.simulate_year(y, target_re / techs.size, true))
      // Update params from RE sector
      res_re.sumResults(start_year + y, techs_it.map(_.results), calib.qf, calib.energy_units)
      // Remove from the total the energy that is now produced by the re sector
      // e_nre < 0 is not supposed to happen before the last year of the simulation !!
      val e_nre = List(e_0 - res_re.e.last, Joules(0)).maxBy(_.value)
      val u_nre = e_nre / (1 - qe_0)
      res_nre.updateProduction(start_year + y, u_nre, u_nre * qe_0, u_nre.to(KilowattHours) * ve_0, calib.qf, calib.energy_units)
      res.sumResults(start_year + y, List(res_re, res_nre), calib.qf, calib.energy_units)
    }

    val year_double = res.year.toList.map(_.toDouble)
    val ys = List((res.ve.toList, "ve"), (res.qe.toList, "qe"), (res.eroi.toList, "EROI"))
    combinedPlots(year_double, ys)
    plotXY(List((year_double, res.ee.toList.map(_.to(Exajoules)), "Ee"), (year_double, res_re.ee.toList.map(_.to(Exajoules)), "E_eRE"), (year_double, res_nre.ee.toList.map(_.to(Exajoules)), "E_eNRE")), legend = true)
    plotXY(List((year_double, res.e.toList.map(_.to(Exajoules)), "E"), (year_double, res_re.e.toList.map(_.to(Exajoules)), "E_RE"), (year_double, res_nre.e.toList.map(_.to(Exajoules)), "E_NRE")), legend = true)
    plotXY(List((year_double, res.ke.toList, "Ke"), (year_double, res_re.ke.toList, "Ke_RE"), (year_double, res_nre.ke.toList, "Ke_NRE")), legend = true)
    plotXY(List((year_double, res.qe.toList, "qe"), (year_double, res_re.qe.toList, "qe_RE"), (year_double, res_nre.qe.toList, "qe_NRE")), legend = true)
    plotXY(List((year_double, res.ve.toList, "ve"), (year_double, res_re.ve.toList, "ve_RE"), (year_double, res_nre.ve.toList, "ve_NRE")), legend = true)
    plotXY(List((year_double, res.eroi.toList, "eroi"), (year_double, res_re.eroi.toList, "eroi_RE"), (year_double, res_nre.eroi.toList, "eroi_NRE")), legend = true)

  }

}

class TechnologyIterator(val tech: RenewableTechnology, sites: List[Cell], delta: Double, qf_0: Double, qf_final: Double, log: Boolean = false, energy_units: EnergyUnit = KilowattHours,
    pib_units: Int = 1) {
  // Initialize with current installed capacity and energy produced
  val (cap0, e0) = (Watts(0), Joules(0)) //ProductionFunction.initialValues(tech)
  val ke0 = tech.ee.energyInputsInstallation(cap0).to(energy_units) / qf_0;
  val oe0 = tech.ee.energyInputsOMYearly(cap0);
  val results = new GrowthModelResults(delta)
  results.updateProduction(2017, e0, oe0, ke0, qf_0, energy_units)

  //assert(math.abs(results.eroi.last - (e0 + oe0) / (oe0 + delta * tech.ee.energyInputsInstallation(cap0))) < 0.1)

  val tilde_ke = scala.collection.mutable.ArrayBuffer.empty[Energy]; tilde_ke += tech.ee.energyInputsInstallation(cap0)
  val tilde_ie = scala.collection.mutable.ArrayBuffer.empty[Energy]; tilde_ie += tilde_ke.last * delta
  val installed_cap = scala.collection.mutable.ArrayBuffer.empty[Power]; installed_cap += cap0

  val sites_sorted = sites.filter(s => tech.suitabilityFactor(s) > 0).sortBy(tech.eroi(_, 1.0)).reverse.toIterator

  var installations: List[RenewableInstallation] = List()
  def simulate_year(y: Int, target: Energy, target_final: Boolean) {
    // year += y
    var ended = false
    // New direct ke is a "one-shot" investment
    var newCap = Watts(0); var newProd = Joules(0); var newOperationE = Joules(0);
    var newIndirectInstallationE = Joules(0); var newDirectInstallationE = Joules(0); var newDirectDecommissioningE = Joules(0)

    def progress = { if (target_final) (newProd - newOperationE) else newProd }
    if (target.value != 0) {
      while (progress <= target && !ended) {
        if (sites_sorted.hasNext) {
          val next_site = sites_sorted.next()
          val prod = tech.potential(next_site, 1.0) * Hours(365 * 24)
          newProd += prod
          newOperationE += tech.energyInputsOMYearly(next_site, 1.0)
          newDirectInstallationE += tech.directEnergyInputsInstallation(next_site, 1.0)
          newIndirectInstallationE += tech.indirectEnergyInputsInstallation(next_site, 1.0)
          newDirectDecommissioningE += tech.energyInputsDecomissioning(next_site, 1.0)
          newCap += tech.ratedPower(next_site, 1.0)
        } else {
          ended = true
        }
      }
    }
    installations = installations :+ new RenewableInstallation(tech, newCap, newProd, newOperationE, newDirectInstallationE, newIndirectInstallationE, newDirectDecommissioningE)
    // Update installation and see if one need replacement !
    installations.map(_.updateYear)
    // Update parameters !
    installed_cap += installations.map(_.rated_power).foldLeft(Watts(0))(_ + _)
    // Investment = new capacity + depreciation compensation of installed capacity
    // Energy sector capital stock: Capital stock(t-1) + new capital
    val e_installation = sum(installations.filter(_.age == 0).map(_.directInstallationE))
    val ke_installation = sum(installations.map(_.indirectInstallationE))
    val e_decom = sum(installations.filter(t => t.age == t.tech.lifeTime).map(_.directDecommissioningE))

    results.updateProduction(y, sum(installations.map(_.production)), sum(installations.map(_.operationE)) + e_installation + e_decom,
      ke_installation.to(energy_units) / qf_final, qf_final, energy_units)
  }
  def sum(e: List[Energy]) = e.foldLeft(Joules(0))(_ + _)
  def simulate_static(target: Energy, target_final: Boolean) {
    // year += y
    var ended = false
    var newCap = Watts(0); var newProd = Joules(0); var newKe = Joules(0); var newEE = Joules(0);

    def progress = { if (target_final) (newProd - newEE) else newProd }
    if (target.value != 0) {
      while (progress <= target && !ended) {
        if (sites_sorted.hasNext) {
          val next_site = sites_sorted.next()
          val prod = tech.potential(next_site, 1.0) * Hours(365 * 24)
          newProd += prod
          newEE += tech.energyInputsOMYearly(next_site, 1.0) + (tech.directEnergyInputsInstallation(next_site, 1.0) + tech.energyInputsDecomissioning(next_site, 1.0)) / tech.lifeTime // + prod * tech.operation_variable 
          newKe += tech.indirectEnergyInputsInstallation(next_site, 1.0)
          newCap += tech.ratedPower(next_site, 1.0)
        } else {
          ended = true
        }
      }
    }

    println(tech.name + "\t" + " CF : " + (100 * newProd / (newCap * Hours(365 * 24))) + " EROI : " + (newProd / (newEE + newKe / tech.lifeTime)))
    if (log) println(tech.name + ",static , new production:" + newProd.to(MegaTonOilEquivalent) + "(Target was : " + target.to(MegaTonOilEquivalent))

    // Update parameters !
    installed_cap += (installed_cap.last + newCap)
    // Investment = new capacity + depreciation compensation of installed capacity
    tilde_ie += newKe + tilde_ke.last / tech.lifeTime
    // Energy sector capital stock: Capital stock(t-1) + new capital
    tilde_ke += tilde_ke.last + newKe
    results.updateProduction(2050, results.ye.last + newProd, results.ee.last + newEE, tilde_ke.last.to(energy_units) / qf_final, qf_final, energy_units)
  }
}

class RenewableInstallation(val tech: RenewableTechnology, val rated_power: Power, val production: Energy,
    val operationE: Energy, val directInstallationE: Energy, val indirectInstallationE: Energy, val directDecommissioningE: Energy) {
  var age = -1
  def updateYear {
    age += 1
    if (age == tech.lifeTime + 1) age = 0
  }

}

class GrowthModelResults(delta: Double) {
  val year = scala.collection.mutable.ArrayBuffer.empty[Int];
  val ye = scala.collection.mutable.ArrayBuffer.empty[Energy];
  val ee = scala.collection.mutable.ArrayBuffer.empty[Energy];
  val e = scala.collection.mutable.ArrayBuffer.empty[Energy];
  val ke = scala.collection.mutable.ArrayBuffer.empty[Double];
  val ve = scala.collection.mutable.ArrayBuffer.empty[Double];
  val qe = scala.collection.mutable.ArrayBuffer.empty[Double];
  val eroi = scala.collection.mutable.ArrayBuffer.empty[Double];

  def updateProduction(y: Int, newYe: Energy, newEe: Energy, newKe: Double, qf: Double, energy_units: EnergyUnit) {
    year += y
    ye += newYe
    e += (newYe - newEe)
    ee += newEe
    ke += newKe
    updateParams(qf, energy_units)
  }
  def updateParams(qf: Double, energy_units: EnergyUnit) {
    ve += ke.last / ye.last.to(energy_units)
    qe += ee.last / ye.last
    eroi += 1 / (qe.last + delta * qf * ve.last)
  }
  def sumResults(y: Int, res: List[GrowthModelResults], qf: Double, energy_units: EnergyUnit) {
    year += y
    ye += res.map(_.ye.last).foldLeft(Joules(0))(_ + _)
    e += res.map(_.e.last).foldLeft(Joules(0))(_ + _)
    ee += res.map(_.ee.last).foldLeft(Joules(0))(_ + _)
    ke += res.map(_.ke.last).sum
    updateParams(qf, energy_units)
  }
}

