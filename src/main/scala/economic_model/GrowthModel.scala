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

    val e_units = MegaTonOilEquivalent; val pib_units = 1E9.toInt;
    val cal = new calibration_results_work(energy_units = e_units, pib_units = pib_units, pop_units = 1E6.toInt);
    val qf_f = cal.qf * math.pow(1 - 1.06 / 100, 33);
    val pib_f = cal.pib // * math.pow(1 + 2.0 / 100, 33);
    val target = cal.data.ye(cal.i) // * (1 - cal.qe - cal.delta_e * cal.qf * cal.ve)
    
    val share = (0 to 100).map(_/100.0).toList
    
    // val share = (1 to 5).map(_ * 0.2).toList
    // Jacobson scenario for 2050: 
    // 14.89% rooftop residential + 11.58 % commercial
    // 21.36 % pv power plants  
    // 9.72% CSP
    // 23.52% Onshore wind
    // 13.62% Offshore wind
    // Total = 94.69 / 100 
    val techs = List((OnshoreWindTechnology, 23.52 / 94.69), (OffshoreWindTechnology, 13.62 / 94.69), (PVPoly, (14.89 + 11.58 + 21.36) / 94.69), (CSPParabolic, 9.72 / 94.69))
    //println("Res fixed Ye")
    //val res_ye = share.map(s => (s, calculate(target, false, techs, s, cal, qf_f, cal.lf)))
    //println("Res fixed Net E")
    //val res_nete = share.map(s => (s, calculate(target, true, techs, s, cal, qf_f, cal.lf)))
    simulateTransition(share, techs)
    
    // share.map(s => (s, calculate(target, false, techs, s, cal, qf_f, cal.lf)))
    
  }

  // For a given target (= final net energy demand NE), and renewable energy share, 
  // gives a estimate of the technical parameters and EROI of the energy system
  def calculate(target: Energy, net: Boolean, techs: List[(RenewableTechnology, Double)], share_re: Double = 1.0,
    calib: calibration_results_work, qf: Double, lf: Double): Z = {

    val all_sites = Grid().cells;
    // Initialise 
    val start_year = 2017; val ind = Calibration.index_year(start_year)
    val (res, res_re, res_nre) = (new GrowthModelResults(calib.energy_units), new GrowthModelResults(calib.energy_units), new GrowthModelResults(calib.energy_units))
    res.updateProduction(start_year, calib.data.ye(calib.i), calib.data.ee(calib.i), calib.tilde_Ke(qf), calib.delta_e)
    res_nre.updateProduction(start_year, calib.data.ye(calib.i), calib.data.ee(calib.i), calib.tilde_Ke(qf), calib.delta_e)
    res_re.updateProduction(start_year, Joules(0), Joules(0), Joules(0), calib.delta_e)
    // res_nre.updateProduction(start_year, calib.data.ye(calib.i) - MegaTonOilEquivalent(45), calib.data.e(calib.i) - MegaTonOilEquivalent(45), calib.data.ee(calib.i), calib.Ke, qf_0, calib.energy_units)
    // res_re.updateProduction(start_year, MegaTonOilEquivalent(45), MegaTonOilEquivalent(45), Joules(0), 0, qf_0, calib.energy_units)

    // Iterate on each technology to produce at optimal eroi
    val techs_it = techs.map(tech => new TechnologyIterator(tech._1, all_sites, energy_units = calib.energy_units, pib_units = calib.pib_units))
    techs_it.map(t => t.simulate_static((target * share_re) * techs.find(_._1.equals(t.tech)).get._2, net))
    // println("End simulation " + share_re)
    // End values
    res_re.sumResults(2050, techs_it.map(_.results))

    val qe_nre = Calibration.data.qe(ind); val ve_nre = calib.ve; val delta_e_nre = calib.delta_e
    // Net E = Ye * (1 - qe - delta_e q_f v_e)
    val net_e_re = if (share_re == 0) MegaTonOilEquivalent(0) else res_re.netE
    val ye_nre = if (net) (target - net_e_re) / (1.0 - qe_nre - ve_nre * delta_e_nre * qf) else target - res_re.ye.last
    res_nre.updateProduction(2050, ye_nre, ye_nre * qe_nre, calib.energy_units(ye_nre.to(calib.energy_units) * ve_nre * qf), delta_e_nre)

    res.sumResults(start_year, List(res_re, res_nre))
    import Helper._
    
    println(share_re + "\t" + res_re.qe.last + "\t" + res_re.tilde_ke.last.to(MegaTonOilEquivalent) + "\t" + res_re.delta_e.last + "\t" + res_re.ye.last.to(MegaTonOilEquivalent)+ "\t" + res_re.ee.last.to(MegaTonOilEquivalent))
   
    // println(delta + "\t" + share_re + "\t" + u + "\t" + a + "\t" + tilde_ke + "\t" + u / (a + tilde_ke * delta) + "\t" + res_re.eroi.last + "\t" + res_nre.eroi.last + "\t" + res.eroi.last)
    // g = s*PIB/K - delta
    //println(share_re * 100 + "\t" + res.ye.last.to(calib.energy_units) + "\t" + res.e.last.to(calib.energy_units) + "\t" + res.ee.last.to(calib.energy_units) + "\t" + res.ke.last + "\t" + res_re.ke.last + "\t" + res_nre.ke.last + "\t" + Kf_final + "\t" + pib_final + "\t" + qf_final)
    Z(res.ve(qf), calib.vf, res.qe.last, qf, calib.le, lf, res.delta_e.last, calib.delta_f)

  }

  def simulateTransition(shares: List[Double],techs: List[(RenewableTechnology, Double)]) {

    val all_sites = Grid().cells
    // val techs = List((OnshoreWindTechnology, 0.25), (OffshoreWindTechnology, 0.25), (PVMono, 0.5)) //, CSPTowerStorage12h)

    val calib = new calibration_results_work
    val techs_it = techs.map(tech => (new TechnologyIterator(tech._1, all_sites, energy_units = calib.energy_units, pib_units = calib.pib_units),tech._2))
    val ind = Calibration.index_year(2017)

    val (res, res_re, res_nre) = (new GrowthModelResults(calib.energy_units), new GrowthModelResults(calib.energy_units), new GrowthModelResults(calib.energy_units))
    res.updateProduction(2017, calib.data.ye(calib.i), calib.data.ee(calib.i), calib.tilde_Ke(calib.qf), calib.delta_e)
    res_nre.updateProduction(2017, calib.data.ye(calib.i), calib.data.ee(calib.i), calib.tilde_Ke(calib.qf), calib.delta_e)
    res_re.updateProduction(2017, Joules(0), Joules(0), Joules(0), calib.delta_e)

    // We want to reach 100% renewables by end_year: 
    val n_year = 2017 + shares.size;
    val qe_nre = Calibration.data.qe(ind); val ve_nre = calib.ve; val delta_e_nre = calib.delta_e; val qf = calib.qf

    // If we consider an exponential growth (i.e. a constant growth rate): 
    // Ere,n = E,2017 & Ere,n = Ere,2017 * g^n => g = (E,2017/Ere,2017)^1/n
    val target = calib.data.ye(ind)
    var i = 0
    for (s <- shares) {
      i += 1
      // The desired total production from RE sources by the end of the year.
      val target_re =  (s * target) - res_re.ye.last
      // println("Simulate year " + res.year.last + " with target " + target_re.to(MegaTonOilEquivalent))
      techs_it.map(it => it._1.simulate_year(2017 + i, target_re*it._2, true))
      // Update params from RE sector
      res_re.sumResults(2017 + i, techs_it.map(_._1.results))
      // Remove from the total the energy that is now produced by the re sector
      // e_nre < 0 is not supposed to happen before the last year of the simulation !!
      val ye_nre = (1 - s) * target
      res_nre.updateProduction(2017 + i, ye_nre, ye_nre * qe_nre, calib.energy_units(ye_nre.to(calib.energy_units) * ve_nre * qf), delta_e_nre)
      res.sumResults(2017 + i, List(res_re, res_nre))
      
      println(s + "\t" + res_re.qe.last + "\t" + res_re.tilde_ke.last.to(MegaTonOilEquivalent) + "\t" + res_re.delta_e.last + "\t" + res_re.ye.last.to(MegaTonOilEquivalent)+ "\t" + res_re.ee.last.to(MegaTonOilEquivalent))
    }

    val year_double = res.year.toList.map(_.toDouble)
    // val ys = List((res.ve(calib.qf).toList, "ve"), (res.qe.toList, "qe"), (res.eroi(calib.qf).toList, "EROI"))
    // combinedPlots(year_double, ys)
    plotXY(List((year_double, res.ee.toList.map(_.to(Exajoules)), "Ee"), (year_double, res_re.ee.toList.map(_.to(Exajoules)), "E_eRE"), (year_double, res_nre.ee.toList.map(_.to(Exajoules)), "E_eNRE")), legend = true)
    // plotXY(List((year_double, res.e.toList.map(_.to(Exajoules)), "E"), (year_double, res_re.e.toList.map(_.to(Exajoules)), "E_RE"), (year_double, res_nre.e.toList.map(_.to(Exajoules)), "E_NRE")), legend = true)
    // plotXY(List((year_double, res.ke.toList, "Ke"), (year_double, res_re.ke.toList, "Ke_RE"), (year_double, res_nre.ke.toList, "Ke_NRE")), legend = true)
    plotXY(List((year_double, res.qe.toList, "qe"), (year_double, res_re.qe.toList, "qe_RE"), (year_double, res_nre.qe.toList, "qe_NRE")), legend = true)
    // plotXY(List((year_double, res.ve.toList, "ve"), (year_double, res_re.ve.toList, "ve_RE"), (year_double, res_nre.ve.toList, "ve_NRE")), legend = true)
    // plotXY(List((year_double, res.eroi.toList, "eroi"), (year_double, res_re.eroi.toList, "eroi_RE"), (year_double, res_nre.eroi.toList, "eroi_NRE")), legend = true)
    
  }

}

class TechnologyIterator(val tech: RenewableTechnology, sites: List[Cell], log: Boolean = false, energy_units: EnergyUnit = KilowattHours,
    pib_units: Int = 1) {
  // Initialize with current installed capacity and energy produced
  val (cap0, e0) = (Watts(0), Joules(0)) //ProductionFunction.initialValues(tech)
  val ke0 = tech.ee.energyInputsInstallation(cap0) // .to(energy_units) / qf_0;
  val oe0 = tech.ee.energyInputsOMYearly(cap0);
  val results = new GrowthModelResults(energy_units)
  val delta_e = 1.0 / tech.lifeTime
  results.updateProduction(2017, e0, oe0, ke0, delta_e)

  //assert(math.abs(results.eroi.last - (e0 + oe0) / (oe0 + delta * tech.ee.energyInputsInstallation(cap0))) < 0.1)

  val tilde_ke = scala.collection.mutable.ArrayBuffer.empty[Energy]; tilde_ke += tech.ee.energyInputsInstallation(cap0)
  val tilde_ie = scala.collection.mutable.ArrayBuffer.empty[Energy]; tilde_ie += tilde_ke.last * delta_e
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
    
    results.updateProduction(y, sum(installations.map(_.production)),
      sum(installations.map(_.operationE)),
      sum(installations.map(_.tildeKe)),
      //ke_installation + e_installation + e_decom, 
      delta_e)
  }

  def sum(e: List[Energy]) = e.foldLeft(Joules(0))(_ + _)
  // Net Energy Target !
  def simulate_static(target: Energy, net: Boolean) {
    // year += y
    var ended = false; var currentTarget = Joules(0);
    var newCap = Watts(0); var newProd = Joules(0); var newKe = Joules(0); var newEE = Joules(0);

    if (target.value != 0) {
      while (newProd <= currentTarget && !ended) {
        if (sites_sorted.hasNext) {
          val next_site = sites_sorted.next()
          newProd += tech.potential(next_site, 1.0) * Hours(365 * 24)
          newCap += tech.ratedPower(next_site, 1.0)
          newEE += tech.energyInputsOMYearly(next_site, 1.0) // Is that really in qe ?
          newKe += (tech.energyInputsInstallation(next_site, 1.0) + tech.energyInputsDecomissioning(next_site, 1.0)) // Total energy embodied in energy sector capital stock; to divise by life time 
        } else {
          ended = true
        }
        currentTarget = if (net) (target + newEE + newKe / tech.lifeTime) else target
      }
    }
    if (log) println(target.to(MegaTonOilEquivalent) + "\t" + tech.name + ",static , new production:" + newProd.to(MegaTonOilEquivalent) + "(Target was : " + target.to(MegaTonOilEquivalent))

    // Update parameters !
    installed_cap += (installed_cap.last + newCap)
    // Investment = new capacity + depreciation compensation of installed capacity
    tilde_ie += newKe + tilde_ke.last / tech.lifeTime
    // Energy sector capital stock: Capital stock(t-1) + new capital
    tilde_ke += tilde_ke.last + newKe
    results.updateProduction(2050, results.ye.last + newProd, results.ee.last + newEE, tilde_ke.last, delta_e)
  }
}

class RenewableInstallation(val tech: RenewableTechnology, val rated_power: Power, val production: Energy,
    val operationE: Energy, val directInstallationE: Energy, val indirectInstallationE: Energy, val directDecommissioningE: Energy) {
  var age = -1
  def updateYear {
    age += 1
    if (age == tech.lifeTime + 1) age = 0
  }
val tildeKe = directInstallationE+indirectInstallationE+directDecommissioningE
}

class GrowthModelResults(energy_units: EnergyUnit) {
  val year = scala.collection.mutable.ArrayBuffer.empty[Int];
  val ye = scala.collection.mutable.ArrayBuffer.empty[Energy];
  val ee = scala.collection.mutable.ArrayBuffer.empty[Energy];
  val tilde_ke = scala.collection.mutable.ArrayBuffer.empty[Energy];
  // Results
  // val eroi = scala.collection.mutable.ArrayBuffer.empty[Double];
  val qe = scala.collection.mutable.ArrayBuffer.empty[Double];
  val delta_e = scala.collection.mutable.ArrayBuffer.empty[Double];

  def netE = ye.last - ee.last - tilde_ke.last * delta_e.last
  // EROI = Ye/(Ee+tilde(delta_e K_e)) = Y_e/(q_e Y_e + \delta_e v_e q_f Y_e) = 1/(q_e + \delta_e v_e q_f) ==> v_e = (1/EROI-q_e)/(\delta_e q_f)
  def eroi(qf: Double): Double = 1.0 / (qe.last + delta_e.last * ve(qf) * qf)

  def ve(qf: Double): Double = {
    val ve1 = tilde_ke.last / ye.last / qf
    // val ve2 = ((1 / eroi(qf)) - qe.last) / (delta_e.last * qf)
    // assert(ve1 == ve2)
    ve1
  }
  // def le(le_0: Double) = le_0 * (1 + (qe.last - qe(0)) / qe(0))

  def updateProduction(y: Int, newYe: Energy, newEe: Energy, newTildeKe: Energy, newDelta_e: Double) {
    year += y
    ye += newYe
    ee += newEe
    tilde_ke += newTildeKe
    delta_e += newDelta_e;
    // Calculated results
    qe += (if (newYe.value == 0) 0.0 else newEe / newYe)
    // eroi += (if (newYe.value == 0) 1.0 else newYe / (newEe + newDelta_e * newTildeKe))
  }

  def sumResults(y: Int, res: List[GrowthModelResults]) {
    val new_tke = res.map(_.tilde_ke.last).foldLeft(Joules(0))(_ + _)
    val new_delta_e = if (new_tke.value == 0) 1.0 else res.map(r => r.delta_e.last * r.tilde_ke.last).foldLeft(Joules(0))(_ + _) / new_tke
    updateProduction(y,
      res.map(_.ye.last).foldLeft(Joules(0))(_ + _),
      res.map(_.ee.last).foldLeft(Joules(0))(_ + _),
      new_tke,
      // res.map(_.qf.last).sum / res.size,
      new_delta_e)
  }
}

