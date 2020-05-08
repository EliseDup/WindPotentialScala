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

    val e_units = MegaTonOilEquivalent; val pib_units = 1E9.toInt; val cal = new calibration_results_work(energy_units = e_units, pib_units = pib_units);
    val qf_f = cal.qf // * math.pow(1 - 2.0 / 100, 33);
    val pib_f = cal.pib // * math.pow(1 + 2.0 / 100, 33);
    val target = cal.data.ye(cal.i) * (1 - cal.qe - cal.delta_e * cal.qf * cal.ve)

    println("Net Energy Target " + target.to(MegaTonOilEquivalent) + "\t" + cal.s)
    val share = (0 to 10).map(_ * 0.1).toList
    // Jacobson scenario for 2050: 
    // 14.89% rooftop residential + 11.58 % commercial
    // 21.36 % pv power plants  
    // 9.72% CSP
    // 23.52% Onshore wind
    // 13.62% Offshore wind
    // Total = 94.69 / 100 
    val techs = List((OnshoreWindTechnology, 23.52 / 94.69), (OffshoreWindTechnology, 13.62 / 94.69), (PVPoly, (14.89 + 11.58 + 21.36) / 94.69), (CSPParabolic, 9.72 / 94.69))

    //val techs = List((OnshoreWindTechnology, 0.3),(OffshoreWindTechnology, 0.1), (PVMono, 0.6))
    // println("RE Share [%]" + "\t" + "EROI" + "\t" + " qe [%] " + "\t" + " ve " + "\t" + " gi " + "\t" + "gs" + "\t" + "Ye" + "\t" + "E" + "\t" + "Ee")
    val res = share.map(s => (s, calculate(target, techs, s, cal, qf_f, pib_f, cal.vf * pib_f)))

    plotXY(List((share, res.map(_._2.ye.last.to(MegaTonOilEquivalent)), "Ye"),
      (share, res.map(_._2.netE.to(MegaTonOilEquivalent)), "Net E"),
      (share, res.map(r => r._2.tilde_ke.last.to(MegaTonOilEquivalent) * r._2.delta_e.last), "tilde(delta_e Ke)"),
      (share, res.map(_._2.ee.last.to(MegaTonOilEquivalent)), "EE")), xLabel = "RE Share", yLabel = "Mtoe", legend = true)
    plotXY(List((share, res.map(_._2.tilde_ke.last.to(e_units) / cal.qf * pib_units / 1E9), "Ke [GUS $]"), (share, share.map(i => cal.Kf * pib_units / 1E9), "Kf [GUS $]")), legend = true)
  }

  // For a given target (= final net energy demand NE), and renewable energy share, 
  // gives a estimate of the technical parameters and EROI of the energy system
  def calculate(target: Energy, techs: List[(RenewableTechnology, Double)], share_re: Double = 1.0,
    calib: calibration_results_work, qf_final: Double, pib_final: Double, Kf_final: Double) = {

    val all_sites = Grid().cells; val qf_0 = calib.qf;
    // Initialise 
    val start_year = 2017; val ind = Calibration.index_year(start_year)
    val (res, res_re, res_nre) = (new GrowthModelResults(calib.energy_units), new GrowthModelResults(calib.energy_units), new GrowthModelResults(calib.energy_units))
    res.updateProduction(start_year, calib.data.ye(calib.i), calib.data.ee(calib.i), calib.tilde_Ke, calib.delta_e)
    res_nre.updateProduction(start_year, calib.data.ye(calib.i), calib.data.ee(calib.i), calib.tilde_Ke, calib.delta_e)
    res_re.updateProduction(start_year, Joules(0), Joules(0), Joules(0), calib.delta_e)
    // res_nre.updateProduction(start_year, calib.data.ye(calib.i) - MegaTonOilEquivalent(45), calib.data.e(calib.i) - MegaTonOilEquivalent(45), calib.data.ee(calib.i), calib.Ke, qf_0, calib.energy_units)
    // res_re.updateProduction(start_year, MegaTonOilEquivalent(45), MegaTonOilEquivalent(45), Joules(0), 0, qf_0, calib.energy_units)

    // Iterate on each technology to produce at optimal eroi
    val techs_it = techs.map(tech => new TechnologyIterator(tech._1, all_sites, qf_0, qf_final, energy_units = calib.energy_units, pib_units = calib.pib_units))
    techs_it.map(t => t.simulate_static((target * share_re) * techs.find(_._1.equals(t.tech)).get._2))

    // End values
    res_re.sumResults(2050, techs_it.map(_.results))

    val qe_nre = Calibration.data.qe(ind); val ve_nre = calib.ve; val delta_e_nre = calib.delta_e
    // Net E = Ye * (1 - qe - delta_e q_f v_e)
    val net_e_re = if (share_re == 0) MegaTonOilEquivalent(0) else res_re.netE
    val ye_nre = (target - net_e_re) / (1.0 - qe_nre - ve_nre * delta_e_nre * qf_final)
    res_nre.updateProduction(2050, ye_nre, ye_nre * qe_nre, calib.energy_units(ye_nre.to(calib.energy_units) * ve_nre * qf_final), delta_e_nre)

    res.sumResults(start_year, List(res_re, res_nre))
    import Helper._
    // println(delta + "\t" + share_re + "\t" + u + "\t" + a + "\t" + tilde_ke + "\t" + u / (a + tilde_ke * delta) + "\t" + res_re.eroi.last + "\t" + res_nre.eroi.last + "\t" + res.eroi.last)
    // g = s*PIB/K - delta
    //println(share_re * 100 + "\t" + res.ye.last.to(calib.energy_units) + "\t" + res.e.last.to(calib.energy_units) + "\t" + res.ee.last.to(calib.energy_units) + "\t" + res.ke.last + "\t" + res_re.ke.last + "\t" + res_nre.ke.last + "\t" + Kf_final + "\t" + pib_final + "\t" + qf_final)
    val z = Z(res.ve(calib.qf), calib.vf, res.qe.last, calib.qf, calib.le, calib.lf, res.delta_e.last, calib.delta_f)
    val s_fixed = calib.s; val n_fixed = calib.n; val gk_fixed = calib.g; val c_fixed = calib.data.ce(calib.i).to(calib.energy_units) / calib.cf
    val phi = calib.mean_std(calib.interval_phi(z))
    val gk = calib.mean_std(calib.interval_gk(s_fixed, n_fixed, z))
    val m_gk_bounds = calib.interval_m_gk(s_fixed, n_fixed, z)
    val m_gk = calib.mean_std(m_gk_bounds)
    val mu_gk = calib.mean_std(calib.mu_m(m_gk_bounds._1), calib.mu_m(m_gk_bounds._2))
    val k_gk = calib.mean_std(calib.k_m(m_gk_bounds._1, z.ve), calib.k_m(m_gk_bounds._2, z.ve))
    val delta_gk = calib.mean_std((calib.delta_m(m_gk_bounds._1, z), calib.delta_m(m_gk_bounds._2, z)))

    // Calcus Elise
    val m_s_bounds = calib.interval_m_s(gk_fixed, n_fixed, z)
    val m_s = calib.mean_std(m_s_bounds)
    val mu_s = calib.mean_std(calib.mu_m(m_s_bounds._1), calib.mu_m(m_s_bounds._2))
    val k_s = calib.mean_std(calib.k_m(m_s_bounds._1, z.ve), calib.k_m(m_s_bounds._2, z.ve))
    val s = calib.mean_std(calib.interval_s(gk_fixed, n_fixed, z))

    val m_c = calib.m_c(gk_fixed, c_fixed, z)
    val n_c = calib.mean_std(calib.interval_c(c_fixed, z))
    val s_c = calib.mean_std(calib.interval_s_c(gk_fixed, c_fixed, z))

    // Calculs Marc
    val (mui, mus) = calib.mean_std(calib.interval_mu2_s(gk_fixed, n_fixed, z))
    val (si_mu, ss_mu) = calib.mean_std(calib.interval_s_mu(gk_fixed, n_fixed, z))
    val (mi_mu, ms_mu) = calib.mean_std((calib.m_mu(mui), calib.m_mu(mus)))

  //  assert(mui == mu_gk._1 && mus == mu_gk._2)
 //   assert(si_mu == s._1 && ss_mu == s._2)

    /*  println(
      round(res.ye.last.to(MegaTonOilEquivalent), 0) + " &" +
        round(share_re * 100, 0) + " & " + round(res.eroi.last) + " & " + round(100 * res.qe.last) + " & " + round(res.ve(qf_final)) + "& " +
        round(gi * 100) + " & " + round(gs * 100) + " & " + round(mi * 100) + " & " + round(ms * 100) + " & " +
        
        round(100 * mi_s) + " & " + round(100 * ms_s) + " & " + round(100 * si) + " & " + round(100 * ss)
        + " & " + round(100 * mui) + " & " + round(100 * mus) +
        " & " + round(100 * mi_mu) + " & " + round(100 * ms_mu) +
        " & " + round(100 * si_mu) + " & " + round(100 * ss_mu))*/
    // + " & " + round(res_re.ye.last.to(MegaTonOilEquivalent), 0) + " &" + round(res_re.eroi.last) + " & " + round(100 * res_re.qe.last) + " & " + round(res_re.ve(qf_final)))
    println(round(share_re * 100, 0) + "\t" +
      round(res.eroi.last) + "\t" +
      phi._1 + "\t" + phi._2 + "\t" +
      m_gk._1 + "\t" + m_gk._2 + "\t" + "\t" + gk._1 + "\t" + gk._2 + "\t" +
      m_s._1 + "\t" + m_s._2 + "\t" + s._1 + "\t" + s._2 + "\t" +
      m_c + "\t" + n_c._1 + "\t" + n_c._2 + "\t" + s_c._1 + "\t" + s_c._2)

    res
  }
  /*
  def simulateTransition(start_year: Int, end_year: Int) {

    val all_sites = Grid().cells
    val techs = List((OnshoreWindTechnology, 0.25), (OffshoreWindTechnology, 0.25), (PVMono, 0.5)) //, CSPTowerStorage12h)

    val calib = new calibration_results_work
    val techs_it = techs.map(tech => new TechnologyIterator(tech._1, all_sites, calib.delta, calib.qf, calib.qf, energy_units = calib.energy_units, pib_units = calib.pib_units))
    val ind = Calibration.index_year(start_year)

    val res = new GrowthModelResults(calib.delta); val res_re = new GrowthModelResults(calib.delta); val res_nre = new GrowthModelResults(calib.delta);
    res.updateProduction(start_year, calib.data.ye(calib.i), calib.data.ee(calib.i), calib.tilde_Ke, calib.qf, calib.energy_units)
    res_nre.updateProduction(start_year, calib.data.ye(calib.i) - MegaTonOilEquivalent(45), calib.data.ee(calib.i), calib.tilde_Ke, calib.qf, calib.energy_units)
    res_re.updateProduction(start_year, MegaTonOilEquivalent(45), Joules(0), Joules(0), calib.qf, calib.energy_units)

    // We want to reach 100% renewables by end_year: 
    val n_year = end_year - start_year;
    val qe_0 = Calibration.data.qe(ind); val ve_0 = calib.ve; val ke_0 = calib.Ke
    // If we consider an exponential growth (i.e. a constant growth rate): 
    // Ere,n = E,2017 & Ere,n = Ere,2017 * g^n => g = (E,2017/Ere,2017)^1/n
    val ye_re0 = res_re.ye.last;
    val growth_rate = math.pow(Calibration.data.ye(ind) / e_re0, 1.0 / n_year)
    println("Simulation " + n_year + ", exponential growth rate " + growth_rate)

    for (y <- 1 to n_year + 50) {
      // The desired total production from RE sources by the end of the year.
      val target_re = if (res.year.last >= end_year) Joules(0) else List(e_re0 * math.pow(growth_rate, y) - res_re.ye.last, Joules(0)).maxBy(_.value)
      println("Simulate year " + res.year.last + " with target " + target_re.to(MegaTonOilEquivalent))
      techs_it.map(it => it.simulate_year(y, target_re / techs.size, true))
      // Update params from RE sector
      res_re.sumResults(start_year + y, techs_it.map(_.results), calib.qf, calib.energy_units)
      // Remove from the total the energy that is now produced by the re sector
      // e_nre < 0 is not supposed to happen before the last year of the simulation !!
      val ye_nre = List(-res_re.ye.last, Joules(0)).maxBy(_.value)

      res_nre.updateProduction(start_year + y, ye_nre, ye_nre * qe_0, ye_nre.to(KilowattHours) * ve_0, calib.qf, calib.energy_units)
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
  */
}

class TechnologyIterator(val tech: RenewableTechnology, sites: List[Cell], qf_0: Double, qf_final: Double, log: Boolean = false, energy_units: EnergyUnit = KilowattHours,
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
      sum(installations.map(_.operationE)) + e_installation + e_decom,
      ke_installation, delta_e)
  }

  def sum(e: List[Energy]) = e.foldLeft(Joules(0))(_ + _)
  // Net Energy Target !
  def simulate_static(target: Energy) {
    // Remove the x % best sites as there are not available in practice
    /*val limit_pot = 0.5 * sites.map(tech.potential(_, 1.0)).foldLeft(Watts(0))(_ + _);
    var pot = Watts(0); var ended = !sites_sorted.hasNext;
    while (pot <= limit_pot && !ended) {
      pot = pot + tech.potential(sites_sorted.next, 1.0)
      if (!sites_sorted.hasNext || pot >= limit_pot) {
        ended = true
      }
    }*/

    // year += y
    var ended = false
    var newCap = Watts(0); var newProd = Joules(0); var newKe = Joules(0); var newEE = Joules(0);

    if (target.value != 0) {
      while (newProd <= (target + newEE + newKe / tech.lifeTime) && !ended) {
        if (sites_sorted.hasNext) {
          val next_site = sites_sorted.next()
          newProd += tech.potential(next_site, 1.0) * Hours(365 * 24)
          newCap += tech.ratedPower(next_site, 1.0)
          newEE += tech.energyInputsOMYearly(next_site, 1.0) // Is that really in qe ?
          newKe += (tech.energyInputsInstallation(next_site, 1.0) + tech.energyInputsDecomissioning(next_site, 1.0)) // Total energy embodied in energy sector capital stock; to divise by life time 
        } else {
          ended = true
        }
      }
    }
    // println(tech.name + "\t" + tech.eroi(sites_sorted.next(), 1.0) + "\t" + newCap.toGigawatts)

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

}

class GrowthModelResults(energy_units: EnergyUnit) {
  val year = scala.collection.mutable.ArrayBuffer.empty[Int];
  val ye = scala.collection.mutable.ArrayBuffer.empty[Energy];
  val ee = scala.collection.mutable.ArrayBuffer.empty[Energy];
  val tilde_ke = scala.collection.mutable.ArrayBuffer.empty[Energy];
  // Results
  val eroi = scala.collection.mutable.ArrayBuffer.empty[Double];
  val qe = scala.collection.mutable.ArrayBuffer.empty[Double];
  val delta_e = scala.collection.mutable.ArrayBuffer.empty[Double];
  // val ve = scala.collection.mutable.ArrayBuffer.empty[Double];
  // val qf = scala.collection.mutable.ArrayBuffer.empty[Double];

  def netE = ye.last - ee.last - tilde_ke.last * delta_e.last
  // EROI = Ye/(Ee+tilde(delta_e K_e)) = Y_e/(q_e Y_e + \delta_e v_e q_f Y_e) = 1/(q_e + \delta_e v_e q_f) ==> v_e = (1/EROI-q_e)/(\delta_e q_f)
  def ve(qf: Double) = ((1 / eroi.last) - qe.last) / (delta_e.last * qf)

  def updateProduction(y: Int, newYe: Energy, newEe: Energy, newTildeKe: Energy, newDelta_e: Double) {
    year += y
    ye += newYe
    ee += newEe
    tilde_ke += newTildeKe
    delta_e += newDelta_e;
    // Calculated results
    qe += (if (newYe.value == 0) 0.0 else newEe / newYe)
    eroi += (if (newYe.value == 0) 1.0 else newYe / (newEe + newDelta_e * newTildeKe))
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

