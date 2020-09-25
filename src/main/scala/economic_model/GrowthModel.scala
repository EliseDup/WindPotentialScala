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
  // Jacobson scenario for 2050: 
  // 14.89% rooftop residential + 11.58 % commercial
  // 21.36 % pv power plants  
  // 9.72% CSP
  // 23.52% Onshore wind
  // 13.62% Offshore wind
  // Total = 94.69 / 100 
  val techs = List((OnshoreWindTechnology, 23.52 / 94.69), (OffshoreWindTechnology, 13.62 / 94.69), (PVMono, (14.89 + 11.58 + 21.36) / 94.69), (CSPTowerStorage12h, 9.72 / 94.69))
  val calib = new calibration_results_CI
  val e_units = MegaTonOilEquivalent; val pib_units = 1E9.toInt;
  val n = 100
  val share = (0 to n).map(_ / n.toDouble).toList
  val all_sites ={
    val s = System.currentTimeMillis() 
    val c = Grid().cells
    println("Loading Sites in " + (System.currentTimeMillis()-s)/1000  + " s ")
    c
  }
   
  def main(args: Array[String]): Unit = {
    // simulateTransition(share, techs)
    val r = x_qe_xe_ve(calib.data.ye(calib.i))
    val r2 = x_qe_xe_ve(2*calib.data.ye(calib.i))
    val r3 = x_qe_xe_ve(5*calib.data.ye(calib.i))
  }

  def x_qe_xe_ve(ye: Energy, s: List[Double] = share) = {
     val techs_it = techs.map(tech => (new TechnologyIterator(tech._1, all_sites, energy_units = calib.energy_units, pib_units = calib.pib_units), tech._2))
    val ind = Calibration.index_year(2017)

    val (res, res_re, res_nre) = (new GrowthModelResults(calib.energy_units), new GrowthModelResults(calib.energy_units), new GrowthModelResults(calib.energy_units))
    res.updateProduction(2017, ye, ye * calib.qe, ye * calib.xe * calib.qf, ye * calib.ve * calib.qf, calib.delta_e)
    res_nre.updateProduction(2017, ye, ye * calib.qe, ye * calib.xe * calib.qf, ye * calib.ve * calib.qf, calib.delta_e)
    res_re.updateProduction(2017, Joules(0), Joules(0), Joules(0), Joules(0), calib.delta_e)

    // We want to reach 100% renewables by end_year: 
    val n_year = 2017 + share.size;
    val qe_nre = Calibration.data.qe(ind); val xe_nre = calib.xe; val ve_nre = calib.ve; val delta_e_nre = calib.delta_e; val qf = calib.qf

    // If we consider an exponential growth (i.e. a constant growth rate): 
    // Ere,n = E,2017 & Ere,n = Ere,2017 * g^n => g = (E,2017/Ere,2017)^1/n
    val target = ye
    var i = 0
    val list = share.map(s => {
      i += 1
      // The desired total production from RE sources by the end of the year.
      val target_re = (s * target) - res_re.ye.last
      // println("Simulate year " + res.year.last + " with target " + target_re.to(MegaTonOilEquivalent) + "\t" + res_re.ye.last.to(MegaTonOilEquivalent))
      techs_it.map(it => it._1.simulate_year(2017 + i, target_re * it._2, true, 1.0))
      // Update params from RE sector
      res_re.sumResults(2017 + i, techs_it.map(_._1.results))
      // Remove from the total the energy that is now produced by the re sector
      // e_nre < 0 is not supposed to happen before the last year of the simulation !!
      val ye_nre = (1 - s) * target
      res_nre.updateProduction(2017 + i, ye_nre, ye_nre * qe_nre, calib.energy_units(ye_nre.to(calib.energy_units) * xe_nre * qf), calib.energy_units(ye_nre.to(calib.energy_units) * ve_nre * qf), delta_e_nre)
      res.sumResults(2017 + i, List(res_re, res_nre))
      (s, res.qe.last, res.xe(calib.qf), res.ve(calib.qf))
    })
    (list.map(_._1), list.map(_._2), list.map(_._3), list.map(_._4))
  }
  // For a given target (= final net energy demand NE), and renewable energy share, 
  // gives a estimate of the technical parameters and EROI of the energy system
  def calculate(target: Energy, net: Boolean, techs: List[(RenewableTechnology, Double)], share_re: Double = 1.0,
    calib: calibration_results_CI, qf: Double, lf: Double): Z_xi = {
    val all_sites = Grid().cells;
    // Initialise 
    val start_year = 2017; val ind = Calibration.index_year(start_year)
    val (res, res_re, res_nre) = (new GrowthModelResults(calib.energy_units), new GrowthModelResults(calib.energy_units), new GrowthModelResults(calib.energy_units))
    res.updateProduction(start_year, calib.data.ye(calib.i), calib.data.ee(calib.i), calib.tilde_Xe(qf), calib.tilde_Ke(qf), calib.delta_e)
    res_nre.updateProduction(start_year, calib.data.ye(calib.i), calib.data.ee(calib.i), calib.tilde_Xe(qf), calib.tilde_Ke(qf), calib.delta_e)
    res_re.updateProduction(start_year, Joules(0), Joules(0), Joules(0), Joules(0), calib.delta_e)
    // res_nre.updateProduction(start_year, calib.data.ye(calib.i) - MegaTonOilEquivalent(45), calib.data.e(calib.i) - MegaTonOilEquivalent(45), calib.data.ee(calib.i), calib.Ke, qf_0, calib.energy_units)
    // res_re.updateProduction(start_year, MegaTonOilEquivalent(45), MegaTonOilEquivalent(45), Joules(0), 0, qf_0, calib.energy_units)

    // Iterate on each technology to produce at optimal eroi
    val techs_it = techs.map(tech => new TechnologyIterator(tech._1, all_sites, energy_units = calib.energy_units, pib_units = calib.pib_units))
    techs_it.map(t => t.simulate_static((target * share_re) * techs.find(_._1.equals(t.tech)).get._2, net, qf / calib.qf))
    res_re.sumResults(2050, techs_it.map(_.results))

    val qe_nre = Calibration.data.qe(ind); val ve_nre = calib.ve; val delta_e_nre = calib.delta_e; val xe_nre = calib.xe
    // Net E = Ye * (1 - qe - delta_e q_f v_e)
    val net_e_re = if (share_re == 0) MegaTonOilEquivalent(0) else res_re.netE
    val ye_nre = if (net) (target - net_e_re) / (1.0 - qe_nre - ve_nre * delta_e_nre * qf) else target - res_re.ye.last
    res_nre.updateProduction(2050, ye_nre, ye_nre * qe_nre, calib.energy_units(ye_nre.to(calib.energy_units) * xe_nre * qf), calib.energy_units(ye_nre.to(calib.energy_units) * ve_nre * qf), delta_e_nre)

    res.sumResults(start_year, List(res_re, res_nre))
    import Helper._

    println(share_re + "\t" + res_re.qe.last + "\t" + res_re.tilde_ke.last.to(MegaTonOilEquivalent) + "\t" + res_re.delta_e.last + "\t" + res_re.ye.last.to(MegaTonOilEquivalent) + "\t" + res_re.ee.last.to(MegaTonOilEquivalent))
    Z_xi(res.ve(qf), calib.vf, res.qe.last, qf, res.xe(qf), calib.xf, res.delta_e.last, calib.delta_f)
  }

  def simulateTransition(shares: List[Double], techs: List[(RenewableTechnology, Double)], file_name: String = "x_qe_xe_ve_100_new") {
    val out_stream = new java.io.PrintStream(new java.io.FileOutputStream(file_name))

    val all_sites = Grid().cells
    val calib = new calibration_results_CI
    val techs_it = techs.map(tech => (new TechnologyIterator(tech._1, all_sites, energy_units = calib.energy_units, pib_units = calib.pib_units), tech._2))
    val ind = Calibration.index_year(2017)
    println(calib.qf + "\t" + calib.qe + "\t" + calib.ve + "\t" + calib.xe + "\t" + calib.tilde_Ke.to(MegaTonOilEquivalent) + "\t" + calib.tilde_Xe.to(MegaTonOilEquivalent))

    val (res, res_re, res_nre) = (new GrowthModelResults(calib.energy_units), new GrowthModelResults(calib.energy_units), new GrowthModelResults(calib.energy_units))
    res.updateProduction(2017, calib.data.ye(calib.i), calib.data.ee(calib.i), Joules(0), calib.tilde_Ke(calib.qf), calib.delta_e)
    res_nre.updateProduction(2017, calib.data.ye(calib.i), calib.data.ee(calib.i), calib.tilde_Xe(calib.qf), calib.tilde_Ke(calib.qf), calib.delta_e)
    res_re.updateProduction(2017, Joules(0), Joules(0), Joules(0), Joules(0), calib.delta_e)

    // We want to reach 100% renewables by end_year: 
    val n_year = 2017 + shares.size;
    val qe_nre = Calibration.data.qe(ind); val xe_nre = calib.xe; val ve_nre = calib.ve; val delta_e_nre = calib.delta_e; val qf = calib.qf

    // If we consider an exponential growth (i.e. a constant growth rate): 
    // Ere,n = E,2017 & Ere,n = Ere,2017 * g^n => g = (E,2017/Ere,2017)^1/n
    val target = calib.data.ye(ind)
    var i = 0
    for (s <- shares) {
      i += 1
      // The desired total production from RE sources by the end of the year.
      val target_re = (s * target) - res_re.ye.last
      // println("Simulate year " + res.year.last + " with target " + target_re.to(MegaTonOilEquivalent))
      techs_it.map(it => it._1.simulate_year(2017 + i, target_re * it._2, true, 1.0))
      // Update params from RE sector
      res_re.sumResults(2017 + i, techs_it.map(_._1.results))
      // Remove from the total the energy that is now produced by the re sector
      // e_nre < 0 is not supposed to happen before the last year of the simulation !!
      val ye_nre = (1 - s) * target
      res_nre.updateProduction(2017 + i, ye_nre, ye_nre * qe_nre, calib.energy_units(ye_nre.to(calib.energy_units) * xe_nre * qf), calib.energy_units(ye_nre.to(calib.energy_units) * ve_nre * qf), delta_e_nre)
      res.sumResults(2017 + i, List(res_re, res_nre))

      // println(s + "\t" + res.qe.last + "\t" + res.ve(calib.qf) + "\t" + res.xe(calib.qf) + "\t" + res.tilde_ke.last.to(MegaTonOilEquivalent) + "\t" + res.tilde_xe.last.to(MegaTonOilEquivalent) + "\t" + res.ye.last.to(MegaTonOilEquivalent))
      // println(s + "\t" + res_re.qe.last + "\t" + res_re.ve(qf) + "\t" + res_re.xe(qf)  + "\t" + res_re.tilde_ke.last.to(MegaTonOilEquivalent) + "\t" + res_re.tilde_xe.last.to(MegaTonOilEquivalent) + "\t" + res_re.delta_e.last + "\t" + res_re.ye.last.to(MegaTonOilEquivalent) + "\t" + res_re.ee.last.to(MegaTonOilEquivalent))
      out_stream.print(s + "\t" + res.qe.last + "\t" + res.xe(calib.qf) + "\t" + res.ve(calib.qf) + "\t" + res.tilde_xe.last.to(MegaTonOilEquivalent) + "\t" + res.tilde_ke.last.to(MegaTonOilEquivalent) + "\t" + res.ye.last.to(MegaTonOilEquivalent) + "\n")
    }
    out_stream.close()
  }

  def simulate_qe_xe_ve(techs: List[(RenewableTechnology, Double)], qf_final: Double) {
    val calib = new calibration_results_CI
    val all_sites = Grid().cells
    val techs_it = techs.map(tech => (new TechnologyIterator(tech._1, all_sites, energy_units = calib.energy_units, pib_units = calib.pib_units), tech._2))
    val res = new GrowthModelResults(calib.energy_units)
    res.updateProduction(2017, Joules(0), Joules(0), Joules(0), Joules(0), calib.delta_e)
    val out_stream = new java.io.PrintStream(new java.io.FileOutputStream("ye_qe_xe_ve"))

    for (i <- (1 to 2000)) {
      val target = MegaTonOilEquivalent(10)
      techs_it.map(tech => tech._1.simulate_year(i + 2017, target * tech._2, false, qf_final / calib.qf))
      res.sumResults(i + 2017, techs_it.map(_._1.results))
      out_stream.print(res.ye.last.to(MegaTonOilEquivalent).toString +
        "\t" + res.qe.last.toString + "\t" + res.xe(calib.qf).toString + "\t" + res.ve(calib.qf).toString + "\n")
    }
    out_stream.close()
  }
}

class TechnologyIterator(val tech: RenewableTechnology, sites: List[Cell], log: Boolean = false, energy_units: EnergyUnit = KilowattHours,
    pib_units: Int = 1) {
  // Initialize with current installed capacity and energy produced
  val (cap0, e0) = (Watts(0), Joules(0)) //ProductionFunction.initialValues(tech)
  val ke0 = tech.ee.energyInputsInstallation(cap0) // .to(energy_units) / qf_0;
  val oe0 = Joules(0) // tech.ee.energyInputsOYearly(cap0);
  val xe0 = tech.ee.energyInputsOMYearly(cap0)
  val results = new GrowthModelResults(energy_units)
  val delta_e = 1.0 / tech.lifeTime
  results.updateProduction(2017, e0, oe0, xe0, ke0, delta_e)

  //assert(math.abs(results.eroi.last - (e0 + oe0) / (oe0 + delta * tech.ee.energyInputsInstallation(cap0))) < 0.1)

  val tilde_ke = scala.collection.mutable.ArrayBuffer.empty[Energy]; tilde_ke += tech.ee.energyInputsInstallation(cap0)
  val tilde_ie = scala.collection.mutable.ArrayBuffer.empty[Energy]; tilde_ie += tilde_ke.last * delta_e
  val installed_cap = scala.collection.mutable.ArrayBuffer.empty[Power]; installed_cap += cap0

  val sites_sorted = sites.filter(s => tech.suitabilityFactor(s) > 0).sortBy(tech.eroi(_, 1.0)).reverse.toIterator

  var installations: List[RenewableInstallation] = List()
  def simulate_year(y: Int, target: Energy, target_final: Boolean, qf_ratio: Double) {
    // year += y
    var ended = false
    // New direct ke is a "one-shot" investment
    var newCap = Watts(0); var newProd = Joules(0); var newOperationE = Joules(0); //var newXe = Joules(0);
    var newIndirectInstallationE = Joules(0); var newDirectInstallationE = Joules(0); var newDirectDecommissioningE = Joules(0)

    def progress = { if (target_final) (newProd - newOperationE) else newProd }
    if (target.value != 0) {
      while (progress <= target && !ended) {
        if (sites_sorted.hasNext) {
          val next_site = sites_sorted.next()
          val prod = tech.potential(next_site, 1.0) * Hours(365 * 24)
          newProd += prod
          newOperationE += tech.energyInputsOMYearly(next_site, 1.0)
          // newXe += tech.energyInputsOMYearly(next_site, 1.0)
          // If qf decreases the energy embodied also decreases !
          newDirectInstallationE += qf_ratio * tech.directEnergyInputsInstallation(next_site, 1.0)
          newIndirectInstallationE += qf_ratio * tech.indirectEnergyInputsInstallation(next_site, 1.0)
          newDirectDecommissioningE += qf_ratio * tech.energyInputsDecomissioning(next_site, 1.0)
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
      Joules(0), //sum(installations.map(_.operationE)),
      sum(installations.map(_.tildeXe)),
      sum(installations.map(_.tildeKe)),
      //ke_installation + e_installation + e_decom, 
      delta_e)
  }

  def sum(e: List[Energy]) = e.foldLeft(Joules(0))(_ + _)
  // Net Energy Target !
  def simulate_static(target: Energy, net: Boolean, qf_ratio: Double) {
    // year += y
    var ended = false; var currentTarget = Joules(0);
    var newCap = Watts(0); var newProd = Joules(0); var newKe = Joules(0); var newXe = Joules(0);

    if (target.value != 0) {
      while (newProd <= currentTarget && !ended) {
        if (sites_sorted.hasNext) {
          val next_site = sites_sorted.next()
          newProd += tech.potential(next_site, 1.0) * Hours(365 * 24)
          newCap += tech.ratedPower(next_site, 1.0)
          newXe += tech.energyInputsOMYearly(next_site, 1.0)
          newKe += qf_ratio * (tech.energyInputsInstallation(next_site, 1.0) + tech.energyInputsDecomissioning(next_site, 1.0)) // Total energy embodied in energy sector capital stock; to divise by life time 
        } else {
          ended = true
        }
        currentTarget = if (net) (target + newXe + newKe / tech.lifeTime) else target
      }
    }
    if (log) println(target.to(MegaTonOilEquivalent) + "\t" + tech.name + ",static , new production:" + newProd.to(MegaTonOilEquivalent) + "(Target was : " + target.to(MegaTonOilEquivalent))

    // Update parameters !
    installed_cap += (installed_cap.last + newCap)
    // Investment = new capacity + depreciation compensation of installed capacity
    tilde_ie += newKe + tilde_ke.last / tech.lifeTime
    // Energy sector capital stock: Capital stock(t-1) + new capital
    tilde_ke += tilde_ke.last + newKe
    results.updateProduction(2050, results.ye.last + newProd, Joules(0), results.tilde_xe.last + newXe, tilde_ke.last, delta_e)
  }
}

class RenewableInstallation(val tech: RenewableTechnology, val rated_power: Power, val production: Energy,
    val operationE: Energy, val directInstallationE: Energy, val indirectInstallationE: Energy, val directDecommissioningE: Energy) {
  var age = -1
  def updateYear {
    age += 1
    if (age == tech.lifeTime + 1) age = 0
  }
  val tildeKe = directInstallationE + indirectInstallationE + directDecommissioningE
  val tildeXe = operationE
}

class GrowthModelResults(energy_units: EnergyUnit) {
  val year = scala.collection.mutable.ArrayBuffer.empty[Int];
  val ye = scala.collection.mutable.ArrayBuffer.empty[Energy];
  val ee = scala.collection.mutable.ArrayBuffer.empty[Energy];
  val tilde_ke = scala.collection.mutable.ArrayBuffer.empty[Energy];
  val tilde_xe = scala.collection.mutable.ArrayBuffer.empty[Energy];

  // Results
  // val eroi = scala.collection.mutable.ArrayBuffer.empty[Double];
  val qe = scala.collection.mutable.ArrayBuffer.empty[Double];
  val delta_e = scala.collection.mutable.ArrayBuffer.empty[Double];

  def netE = ye.last - ee.last - tilde_ke.last * delta_e.last
  // EROI = Ye/(Ee+tilde(delta_e K_e)) = Y_e/(q_e Y_e + \delta_e v_e q_f Y_e) = 1/(q_e + \delta_e v_e q_f) ==> v_e = (1/EROI-q_e)/(\delta_e q_f)
  def eroi(qf: Double): Double = 1.0 / (qe.last + (delta_e.last * ve(qf) + xe(qf)) * qf)

  def ve(qf: Double): Double = tilde_ke.last / ye.last / qf
  def xe(qf: Double): Double = tilde_xe.last / ye.last / qf
  // def le(le_0: Double) = le_0 * (1 + (qe.last - qe(0)) / qe(0))

  def updateProduction(y: Int, newYe: Energy, newEe: Energy, newTildeXe: Energy, newTildeKe: Energy, newDelta_e: Double) {
    year += y
    ye += newYe
    ee += newEe
    tilde_ke += newTildeKe
    tilde_xe += newTildeXe
    delta_e += newDelta_e;
    // Calculated results
    qe += (if (newYe.value == 0) 0.0 else newEe / newYe)
    // eroi += (if (newYe.value == 0) 1.0 else newYe / (newEe + newDelta_e * newTildeKe))
  }

  def sumResults(y: Int, res: List[GrowthModelResults]) {
    val new_tke = res.map(_.tilde_ke.last).foldLeft(Joules(0))(_ + _)
    val new_txe = res.map(_.tilde_xe.last).foldLeft(Joules(0))(_ + _)
    val new_delta_e = if (new_tke.value == 0) 1.0 else res.map(r => r.delta_e.last * r.tilde_ke.last).foldLeft(Joules(0))(_ + _) / new_tke
    updateProduction(y,
      res.map(_.ye.last).foldLeft(Joules(0))(_ + _),
      res.map(_.ee.last).foldLeft(Joules(0))(_ + _),
      new_txe, new_tke,
      // res.map(_.qf.last).sum / res.size,
      new_delta_e)
  }
}

