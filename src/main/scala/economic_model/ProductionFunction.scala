package economic_model

import wind_solar._
import squants.time.Hours
import squants.energy._
import wind_energy._
import solar_energy._
import utils._
import solar_energy.PVMono
import solar_energy.CSPTowerStorage12h
import squants.motion.MetersPerSecond
import squants.motion.Velocity
import org.jfree.chart.plot.CombinedDomainXYPlot
import org.jfree.ui.ApplicationFrame
import org.jfree.chart.ChartPanel
import org.jfree.chart.JFreeChart

object ProductionFunction {
  import PlotHelper._
  import Helper._

  /**
   * Sources :
   * - Installed Capacity : IRENA_RE_Capacity_Statistics_2019
   * - Production: IEA
   */

  val initialValues =
    Map((OnshoreWindTechnology, (Gigawatts(540.37), GigawattHours(1127319 * 540.37 / (540.37 + 23.356)))),
      (OffshoreWindTechnology, (Gigawatts(23.356), GigawattHours(1127319 * 23.356 / (540.37 + 23.356)))),
      (PVMono, (Gigawatts(480.357), GigawattHours(443554))),
      (PVPoly, (Gigawatts(480.357), GigawattHours(443554))),
      (CSPParabolic, (Gigawatts(5.469), GigawattHours(10848) + Terajoules(1815))),
      (CSPParabolicStorage12h, (Gigawatts(5.469), GigawattHours(10848) + Terajoules(1815))),
      (CSPTowerStorage12h, (Gigawatts(5.469), GigawattHours(10848) + Terajoules(1815))))

  val all_sites = Grid().cells

  def main(args: Array[String]): Unit = {

    val techs = List(OnshoreWindTechnology, OffshoreWindTechnology, PVMono) //, CSPTowerStorage12h)
    val sites_sf = all_sites.filter(s => techs.map(_.suitabilityFactor(s)).sum > 0)
    val delta = Calibration.delta_(25)         
    val (e, qy, vy, ve, v) = Calibration.calibration_results(2017, delta, 0.05, 0.1 / 100, Some(0.5))
    techs.map(tech => simulateGrowth(0.25, initialValues(tech)._1, 2018, 2050, tech, qy, vy, 5))
    println("Simulation -- END")

    def simulateGrowth(annual_growth_rate: Double, cap_init: Power, year_init: Int, year_end: Int, tech: RenewableTechnology, qy: Double, vy: Double, eroi_min: Double) {

      val tilde_ke = scala.collection.mutable.ArrayBuffer.empty[Energy]; tilde_ke += Joules(0)
      val tilde_ie = scala.collection.mutable.ArrayBuffer.empty[Energy]; tilde_ie += Joules(0)
      val u = scala.collection.mutable.ArrayBuffer.empty[Energy]; u += Joules(0)
      val a = scala.collection.mutable.ArrayBuffer.empty[Energy]; a += Joules(0)
      val installed_cap = scala.collection.mutable.ArrayBuffer.empty[Power]; installed_cap += cap_init
      val year = scala.collection.mutable.ArrayBuffer.empty[Int]

      val ve = scala.collection.mutable.ArrayBuffer.empty[Double]
      val ke = scala.collection.mutable.ArrayBuffer.empty[Double]
      val qe = scala.collection.mutable.ArrayBuffer.empty[Double]
      val eroi = scala.collection.mutable.ArrayBuffer.empty[Double]

      val sites_sorted = sites_sf.sortBy(tech.eroi(_, 1.0)).reverse.toIterator

      var ended = false
      for (y <- year_init until year_end) {
        year += y
        if (eroi.nonEmpty && eroi.last <= eroi_min) {
          ended = true
        }
        var target = annual_growth_rate * installed_cap.last
        var newCap = Watts(0); var newProd = Joules(0); var newKe = Joules(0); var newOperationE = Joules(0)
        while (newCap <= target && !ended) {
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

        // Update parameters !
        installed_cap += (installed_cap.last + newCap)

        // Investment = new capacity + depreciation compensation of installed capacity
        tilde_ie += newKe + tilde_ke.last * delta
        // Energy sector capital stock: Capital stock(t-1) + new capital
        tilde_ke += tilde_ke.last + newKe
        u += u.last + newProd

        a += a.last + newOperationE
        ke += tilde_ke.last.toKilowattHours / qy
        ve += ke.last / u.last.toKilowattHours
        qe += a.last / u.last
        eroi += 1 / (qe.last + delta * qy * ve.last)
      }
      val year_double = year.toList.map(_.toDouble)

      val ys = List(
        //(installed_cap.toList.map(_.to(Gigawatts)), "Cap [GW]"),
        (ve.toList, "ve"),
        (qe.toList, "qe"),
        //(ke.toList, "K_e [US $]"),
        (u.toList.map(_.to(Exajoules)), "U [EJ/year]"),
        //(a.toList.map(_.to(Exajoules)), "A [EJ/year]"),
        (eroi.toList, "EROI") //(tilde_ie.toList.map(_.to(Exajoules)), "I_e [EJ/year]")
        )
      combinedPlots(year_double, ys, xLabel = tech.name + "(" + cap_init.toGigawatts.toInt + "GW in 2017, annual growth rate " + annual_growth_rate * 100.toInt + " %)", title = tech.name)

    }
  }
}

class ProductionFunction(val sites: List[Cell], val techs: List[RenewableTechnology], val name: String,
    val defaultVR: Velocity = MetersPerSecond(11), val defaultN: Double = 8, val defaultSM: Double = 2.7) {
  import PlotHelper._
  import Helper._

  val sites_sf = sites.filter(s => techs.map(_.suitabilityFactor(s)).sum > 0)
  println("# Suitable sites for " + name + " = " + sites_sf.size + " / " + sites.size)

  def energyToDouble(list: (List[Energy], List[Energy])) = (list._1.map(_.to(Exajoules)), list._2.map(_.to(Exajoules)))
  def doubleToEnergy(list: (List[Double], List[Double])) = (list._1.map(Gigajoules(_)), list._2.map(Gigajoules(_)))
}