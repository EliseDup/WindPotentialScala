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
    Map((OnshoreWindTechnology, (Gigawatts(540.37), GigawattHours(1127319*540.37/(540.37+23.356)))),
      (OffshoreWindTechnology, (Gigawatts(23.356), GigawattHours(1127319*23.356/(540.37+23.356)))),
      (PVMono, (Gigawatts(480.357), GigawattHours(443554))),
      (PVPoly, (Gigawatts(480.357), GigawattHours(443554))),
      (CSPParabolic, (Gigawatts(5.469), GigawattHours(10848)+Terajoules(1815))),
      (CSPParabolicStorage12h, (Gigawatts(5.469), GigawattHours(10848)+Terajoules(1815))),
      (CSPTowerStorage12h, (Gigawatts(5.469), GigawattHours(10848)+Terajoules(1815))))

  val all_sites = Grid().cells

  def main(args: Array[String]): Unit = {

    val techs = List(OnshoreWindTechnology, OffshoreWindTechnology, PVMono, CSPTowerStorage12h)
    val sites_sf = all_sites.filter(s => techs.map(_.suitabilityFactor(s)).sum > 0)
    val delta = GrowthModel.delta
    techs.map(tech =>
    simulateTransition(0.25, initialValues(tech)._1, 2018, 2050, tech, GrowthModel.qy, GrowthModel.vy)
 )
    println("Simulation -- END")

    def simulateTransition(annual_growth_rate: Double, cap_init: Power, year_init: Int, year_end: Int, tech: RenewableTechnology, qy: Double, vy: Double) {

      val tilde_ke = scala.collection.mutable.ArrayBuffer.empty[Energy]; tilde_ke += Joules(0)
      val tilde_ie = scala.collection.mutable.ArrayBuffer.empty[Energy]; tilde_ie += Joules(0)
      val u = scala.collection.mutable.ArrayBuffer.empty[Energy]; u += Joules(0)
      val a = scala.collection.mutable.ArrayBuffer.empty[Energy]; a += Joules(0)
      val installed_cap = scala.collection.mutable.ArrayBuffer.empty[Power]; installed_cap += cap_init
      val year = scala.collection.mutable.ArrayBuffer.empty[Int]

      val ve = scala.collection.mutable.ArrayBuffer.empty[Double]
      val ke = scala.collection.mutable.ArrayBuffer.empty[Double]
      val qe = scala.collection.mutable.ArrayBuffer.empty[Double]

      val sites_sorted = sites_sf.sortBy(tech.eroi(_, 1.0)).reverse.toIterator

      var ended = false
      for (y <- year_init until year_end) {
        year += y

        var target = annual_growth_rate * installed_cap.last
        var newCap = Watts(0); var newProd = Joules(0); var newKe = Joules(0); var newOperationE = Joules(0)
        while (newCap <= target && !ended) {
          if (sites_sorted.hasNext) {
            val next_site = sites_sorted.next()
            val prod = tech.potential(next_site, 1.0) * Hours(365 * 24)
            newProd += prod
            newOperationE += prod * tech.operation_variable + tech.energyInputsOMYearly(next_site, 1.0)
            newKe += tech.energyInputsInstallation(next_site, 1.0) + tech.energyInputsDecomissioning(next_site, 1.0)
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
        ve += u.last.toKilowattHours / ke.last
        qe += a.last / u.last
      }
      val year_double = year.toList.map(_.toDouble)

      val ys = List(
        //(installed_cap.toList.map(_.to(Gigawatts)), "Cap [GW]"),
        (ve.toList, "ve"),
        (qe.toList, "qe"),
        (ke.toList, "K_e [US $]"),
        (u.toList.map(_.to(Exajoules)), "U [EJ/year]"),
        (a.toList.map(_.to(Exajoules)), "A [EJ/year]")
        //(tilde_ie.toList.map(_.to(Exajoules)), "I_e [EJ/year]")
        )
      combinedPlots(year_double, ys)

    }
  }
}

class ProductionFunction(val sites: List[Cell], val techs: List[RenewableTechnology], val name: String,
    val defaultVR: Velocity = MetersPerSecond(11), val defaultN: Double = 8, val defaultSM: Double = 2.7) {
  import PlotHelper._
  import Helper._

  val sites_sf = sites.filter(s => techs.map(_.suitabilityFactor(s)).sum > 0)
  println("# Suitable sites for " + name + " = " + sites_sf.size + " / " + sites.size)

  // Build the list of Gross Energy Produced, Net Energy Produced, Embodied Energy, Operational Energy) for all the sites
  val e_ne_ee: List[(Double, Energy, Energy, Energy)] = sites_sf.map(s => {
    /*val e_ee = techs.map(potential_ee(s, _))
    val e = e_ee.map(_._1).foldLeft(Joules(0))(_ + _)
    val ne = e_ee.map(_._2).foldLeft(Joules(0))(_ + _)
    val ee = e_ee.map(_._3).foldLeft(Joules(0))(_ + _)
    (e / ee, e, ne, ee)*/
    (0.0,Joules(0),Joules(0),Joules(0))
  })
  // (Embodied Energy, Energy delivered) cumulated, by decreasing ratio of e/ee
  val ee_ed_cum = doubleToEnergy(Helper.listCumulatedVSCumulatedBy(e_ne_ee.map(i => (i._1, i._4.toGigajoules, i._2.toGigajoules))))

  def plot(interpolation: Option[(Double, Double, Double)] = None) {
    val ee_ed_cum_double = energyToDouble(ee_ed_cum)
    println("Max Ed " + math.round(ee_ed_cum_double._2.max))

    var list = List((ee_ed_cum_double._1, ee_ed_cum_double._2, "Energy Delivered"))

    interpolation match {
      case Some(params) => {
        val max_x = (-params._2 / (2 * params._1)).toInt
        println(max_x + "\t" + ee_ed_cum_double._1.max)
        val x = (0 to max_x + 10).map(_.toDouble).toList //ee_ed_cum_double._1.max.toInt).map(_.toDouble).toList
        val inter = List((x, x.map(i => params._1 * i * i + params._2 * i + params._3), "Interpolation"))
        list = list ++ inter
      }
      case None => {}
    }
    plotXY(list, yLabel = name + " [EJ/year]", xLabel = "Embodied Energy [EJ/year]", title = name, legend = list.size > 1)
  }
 
  val ed_params = Map(
    (OnshoreWindTechnology, (-0.0637018610663, 11.2840180061, 7.87916132725)),
    (OffshoreWindTechnology, (-0.0806316861928, 9.28538696435, 0.831048129606)),
    (PVMono, (-0.0172059611559, 11.0685240846, -15.6644974786)),
    (CSPTowerStorage12h, (-0.175549090097, 16.5633370464, 3.19917790027)))
  val ed_params_total = (-0.0105137914601, 10.2371376248, 2.35772267845)

  def energyToDouble(list: (List[Energy], List[Energy])) = (list._1.map(_.to(Exajoules)), list._2.map(_.to(Exajoules)))
  def doubleToEnergy(list: (List[Double], List[Double])) = (list._1.map(Gigajoules(_)), list._2.map(Gigajoules(_)))
}