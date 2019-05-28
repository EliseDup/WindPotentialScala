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

object ProductionFunction {
  import PlotHelper._
  import Helper._

  val all_sites = Grid().cells

  def main(args: Array[String]): Unit = {
    val techs = List(OffshoreWindTechnology,OnshoreWindTechnology,PVMono,CSPTowerStorage12h)
    techs.map(t => new ProductionFunction(all_sites, List(t), t.name).plot)
  }
}

class ProductionFunction(val sites: List[Cell], val techs: List[RenewableTechnology], val name: String,
    val defaultVR: Velocity = MetersPerSecond(11), val defaultN: Double = 8, val defaultSM: Double = 2.7) {
  import PlotHelper._
  import Helper._

  val sites_sf = sites.filter(s => techs.map(_.suitabilityFactor(s)).sum > 0)
  println("# Suitable sites for " + name + " = " + sites_sf.size + " / " + sites.size)

  // Build the list of (Energy Produced, Embodied Energy) for all the sites
  val xy: List[(Double, Energy, Energy)] = sites_sf.map(s => {
    val e_ke = techs.map(potential_ee(s, _))
    val e = e_ke.map(_._1).foldLeft(Joules(0))(_ + _)
    val ke = e_ke.map(_._2).foldLeft(Joules(0))(_ + _)
    (e / ke, ke, e)
  })

  val xy_cum = doubleToEnergy(Helper.listCumulatedVSCumulatedBy(xy.map(i => (i._1, i._2.toGigajoules, i._3.toGigajoules))))

  def plot {
    val xy_cum_double = energyToDouble(xy_cum)
    plotXY(List((xy_cum_double._1, xy_cum_double._2, "")), yLabel = "Energy Produced " + name + "[EJ/year]", xLabel = "Embodied Energy [EJ]")
  }

  def potential_ee(site: Cell, tech: RenewableTechnology): (Energy, Energy) = {
    if (tech.wind) {
      val windTech = tech.asInstanceOf[WindTechnology]
      (windTech.power(site, defaultVR, defaultN) * Hours(365 * 24) * (1 - windTech.operation_variable.toGigajoules),
        windTech.embodiedEnergy(site, windTech.ratedPower(site, defaultVR, defaultN), Joules(0)))
    } else if (tech.csp) {
      val cspTech = tech.asInstanceOf[CSP]
      if (site.dni.value == 0) (Joules(0), Joules(0))
      else {
        val panelArea = cspTech.reflectiveArea(site)
        val ratedPower = cspTech.ratedPower(panelArea, defaultSM)

        (cspTech.potential(site.dni, panelArea, defaultSM) * Hours(365 * 24) * (1 - cspTech.operation_variable.toGigajoules),
          cspTech.embodiedEnergy(ratedPower, Joules(0), panelArea))
      }
    } else {
      val pvTech = tech.asInstanceOf[PV]
      val panelArea = pvTech.reflectiveArea(site)
      val ratedPower = pvTech.ratedPower(site, 1.0)
      (pvTech.potential(site) * Hours(365 * 24) * (1 - pvTech.operation_variable.toGigajoules),
        pvTech.embodiedEnergy(ratedPower, Joules(0), panelArea))
    }
  }

  def energyToDouble(list: (List[Energy], List[Energy])) = (list._1.map(_.to(Exajoules)), list._2.map(_.to(Exajoules)))
  def doubleToEnergy(list: (List[Double], List[Double])) = (list._1.map(Gigajoules(_)), list._2.map(Gigajoules(_)))

}