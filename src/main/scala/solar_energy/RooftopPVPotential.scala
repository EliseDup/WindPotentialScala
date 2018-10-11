package solar_energy

import utils.Helper
import squants.space.SquareKilometers
import squants.radio.WattsPerSquareMeter
import squants.energy.Watts
import utils.Terawatts
import squants.space.Area
import squants.radio.Irradiance
import squants.time.Hours
import utils.Exajoules
import utils._
import squants.energy.Joules

object RooftopPVPotential {
  import PlotHelper._
import Helper._
  val grid = SolarGrid._0_5deg
  val rooftop_area = Helper.getLines("../resources/data_solar/rooftop_area", "\t").map(i => (i(0).toString, SquareKilometers(i(1).toInt))).filter(i => grid.country(i._1).nonEmpty)

  val resources: List[(String, Area, Irradiance)] = rooftop_area.map(c => {
    val cells = grid.country(c._1)
    (c._1, c._2, WattsPerSquareMeter(cells.map(_.ghi.toWattsPerSquareMeter).sum / cells.size))
  })

  def main(args: Array[String]): Unit = {
    val area = (resources.map(_._2).foldLeft(SquareKilometers(0))(_ + _))
    val meanGhi = (resources.map(_._3).foldLeft(WattsPerSquareMeter(0))(_ + _) / resources.size)

    val pot = (resources.map(i => potential(i._2, i._3)).foldLeft(Joules(0))(_ + _))
    println(pot.to(Exajoules))
    val results = listValueVSCumulated(resources.map(r => (PVMono.eroi(r._3), potential(r._2,r._3).to(Exajoules))))
    plotXY(results)
  }
  
  def potential(area: Area, irradiance: Irradiance, tech: SolarTechnology = PVMono) = {
    tech.lifeTimeEfficiency(irradiance) * area * irradiance / tech.occupationRatio * Hours(365*24)
  }
}
  