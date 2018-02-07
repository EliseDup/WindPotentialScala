

import utils.PlotHelper
import grid.WorldGrid
import solar_energy._
import squants.energy._
import utils._
import squants.time.Hours
import squants.space._
import squants.radio.WattsPerSquareMeter
import squants.space.SquareKilometers
import solar_energy.EmbodiedEnergyPV
import squants.radio.Irradiance
import grid.GridCell
import grid.GlobCoverClasses
import scala.io.Source

object Test {
  import DayMonth._
  import SolarPower._
  import PlotHelper._
  def main(args: Array[String]): Unit = {

    val l = Source.fromFile("../WindPotentialPython/solar_0_05deg").getLines()
    val list = l.map(i => i.split("\t")).map(i => (GeoPoint(Degrees(i(0).toDouble), Degrees(i(1).toDouble)),
      WattsPerSquareMeter(i(2).toDouble / 24 * 1000), WattsPerSquareMeter(i(3).toDouble / 24 * 1000), GlobCoverClasses.landCoverType(i(4).toInt))).toList
    println(list.size)

    PlotHelper.cumulativeDensity(List((list.map(_._2.toWattsPerSquareMeter), "Global"), (list.map(_._3.toWattsPerSquareMeter), "Direct")))

    def plotEROI(w: WorldGrid, g: List[GridCell]) {

      PlotHelper.cumulativeDensity(List((g.map(_.irradiance.month(0).toWattsPerSquareMeter), "January"), (g.map(_.irradiance.month(6).toWattsPerSquareMeter), "July")), legend = true,
        xLabel = "% Sites", yLabel = "Monthly Irradiance [W/m²]")
      w.writeGrid("jan_ju")

      val eroiMax1 = Math.ceil(g.map(i => SolarPotential.eff_17.eroi(i, 1, true)).max).toInt
      val eroiMax2 = Math.ceil(g.map(i => SolarPotential.eff_24.eroi(i, 1, true)).max).toInt

      PlotHelper.plotXY(List(
        SolarPotential.eff_17.potential_eroi((2 to 2 * eroiMax1).map(_ * 0.5).toList, true, g, "0.17"),
        SolarPotential.eff_24.potential_eroi((2 to 2 * eroiMax2).map(_ * 0.5).toList, true, g, "0.24")),
        xLabel = "Solar Potential [EJ/year]", yLabel = "EROI",
        legend = true)

    }
  }

}