

import utils.PlotHelper
import grid.WorldGrid
import solar_energy.SolarPotential
import squants.energy._
import utils._
import squants.time.Hours
import squants.space.SquareMeters
import squants.radio.WattsPerSquareMeter
import squants.space.SquareKilometers
import solar_energy.EmbodiedEnergyPV

object Test {
  import DayMonth._
  def main(args: Array[String]): Unit = {
test
    println(66 + "\t" + month_dayInMonth(66))
    println(144 + "\t" + month_dayInMonth(144))
    println(200 + "\t" + month_dayInMonth(200))
    println(250 + "\t" + month_dayInMonth(250))
    println(360 + "\t" + month_dayInMonth(360))
    
    val w = WorldGrid.simple()
    val g = w.grids.filter(i => i.EEZ)

    // w.writeGrid("k_av")
  val countries: List[(String,Double)]  = List(("Puerto Rico",18.4),
("Mexico",19.4),
("Argentina",-34.6),
("Chile",-33),
("Ecuador",-0.4),
("Peru",-12.1),
("Venezuela",10.5),
("Venezuela",10.6))
    countries.map(c =>{
      print(c._1 +"\t")
val cell =    g.filter(_.country.countries.contains(c._1)).filter(i => Math.abs(i.center.latitude.toDegrees-c._2) < 0.5)(0)
print(cell.center.latitude.toDegrees + "\t" )
cell.monthlyClearnessIndex.map(k => print(k +"\t"))
    println()
    }
  )
    def plotEROI {

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