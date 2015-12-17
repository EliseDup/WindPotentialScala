package calculation

import scala.io.Source
import utils.PlotHelper
import utils.Helper
import java.io.FileWriter
import au.com.bytecode.opencsv.CSVWriter
import utils.GeoPoint

object TestERA40 {
  def main(args: Array[String]) = {
    //Helper.txtToCSV(Helper.ressourcesPy + "results/worldDailyAugust2002test", Helper.ressourcesPy + "test.csv", List(4,3,2),true)
    val name = "worldDaily2002_0_5deg"
    val wind = new GridData("results/" + name+"lc")
    wind.writeGrid(name + "final")
    println("Grid Size : " + wind.grids.size + "=" + wind.grids.map(_.area).sum + " km2")

    PlotHelper.cumulativeDensity(List((wind.windSpeeds, "10 metres"), (wind.windSpeeds80, "80 metres")), xLabel = "% of Sites", yLabel = "Mean Wind Speed [m/s]")
    PlotHelper.cumulativeDensity(List((wind.windSpeedsLand, "10 metres"), (wind.windSpeedsLand80, "80 metres")), xLabel = "% of Sites", yLabel = "Mean Wind Speed [m/s]")
    // PlotHelper.cumulativeDensity(wind.grids.map(_.powerDensity(80)), 100, "80 meters mean speed")

  }
}