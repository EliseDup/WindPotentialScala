package calculation

import scala.io.Source
import utils.PlotHelper
import utils.Helper
import java.io.FileWriter
import au.com.bytecode.opencsv.CSVWriter
import utils.GeoPoint

object TestERA40 {
  def main(args: Array[String]) = {
    // Helper.txtToCSV(Helper.ressourcesPy + "results/meanWindLCEurope", Helper.ressourcesPy + "windEurope.csv")

    val wind = new GridData
    println("Grid Size : " + wind.grids.size + "=" + wind.grids.map(_.cellSize).sum + " km2")

    //PlotHelper.cumulativeDensity(List((wind.windSpeeds, "10 metres"), (wind.windSpeeds80, "80 metres")), xLabel = "% of Sites", yLabel = "Mean Wind Speed [m/s]")
    //PlotHelper.cumulativeDensity(List((wind.windSpeedsLand, "10 metres"), (wind.windSpeedsLand80, "80 metres")), xLabel = "% of Sites", yLabel = "Mean Wind Speed [m/s]", title = "Water Bodies Excluded")
    // PlotHelper.cumulativeDensity(wind.grids.map(_.powerDensity(80)), 100, "80 meters mean speed")

  }
}