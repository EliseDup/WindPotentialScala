package calculation

import scala.io.Source
import utils.PlotHelper
import utils.Helper
import java.io.FileWriter
import au.com.bytecode.opencsv.CSVWriter
import utils.GeoPoint
import energyGeneration.GridData
import squants.space.SquareMeters
import squants.space.SquareKilometers
import squants.space.Degrees

object TestERA40 {
  def main(args: Array[String]) = {
    //Helper.txtToCSV(Helper.ressourcesPy + "results/worldDailyAugust2002test", Helper.ressourcesPy + "test.csv", List(4,3,2),true)
    val name = "europeDaily2002"
    val wind = new GridData("results/" + name+"lc", Degrees(0.125), new WindTurbine3MW(), new WindTurbine3MW())
   // wind.writeGrid(name + "final")
    println("Grid Size : " + wind.grids.size + "=" + wind.grids.map(_.area).foldLeft(SquareKilometers(0))(_ plus _))

    PlotHelper.cumulativeDensity(List((wind.windSpeeds(), "10 metres"), (wind.windSpeeds80(), "80 metres")), xLabel = "% of Sites", yLabel = "Mean Wind Speed [m/s]")
    PlotHelper.cumulativeDensity(List((wind.powerDensities(), "10 metres"), (wind.powerDensities80(), "80 metres")), xLabel = "% of Sites", yLabel = "Power Density [W/m^2]")
    PlotHelper.cumulativeDensity(List((wind.erois(wind.landGrids()),"")), xLabel = "% of Sites", yLabel = "EROI")

  }
}