package calculation

import scala.io.Source
import utils.PlotHelper
import utils.Helper
import java.io.FileWriter
import au.com.bytecode.opencsv.CSVWriter
import utils.GeoPoint
import utils.GeoPoint

object TestERA40 {
  def main(args: Array[String]) = {
    // Helper.txtToCSV(Helper.ressourcesPy + "results/meanWindLCEurope", Helper.ressourcesPy + "windEurope.csv")

    val wind = new ERA40Wind(false)
    println("Grid Size : " + wind.grids.size + "=" + wind.grids.map(_.cellSize).sum + " km2")
    println("Min Latitude :" + wind.grids.map(_.center.latitude).min)
    println("Max Latitude :" + wind.grids.map(_.center.latitude).max)
    println("Min Longitude :" + wind.grids.map(_.center.longitude).min)
    println("Max Longitude :" + wind.grids.map(_.center.longitude).max)
    val openSpaces = wind.grids.filter(g => g.lc.code >= 26 && g.lc.code <= 34)
    val size = openSpaces.map(_.cellSize).sum
    println("OpenSpaces -> " + openSpaces.size + "\t" + size + " km2 --- " + "\t" + 5 * 2 * size + " MW")
    PlotHelper.cumulativeDensity(List((wind.windSpeeds, "10 metres"), (wind.windSpeeds80, "80 metres")), xLabel = "% of Sites", yLabel = "Mean Wind Speed [m/s]")
    PlotHelper.cumulativeDensity(List((wind.windSpeedsLand, "10 metres"), (wind.windSpeedsLand80, "80 metres")), xLabel = "% of Sites", yLabel = "Mean Wind Speed [m/s]", title="Water Bodies Excluded")
    // PlotHelper.cumulativeDensity(wind.grids.map(_.powerDensity(80)), 100, "80 meters mean speed")

  }
}

class ERA40Wind(val europe: Boolean) {
  // Coefficients for wind extrapolation depends on Land Cover class
  val lcClasses = if (europe) new CorineLandCoverClasses() else new GlobalLandCoverClasses()

  val grids: List[GridObject] = {
    val lines = Source.fromFile(Helper.ressourcesPy + "/results/meanWindLC" + (if (europe) "Europe" else "World")).getLines().toList
    lines.map(l => GridObject(l, lcClasses)).toList
  }
  val noWaterGrids = grids.filter(g => !lcClasses.waterIndexes.contains(g.lc.code))
  println(grids.size + "\t" + noWaterGrids.size)
  val windSpeeds = grids.map(_.windSpeed)
  val windSpeeds80 = grids.map(_.windSpeed80)
  val windSpeedsLand = noWaterGrids.map(_.windSpeed)
  val windSpeedsLand80 = noWaterGrids.map(_.windSpeed80)
  
  def writeGrid(name: String) {
    val writer = new CSVWriter(new FileWriter(name))
    writer.writeNext(Array("LATITUDE", "LONGITUDE", "WIND_SPEED"))
    grids.map(g => {
      writer.writeNext(Array(g.center.latitude.toString, g.center.longitude.toString, g.windSpeed.toString))
    })
    writer.close()
  }
}

/**
 * From data of ERA-40 dataset
 *
 */
class GridObject(val uWind: Double, val vWind: Double, val windSpeed: Double,
    val center: GeoPoint, val lc: LandCoverClass) {

  def windSpeed(h: Double, z0: Double): Double = windSpeed * math.log(h / z0) / math.log(10 / z0)
  val windSpeed80: Double = lc.hubHeigthConversionRatio * windSpeed

  val powerDensity: Double = 0.5 * 1.225 * Math.pow(windSpeed, 3)
  def powerDensity80: Double = 0.5 * 1.225 * Math.pow(windSpeed80, 3)
  /**
   * Calculate the cell size in km^2
   * Lat,Lon represent the center of the cell
   * =>  ___________  lat+0125/2
   *    |						|
   *    |						|
   *    |			o 		|
   *    |						|
   *    |						|
   *     ___________ lat-0125/2
   * lon-0.125/2     lon+0.125/2
   *
   */
  val cellLength = 0.125; val s = cellLength / 2.0;
  val length = Helper.distance(GeoPoint(center.latitude - s, center.longitude - s), GeoPoint(center.latitude - s, center.longitude + s)) / 1000.0
  val height = Helper.distance(GeoPoint(center.latitude - s, center.longitude - s), GeoPoint(center.latitude + s, center.longitude - s)) / 1000.0
  val cellSize = length * height
}

object GridObject {
  def apply(line: String, lcClasses: LandCoverClasses[_]) = {
    val csvLine = line.split("\t")
    val lcClass = lcClasses(csvLine(5).toInt)
    new GridObject(csvLine(0).toDouble, csvLine(1).toDouble, csvLine(2).toDouble,
      GeoPoint(csvLine(3).toDouble, csvLine(4).toDouble), lcClass.asInstanceOf[LandCoverClass])
  }
}

