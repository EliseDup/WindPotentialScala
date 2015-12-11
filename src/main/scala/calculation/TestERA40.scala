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
    // Helper.txtToCSV(Helper.ressourcesPy + "windCLCEurope", Helper.ressourcesPy + "windEurope.csv")
    val wind = new ERA40Wind()
    
    println("Grid Size : " + wind.grids.size + "=" + wind.grids.map(_.cellSize).sum + " km2")
    println("Min Latitude :" + wind.grids.map(_.center.latitude).min)
    println("Max Latitude :" + wind.grids.map(_.center.latitude).max)
    println("Min Longitude :" + wind.grids.map(_.center.longitude).min)
    println("Max Longitude :" + wind.grids.map(_.center.longitude).max)
    val openSpaces = wind.grids.filter(g => g.lc.code >= 26 && g.lc.code <= 34)
    val size = openSpaces.map(_.cellSize).sum
     println("OpenSpaces -> " +  openSpaces.size + "\t" + size  + " km2 --- " + "\t" + 5*2*size + " MW" )
    PlotHelper.cumulativeDensity(wind.grids.map(_.windSpeed), 100, "10 meters mean speed")
    PlotHelper.cumulativeDensity(wind.grids.map(_.windSpeed(80)), 100, "80 meters mean speed")

  }
}

class ERA40Wind {
  // Coefficients for wind extrapolation depends on CLC class
  val clcClasses = new CorineLandCoverClasses()

  val grids: List[GridObject] = {
    val lines = Source.fromFile(Helper.ressourcesPy + "/results/meanWindLCEurope").getLines().toList
    lines.map(l => GridObject(l, clcClasses)).toList
  }
}

/**
 * From data of ERA-40 dataset
 *
 */
class GridObject(val uWind: Double, val vWind: Double, val windSpeed: Double,
    val center: GeoPoint, val lc : LandCoverClass) {

  def windSpeed(h: Double, z0: Double): Double = windSpeed * math.log(h / z0) / math.log(10 / z0)
  def windSpeed(h: Double): Double = lc.hubHeigthConversionRatio * windSpeed
  
  val powerDensity: Double = 0.5*1.225*Math.pow(windSpeed,3)
  def powerDensity(h :Double): Double = 0.5*1.225*Math.pow(windSpeed(h),3)
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
  def apply(line: String, clcClasses: CorineLandCoverClasses) = {
    val csvLine = line.split("\t")
    val clcClass = if (csvLine(5).equals("NA")) clcClasses(50) else clcClasses(csvLine(5).toInt)
    new GridObject(csvLine(0).toDouble, csvLine(1).toDouble, csvLine(2).toDouble,
      GeoPoint(csvLine(3).toDouble, csvLine(4).toDouble), clcClass)
  }
}

