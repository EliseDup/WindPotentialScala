package calculation

import scala.io.Source
import utils.PlotHelper
import utils.Helper
import java.io.FileWriter
import au.com.bytecode.opencsv.CSVWriter
import utils.GeoPoint
import java.io.PrintStream

class GridData(name : String) {
  // Coefficients for wind extrapolation depends on Land Cover class
  val clcClasses = new CorineLandCoverClasses()
  val glcClasses = new GlobalLandCoverClasses()

  val grids: List[GridObject] = {
    val lines = Source.fromFile(Helper.ressourcesPy + name).getLines().toList
    lines.map(l => GridObject(l, this)).toList
  }

  val windSpeeds = grids.map(_.windSpeed)
  val windSpeeds80 = grids.map(_.windSpeed80)

  val noWaterGrids = grids.filter(g => !g.lc.isInWater)
  val agriculturalAreas = grids.filter(_.lc.isAgriculturalArea)

  val windSpeedsLand = noWaterGrids.map(_.windSpeed)
  val windSpeedsLand80 = noWaterGrids.map(_.windSpeed80)

  def energyGenerated(gr: List[GridObject]) = gr.map(_.energyGeneratedPerYearWith2MWTurbines(0.0)).sum
  def nTurbines(gr: List[GridObject]) = gr.map(_.nTurbines).sum

  println("Size =" + "\t" + grids.size + "\t" + grids.map(_.area).sum)
  println("No Water" + "\t" + noWaterGrids.size + "\t" + noWaterGrids.map(_.area).sum)
  println("AgriculturalAreas" + "\t" + agriculturalAreas.size + "\t" + agriculturalAreas.map(_.area).sum)

  println("Total Energy Generated : " + (energyGenerated(grids) / Math.pow(10, 12)) + "TWh" + "\t" + nTurbines(grids))
  println("Energy Generated No Water: " + (energyGenerated(noWaterGrids) / Math.pow(10, 12)) + "TWh" + "\t" + nTurbines(noWaterGrids))
  println("Energy Generated Agriculture: " + (energyGenerated(agriculturalAreas) / Math.pow(10, 12)) + "TWh" + "\t" + nTurbines(agriculturalAreas))

  def writeGridToCSV(name: String) {
    val writer = new CSVWriter(new FileWriter(name))
    writer.writeNext(Array("LATITUDE", "LONGITUDE", "WIND_SPEED", "WIND_SPEED_80"))
    grids.map(g => {
      writer.writeNext(Array(g.center.latitude.toString, g.center.longitude.toString, g.windSpeed.toString, g.windSpeed80.toString))
    })
    writer.close()
  }
  def writeGrid(name: String) {
    val out_stream = new PrintStream(new java.io.FileOutputStream(name))
    grids.map(g => {
      out_stream.print(g.center.latitude.toString + "\t" + g.center.longitude.toString + 
          "\t" + g.uWind.toString + "\t" + g.vWind.toString +
          "\t" + g.windSpeed.toString + "\t" + g.windSpeed80.toString + "\n")
    })
    out_stream.close()
  }
}

/**
 * From data of ERA-40 dataset
 *
 */
class GridObject(val uWind: Double, val vWind: Double, val windSpeed: Double,
    val center: GeoPoint, val clc: Option[CorineLandCoverClass], val glc: Option[GlobalLandCoverClass]) {

  def windSpeed(h: Double, z0: Double): Double = windSpeed * math.log(h / z0) / math.log(10 / z0)
  val lc = if (clc.isDefined) clc.get else glc.get

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
  // val length = Helper.distance(GeoPoint(center.latitude - s, center.longitude - s), GeoPoint(center.latitude - s, center.longitude + s)) / 1000.0
  // val height = Helper.distance(GeoPoint(center.latitude - s, center.longitude - s), GeoPoint(center.latitude + s, center.longitude - s)) / 1000.0
  // val cellSize = length * height
  val lowerLeftCorner = GeoPoint(center.latitude - s, center.longitude - s)
  val upperRightCorner = GeoPoint(center.latitude + s, center.longitude + s)
  val area = Helper.areaRectangle(lowerLeftCorner, upperRightCorner) / Math.pow(10, 6)
  val nTurbines = area * 5
  /**
   * Regarding average wind energy production potential per square kilometre,
   * it is considered that five 2 MW wind turbines can be sited per square kilometre onshore.
   *
   * And the load hours according to the rule : y = 626,38x – 2003,3
   *
   * => Result in Wh
   */
  def energyGeneratedPerYearWith2MWTurbines(minSpeed: Double): Double = {
    if (windSpeed80 < minSpeed) 0.0
    else {
      val loadHours = Math.max(0, 626.38 * windSpeed80 - 2003.3)
      nTurbines * 2 * Math.pow(10, 6) * loadHours
    }
  }
}

object GridObject {
  def apply(line: String, data: GridData) = {
    val csvLine = line.split("\t")
    val clcClass = if (csvLine(5).equals("NA")) None else Some(data.clcClasses(csvLine(5).toInt))
    val glcClass = if (csvLine(6).equals("NA")) None else Some(data.glcClasses(csvLine(6).toInt))
    new GridObject(csvLine(0).toDouble, csvLine(1).toDouble, csvLine(2).toDouble,
      GeoPoint(csvLine(3).toDouble, csvLine(4).toDouble), clcClass, glcClass)
  }
}

