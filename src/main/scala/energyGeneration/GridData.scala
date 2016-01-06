package energyGeneration

import scala.io.Source
import utils.Helper
import java.io.FileWriter
import au.com.bytecode.opencsv.CSVWriter
import utils.GeoPoint
import java.io.PrintStream
import squants.motion.Velocity
import squants.mass.KilogramsPerCubicMeter
import squants.radio.WattsPerSquareMeter
import squants.time.Hours
import squants.energy.Megawatts
import squants.energy.Energy
import squants.energy.Terajoules
import squants.motion.MetersPerSecond
import squants.energy.KilowattHours
import squants.energy.GigawattHours
import utils.TerawattHours
import squants.space.Degrees

class GridData(name: String, val gridSize : Double) {
  // Coefficients for wind extrapolation depends on Land Cover class
  val clcClasses = new CorineLandCoverClasses()
  val glcClasses = new GlobalLandCoverClasses()

  val grids: List[GridObject] = {
    val lines = Source.fromFile(Helper.ressourcesPy + name).getLines().toList
    lines.map(l => GridObject(l, this)).toList
  }
  val clcGrids = grids.filter(_.clc.isDefined)

  def windSpeeds(gr: List[GridObject] = grids) = gr.map(_.windSpeed.value)
  def windSpeeds80(gr: List[GridObject] = grids) = gr.map(_.windSpeed80.value)
  def powerDensities(gr: List[GridObject] = grids) = gr.map(_.powerDensity.value)
  def powerDensities80(gr: List[GridObject] = grids) = gr.map(_.powerDensity80.value)
  def energyGenerated(gr: List[GridObject] = grids) = gr.map(_.energyGeneratedPerYearWith2MWTurbines()).foldLeft(TerawattHours(0.0))(_ + _)
  def nTurbines(gr: List[GridObject] = grids) = gr.map(_.nTurbines).sum
  def erois(gr: List[GridObject] = grids) = gr.map(_.EROI)
  
  def landGrids(gr: List[GridObject] = grids) = gr.filter(g => !g.lc.isInWater)
  def agriculturalAreas(gr: List[GridObject] = grids) = gr.filter(_.lc.isAgriculturalArea)
  def offshoreGrids(gr: List[GridObject] = grids) = gr.filter(g => g.lc.isOffshoreLess50km)
println("Total n grids" +grids.size +" - clc grids " + clcGrids.size)
  println("Total Energy Generated : " + energyGenerated() + "\t" + nTurbines(clcGrids))
  println("Energy Generated No Water: " + energyGenerated(landGrids(clcGrids)) + "\t" + nTurbines(landGrids(clcGrids)))
  println("Energy Generated Agriculture: " + energyGenerated(agriculturalAreas(clcGrids)) + "\t" + nTurbines(agriculturalAreas(clcGrids)))

  def writeGridToCSV(name: String, gr: List[GridObject] = grids) {
    val writer = new CSVWriter(new FileWriter(name))
    writer.writeNext(Array("LATITUDE", "LONGITUDE", "WIND_SPEED", "WIND_SPEED_80"))
    gr.map(g => {
      writer.writeNext(Array(g.center.latitude.toString, g.center.longitude.toString, g.windSpeed.toString, g.windSpeed80.toString))
    })
    writer.close()
  }
  def writeGrid(name: String, gr: List[GridObject] = grids) {
    val out_stream = new PrintStream(new java.io.FileOutputStream(name))
    gr.map(g => {
      out_stream.print(g.center.latitude.toString + "\t" + g.center.longitude.toString +
        "\t" + g.uWind.toString + "\t" + g.vWind.toString +
        "\t" + g.windSpeed.toString + "\t" + g.windSpeed80.toString +
        "\t" + g.clcCode.toDouble.toString + "\t" + g.glcCode.toDouble.toString +
        "\t" + g.loadHours.value.toString + "\n")
    })
    out_stream.close()
  }
}

/**
 * From data of ERA-40 dataset
 *
 */
class GridObject(val center: GeoPoint, val gridSize : Double, val uWind: Velocity,
    val vWind: Velocity, val windSpeed: Velocity,
    val clc: Option[CorineLandCoverClass], val glc: Option[GlobalLandCoverClass]) {

  // def windSpeed(h: Double, z0: Double): Measure[Velocity] = windSpeed * math.log(h / z0) / math.log(10 / z0)
  val lc: LandCoverClass = if (clc.isDefined) clc.get else glc.get
  val clcCode: Int = if (clc.isDefined) clc.get.code else -1
  val glcCode: Int = if (glc.isDefined) glc.get.code else -1

  val windSpeed80 = windSpeed * lc.hubHeigthConversionRatio
  val airDensity = KilogramsPerCubicMeter(1.225)
  val powerDensity = WattsPerSquareMeter(0.5 * airDensity.value * Math.pow(windSpeed.value, 3))
  val powerDensity80 = WattsPerSquareMeter(0.5 * airDensity.value * Math.pow(windSpeed80.value, 3))

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

  val s = Degrees(gridSize / 2.0);
  // val length = Helper.distance(GeoPoint(center.latitude - s, center.longitude - s), GeoPoint(center.latitude - s, center.longitude + s)) / 1000.0
  // val height = Helper.distance(GeoPoint(center.latitude - s, center.longitude - s), GeoPoint(center.latitude + s, center.longitude - s)) / 1000.0
  // val cellSize = length * height
  val lowerLeftCorner = GeoPoint(center.latitude - s, center.longitude - s)
  val upperRightCorner = GeoPoint(center.latitude + s, center.longitude + s)
  val area = Helper.areaRectangle(lowerLeftCorner, upperRightCorner)

  /**
   * Regarding average wind energy production potential per square kilometre,
   * it is considered that five 2 MW wind turbines can be sited per square kilometre onshore.
   *
   * And the load hours according to the rule : y = 626,38x – 2003,3
   *
   * => Result in Wh
   */
  val loadHours = Hours(Math.max(0, 626.38 * windSpeed80.value - 2003.3))
  val nTurbines = if (loadHours.value <= 0) 0 else area.toSquareKilometers * 4
  val powerInstalled = Megawatts(2 * nTurbines)
  def energyGeneratedPerYearWith2MWTurbines(minSpeed: Double = 0.0): Energy = powerInstalled * loadHours

  val EROI = {
    if (nTurbines == 0) 0.0
    else {
      val out = energyGeneratedPerYearWith2MWTurbines() * 20
      //12.9 TJ + 0.3 TJ
      val in = nTurbines * Terajoules(12.9 + 0.3)
      out / in
    }
  }
}

object GridObject {
  def apply(line: String, data: GridData) = {
    val csvLine = line.split("\t")
    val clcClass = if (csvLine(5).equals("NA") || csvLine(5).toInt == 0) None else Some(data.clcClasses(csvLine(5).toInt))
    val glcClass = if (csvLine(6).equals("NA")) None else Some(data.glcClasses(csvLine(6).toInt))
    new GridObject(GeoPoint(Degrees(csvLine(0).toDouble), Degrees(csvLine(1).toDouble)), data.gridSize,
      MetersPerSecond(csvLine(2).toDouble), MetersPerSecond(csvLine(3).toDouble), MetersPerSecond(csvLine(4).toDouble), clcClass, glcClass)
  }
}

