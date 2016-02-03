package landCover

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
import squants.space.Angle
import squants.energy.Watts
import squants.energy.WattHours
import utils.PlotHelper
import landCover._
import operation._
import squants.space.Length
import squants.space.Meters
import squants.space.Kilometers
import construction.MultiplyingFactor
import construction.WindFarm
import construction.OffshoreWindFarm
import squants.space.SquareKilometers

class GridData(val name: String, val gridSize: Angle,
    val onshoreTurbine: WindTurbine, val offshoreTurbine: WindTurbine, val details: Boolean = false) {

  // Coefficients for wind extrapolation depends on Land Cover class
  val clcClasses = new CorineLandCoverClasses()
  val glcClasses = new GlobCoverClasses()

  val grids: List[GridObject] = {
    val lines = Source.fromFile(Helper.ressourcesPy + name).getLines().toList
    lines.map(l => if (details) GridObject.applyDetails(l, this) else GridObject(l, this)).toList
  }
  val clcGrids = grids.filter(_.clc.isDefined)

  def windSpeeds(gr: List[GridObject] = grids, atHub: Boolean = false) = if (atHub) gr.map(_.windSpeedAtHub().value) else gr.map(_.windSpeed.value)
  def windSpeedsMonth(month : Int, gr: List[GridObject] = grids) = gr.map(_.windSpeedMonth(month).value)
 
  def powerDensities(gr: List[GridObject] = grids, atHub: Boolean = false) = if (atHub) gr.map(_.powerDensityAtHub().value) else gr.map(_.powerDensity.value)
  def energyGenerated(gr: List[GridObject] = grids, minEROI: Double = 0.0) = gr.map(_.energyGeneratedPerYear(minEROI = minEROI)).foldLeft(TerawattHours(0.0))(_ + _)
  def nTurbines(gr: List[GridObject] = grids) = gr.map(_.nTurbines).sum
  def erois(gr: List[GridObject] = grids) = gr.map(_.EROI)
  def area(gr: List[GridObject] = grids) = gr.map(_.area).foldLeft(SquareKilometers(0))(_ + _)

  def plotEROIVSCumulatedProduction(gr: List[GridObject] = grids) = {
    val eroiPro = gr.map(g => (g.EROI, g.energyGeneratedPerYear())).sortBy(_._1).reverse
    var tot = 0.0
    val eroiCum = eroiPro.map(i => {
      tot = tot + i._2.to(TerawattHours)
      (i._1, tot)
    })
    PlotHelper.plotXY(List((eroiCum.map(_._2), eroiCum.map(_._1), "")), xLabel = "Cumulated Annual Production [TWh]", yLabel = "EROI")
  }

  def landGrids(gr: List[GridObject] = grids) = gr.filter(g => !g.water)
  def agriculturalAreas(gr: List[GridObject] = grids) = gr.filter(_.lc.isAgriculturalArea)
  // The potential area for offshore wind energy generation is limited to sea deptgs less than 50 m
  def offshoreGrids(gr: List[GridObject] = grids) = gr.filter(g => g.seaLevel <= Meters(0) && g.seaLevel >= Meters(-50))
  val res = offshoreGrids()

  // println("Total n grids" + grids.size + " - clc grids " + clcGrids.size + "("+area(clcGrids)+")")
  // println("Total Energy Generated : " + energyGenerated(clcGrids) + "\t" + nTurbines(clcGrids))
  println(name + " - Energy Generated No Water: " + energyGenerated(landGrids(clcGrids)) + "\t" + nTurbines(landGrids(clcGrids)) + "(" + area(landGrids(clcGrids)) + ")")
  // println("Energy Generated Offshore: " + energyGenerated(offshoreGrids(clcGrids)) + "\t" + nTurbines(offshoreGrids(clcGrids)))

  def writeGridToCSV(name: String, gr: List[GridObject] = grids) {
    val writer = new CSVWriter(new FileWriter(name))
    writer.writeNext(Array("LATITUDE", "LONGITUDE", "WIND_SPEED", "WIND_SPEED_AT_HUB"))
    gr.map(g => {
      writer.writeNext(Array(g.center.latitude.toString, g.center.longitude.toString, g.windSpeed.toString, g.windSpeedAtHub().value.toString))
    })
    writer.close()
  }
  def writeGrid(name: String, gr: List[GridObject] = grids) {
    val out_stream = new PrintStream(new java.io.FileOutputStream(name))
    gr.map(g => {
      out_stream.print(g.center.latitude.value.toString + "\t" + g.center.longitude.value.toString +
        "\t" + g.uWind.value.toString + "\t" + g.vWind.value.toString +
        "\t" + g.windSpeed.value.toString + "\t" + g.windSpeedAtHub().value.toString +
        "\t" + g.clcCode.toDouble.toString + "\t" + g.glcCode.toDouble.toString +
        "\t" + g.seaLevel.value.toString +
        "\t" + g.distanceToCoast.value.toString +
        "\t" + g.loadHours().value.toString + "\n")
    })
    out_stream.close()
  }
}

/**
 * From data of ERA-40 dataset
 *
 */
class GridObjectDetails(center: GeoPoint, gridSize: Angle, turbine: WindTurbine,
  windSpeeds: List[Velocity],
  clc: Option[CorineLandCoverClass], glc: Option[GlobCoverClass],
  seaLevel: Length, distanceToCoast: Length)
    extends GridObject(center, gridSize, turbine, MetersPerSecond(0), MetersPerSecond(0),
      windSpeeds.foldLeft(MetersPerSecond(0))(_ + _) / windSpeeds.size, clc, glc, seaLevel, distanceToCoast){
  
  override def windSpeedMonth(month : Int) = windSpeeds(month)
  
}

class GridObject(val center: GeoPoint, val gridSize: Angle, val turbine: WindTurbine,
    val uWind: Velocity, val vWind: Velocity, val windSpeed: Velocity,
    val clc: Option[CorineLandCoverClass], val glc: Option[GlobCoverClass],
    val seaLevel: Length, val distanceToCoast: Length) {

  val water = seaLevel.value < 0
  // Only area up to 200 m depth are suitable for offshore wind farm !
  val offshoreArea = water && -seaLevel.toMeters <= 200

  /**
   *  Altitude < 2000 meters
   *
   */
  val suitableArea = (offshoreArea || !water) && seaLevel.toMeters <= 2000
  
  val lc: LandCoverClass = if (clc.isDefined) clc.get else glc.get
  val clcCode: Int = if (clc.isDefined) clc.get.code else -1
  val glcCode: Int = if (glc.isDefined) glc.get.code else -1
  val h0 = Meters(10)
  def windSpeedAtHub(h: Length = turbine.specs.hubHeight): Velocity = {
    Math.log(h.toMeters / lc.z0.toMeters) / Math.log(h0.toMeters / lc.z0.toMeters) * windSpeed
  }
  def windSpeedMonth(month : Int) = windSpeed
  
  val airDensity = KilogramsPerCubicMeter(1.225)
  val powerDensity = WattsPerSquareMeter(0.5 * airDensity.value * Math.pow(windSpeed.value, 3))
  def powerDensityAtHub(h: Length = turbine.specs.hubHeight) = WattsPerSquareMeter(0.5 * airDensity.value * Math.pow(windSpeedAtHub(h).value, 3))

  /**
   * Calculate the cell size in km^2
   * Lat,Lon represent the center of the cell
   * =>
   * lat+0125/2	 ___________  lat+0125/2
   * lon-0.125/2|						| lon+0.125/2
   *    				|						|
   *    				|			o 		|
   *    				|						|
   *            |						|
   *             ___________
   * lat-0125/2							 lat-0125/2
   * lon-0.125/2				     lon+0.125/2
   *
   */

  val s = gridSize / 2.0;
  val lowerLeftCorner = GeoPoint(center.latitude - s, center.longitude - s)
  val upperRightCorner = GeoPoint(center.latitude + s, center.longitude + s)
  val area = Helper.areaRectangle(lowerLeftCorner, upperRightCorner)
  val minLatDistance = List(Helper.distance(GeoPoint(center.latitude + s, center.longitude - s), GeoPoint(center.latitude + s, center.longitude + s)),
    Helper.distance(GeoPoint(center.latitude - s, center.longitude - s), GeoPoint(center.latitude - s, center.longitude + s))).minBy(_.value)
  val minLonDisance = List(Helper.distance(GeoPoint(center.latitude - s, center.longitude - s), GeoPoint(center.latitude - s, center.longitude + s)),
    Helper.distance(GeoPoint(center.latitude + s, center.longitude + s), GeoPoint(center.latitude + s, center.longitude - s))).minBy(_.value)

  /**
   * Regarding average wind energy production potential per square kilometre,
   * it is considered that five 2 MW wind turbines can be sited per square kilometre onshore.
   *
   * And the load hours according to the rule : y = 626,38x – 2003,3
   *
   * => Result in Wh
   */

  def loadHours(h: Length = turbine.specs.hubHeight, minSpeed: Velocity = MetersPerSecond(4)) = {
    if (windSpeedAtHub(h) < minSpeed || !suitableArea) Hours(0)
    else Hours(Math.max(0, 626.38 * windSpeedAtHub(h).value - 2003.3))
  }
  val nTurbines = if (loadHours().value <= 0) 0
  else WakeEffect.nTurbines(area, turbine.specs.diameter) //area.toSquareKilometers * turbine.nPerSquareKM

  val powerInstalled = nTurbines * turbine.ratedPower

  val farm = if (water) new OffshoreWindFarm(distanceToCoast, turbine, powerInstalled)
  else new WindFarm(turbine, powerInstalled)

  def energyGeneratedPerYear(minEROI: Double = 0.0, h: Length = turbine.specs.hubHeight): Energy = {
    if (EROI < minEROI) WattHours(0)
    else powerInstalled * loadHours(h)
  }

  val factor = if (water && -seaLevel.value <= 50) MultiplyingFactor.factor(-seaLevel.toMeters, distanceToCoast.toKilometers)
  else 1.0

  val EROI = {
    if (nTurbines == 0) 0.0
    else {
      val out = nTurbines * turbine.lifeTime * turbine.ratedPower * loadHours() * WakeEffect.wakeEffect(nTurbines)
      val in = farm.embodiedEnergy // nTurbines * turbine.specs.embodiedEnergy * factor //+ nFarms * farm.embodiedEnergy
      //   println(out.toGigajoules + "\t" + in.toGigajoules + "\t" + (out / in))
      out / in
    }
  }
}

object GridObject {
  def apply(line: String, data: GridData) = {
    val l = line.split("\t")
    val turbine = if (l(8).toDouble < 0) data.offshoreTurbine else data.onshoreTurbine
    new GridObject(center(l), data.gridSize, turbine,
      velocity(l, 2), velocity(l, 3), velocity(l, 4), clcClass(l, 6, data), glcClass(l, 7, data),
      Meters(l(8).toDouble), Kilometers(l(9).toDouble))
  }
  def applyDetails(line: String, data: GridData) = {
    val l = line.split("\t")
    val turbine = if (l(17).toDouble < 0) data.offshoreTurbine else data.onshoreTurbine
    val speeds = (2 until 2 + 12).map(i => velocity(l, i)).toList
    new GridObjectDetails(center(l), data.gridSize, turbine,
      speeds, clcClass(l, 14, data), glcClass(l, 16, data), Meters(l(17).toDouble), Kilometers(l(18).toDouble))
  }

  def velocity(line: Array[String], index: Int) = MetersPerSecond(line(index).toDouble)
  def center(line: Array[String]) = GeoPoint(Degrees(line(0).toDouble), Degrees(line(1).toDouble))
  def clcClass(line: Array[String], index: Int, data: GridData) = if (line(index).equals("-1.0") || line(index).equals("NA") || line(index).toDouble.toInt == 0) None else Some(data.clcClasses(line(index).toDouble.toInt))
  def glcClass(line: Array[String], index: Int, data: GridData) = if (line(index).equals("-1.0") || line(index).equals("NA")) None else Some(data.glcClasses(line(index).toDouble.toInt))
  def lcClass(clc: Option[CorineLandCoverClass], glc: Option[CorineLandCoverClass]) = if (clc.isDefined) clc.get else glc.get
}

