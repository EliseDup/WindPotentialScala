package landCover

import scala.io.Source
import utils.Helper
import java.io.FileWriter
import au.com.bytecode.opencsv.CSVWriter
import utils.GeoPoint
import java.io.PrintStream
import squants.motion._
import squants.mass._
import squants.radio._
import squants.time._
import squants.energy._
import squants.space._
import utils.TerawattHours
import utils.PlotHelper
import landCover._
import operation._
import construction.MultiplyingFactor
import construction.WindFarm
import construction.OffshoreWindFarm
import utils.Thermodynamics

class GridData(val name: String, val gridSize: Angle,
    val onshoreTurbine: WindTurbine, val offshoreTurbine: WindTurbine, val details: Boolean = false) {

  // Coefficients for wind extrapolation depends on Land Cover class
  val glcClasses = new GlobCoverClasses()
  val modis = new ModisCoverClasses()

  val grids: List[GridObject] = {
    val lines = Source.fromFile(Helper.resultsPy + name).getLines().toList
    lines.map(l => if (details) GridObject.applyDetails(l, this) else GridObject(l, this)).toList
  }
  val constrainedGrids = grids.filter(_.suitableArea)
  val onshoreGrids = grids.filter(_.onshore)
  val onshoreConstrainedGrids = onshoreGrids.filter(_.suitableArea)
  val offshoreGrids = grids.filter(_.offshore)
  val offshoreConstrainedGrids = offshoreGrids.filter(_.suitableArea)

  def windSpeeds(gr: List[GridObject] = grids, atHub: Boolean = false) = if (atHub) gr.map(_.windSpeedAtHub().value) else gr.map(_.windSpeed.value)
  def windSpeedsMonth(month: Int, gr: List[GridObject] = grids) = gr.map(_.windSpeedMonth(month).value)

  def powerDensities(gr: List[GridObject] = grids, atHub: Boolean = false) = if (atHub) gr.map(_.powerDensityAtHub().value) else gr.map(_.powerDensity.value)
  def energyGenerated(gr: List[GridObject] = grids) = gr.map(_.energyGeneratedPerYear).foldLeft(TerawattHours(0.0))(_ + _)
  def nTurbines(gr: List[GridObject] = grids) = gr.map(_.nTurbines).sum
  def erois(gr: List[GridObject] = grids) = gr.map(_.EROI)
  def area(gr: List[GridObject] = grids) = gr.map(_.area).foldLeft(SquareKilometers(0))(_ + _)

  def plotEROIVSCumulatedProduction(gr: List[GridObject] = grids) = {
    val eroiPro = gr.map(g => (g.EROI, g.energyGeneratedPerYear)).sortBy(_._1).reverse
    var tot = 0.0
    val eroiCum = eroiPro.map(i => {
      tot = tot + i._2.to(TerawattHours)
      (i._1, tot)
    })
    PlotHelper.plotXY(List((eroiCum.map(_._2), eroiCum.map(_._1), "")), xLabel = "Cumulated Annual Production [TWh]", yLabel = "EROI")
  }

  def writeGrid(name: String, gr: List[GridObject] = grids) {
    val out_stream = new PrintStream(new java.io.FileOutputStream(name))
    gr.map(g => {
      out_stream.print(g.center.latitude.value.toString + "\t" + g.center.longitude.value.toString +
        "\t" + g.uWind.value.toString + "\t" + g.vWind.value.toString +
        "\t" + g.windSpeed.value.toString + "\t" + g.windSpeedAtHub().value.toString +
        "\t" + g.lc.code.toDouble.toString +
        "\t" + g.elevation.value.toString +
        "\t" + g.distanceToCoast.value.toString +
        "\t" + g.loadHours.value.toString +
        "\t" + g.urbanFactor.toString +
        "\t" + (if (g.suitableArea) "1.0" else "0.0") + "\n")
    })
    out_stream.close()
  }
}

/**
 *
 * Mean wind speed calculated from data of ERA-40 dataset
 *
 * Land cover classes used : corine land cover when available
 *
 * Otherwise GlobCover2009, and 0.5 km MODIS-based Global Land Cover Climatology when no modis was available
 *
 * SEE http://www.globalwindatlas.com/datasets.html
 *
 */
class GridObjectDetails(center: GeoPoint, gridSize: Angle, turbine: WindTurbine,
  windSpeeds: List[Velocity],
  lc: LandCoverClass,
  elevation: Length, distanceToCoast: Length, urbanFactor: Double)
    extends GridObject(center, gridSize, turbine, MetersPerSecond(0), MetersPerSecond(0),
      windSpeeds.foldLeft(MetersPerSecond(0))(_ + _) / windSpeeds.size, lc, elevation, distanceToCoast, urbanFactor) {

  override def windSpeedMonth(month: Int) = windSpeeds(month)

}

class GridObject(val center: GeoPoint, val gridSize: Angle, val turbine: WindTurbine,
    val uWind: Velocity, val vWind: Velocity, val windSpeed: Velocity,
    val lc: LandCoverClass,
    val elevation: Length, val distanceToCoast: Length,
    val urbanFactor: Double) {

  override def toString() = "Grid Object center : " + center + ", mean wind speed : " + windSpeed + ", land cover : " + lc

  val onshore = elevation.value >= 0
  val offshore = elevation.value < 0
  val suitableArea = {
    val geo =
      if (onshore) elevation.toMeters <= 2000
      else elevation.toMeters >= -200
    geo & windSpeed.toMetersPerSecond >= 4
  }

  val h0 = Meters(10)

  def windSpeedAtHub(h: Length = turbine.specs.hubHeight): Velocity = {
    if (offshore) assert(h.toMeters == 90)
    else assert(h.toMeters == 80)
    Math.log(h.toMeters / lc.z0.toMeters) / Math.log(h0.toMeters / lc.z0.toMeters) * windSpeed
  }
  def windSpeedMonth(month: Int) = windSpeed

  // Air density evolve with pressure (i.e. altitude)
  // Here we take the assumption of 15° degrees celsius everywhere
  // density = P / RT
  // P ~ z 
  val hubAltitude = Meters(Math.max(0.0, elevation.toMeters) + turbine.specs.hubHeight.toMeters)
  val powerDensity = Thermodynamics.powerDensity(windSpeed, hubAltitude)
  def powerDensityAtHub(h: Length = turbine.specs.hubHeight) = Thermodynamics.powerDensity(windSpeedAtHub(h), hubAltitude)

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
  val meanLonDistance = (Helper.distance(GeoPoint(center.latitude - s, center.longitude - s), GeoPoint(center.latitude - s, center.longitude + s)) +
    Helper.distance(GeoPoint(center.latitude + s, center.longitude - s), GeoPoint(center.latitude + s, center.longitude + s))) / 2.0
  val meanLatDistance = (Helper.distance(GeoPoint(center.latitude - s, center.longitude - s), GeoPoint(center.latitude + s, center.longitude - s)) +
    Helper.distance(GeoPoint(center.latitude - s, center.longitude + s), GeoPoint(center.latitude + s, center.longitude + s))) / 2.0

  /**
   * Regarding average wind energy production potential per square kilometre,
   * it is considered that five 2 MW wind turbines can be sited per square kilometre onshore.
   *
   * And the load hours according to the rule : y = 626,38x – 2003,3
   *
   * => Result in Wh
   */

  val loadHours =
    if (!suitableArea) Hours(0)
    // EU REPORT
    // else Hours(Math.max(0, Math.min(5500, 626.51 * windSpeedAtHub().value - 1901)))
    // HOOGWIJK REPORT
    else Hours(Math.max(0, Math.min(4000, 565 * windSpeedAtHub().value - 1745)))

  val nTurbines =
    if (loadHours.value <= 0) 0
    else area.toSquareKilometers * (1.0 - urbanFactor) * 2
  // else WakeEffect.nTurbines(area, turbine.specs.diameter) 

  val powerInstalled = nTurbines * turbine.ratedPower

  if ((powerInstalled.toMegawatts / area.toSquareKilometers) > turbine.ratedPower.toMegawatts * 2 + 1) {
    println(powerInstalled.toMegawatts + "\t" + area.toSquareKilometers + "\t" + turbine.ratedPower)
  }
  val farm =
    if (offshore) new OffshoreWindFarm(distanceToCoast, turbine, powerInstalled)
    else new WindFarm(turbine, powerInstalled)

  val energyGeneratedPerYear: Energy = powerInstalled * loadHours * farm.arrrayFactor * farm.availabilityFactor

  val EROI = {
    if (nTurbines == 0) 0.0
    else {
      val out = turbine.lifeTime * energyGeneratedPerYear
      val in = farm.embodiedEnergy
      out / in

    }
  }
}

object GridObject {
  def apply(line: String, data: GridData) = {
    val l = line.split("\t")
    val turbine = if (l(9).toDouble < 0) data.offshoreTurbine else data.onshoreTurbine
    new GridObject(center(l), data.gridSize, turbine,
      velocity(l, 2), velocity(l, 3), velocity(l, 4), lcClass(l, data),
      Meters(l(9).toDouble), Kilometers(l(10).toDouble), l(8).toDouble / 225.0)
  }
  def applyDetails(line: String, data: GridData) = {
    val l = line.split("\t")
    val turbine = if (l(17).toDouble < 0) data.offshoreTurbine else data.onshoreTurbine
    val speeds = (2 until 2 + 12).map(i => velocity(l, i)).toList
    new GridObjectDetails(center(l), data.gridSize, turbine,
      speeds, lcClass(l, data), Meters(l(18).toDouble), Kilometers(l(19).toDouble), l(17).toDouble / (15*15))
  }

  def velocity(line: Array[String], index: Int) = MetersPerSecond(line(index).toDouble)
  def center(line: Array[String]) = GeoPoint(Degrees(line(0).toDouble), Degrees(line(1).toDouble))

  // 5 = Corine, 6 = GlobCover, 7 = Modis
  def lcClass(line: Array[String], data: GridData) = {
    val corine = line(5); val globCover = line(6); val modis = line(7);
    /*if (!(corine.equals("NA") || data.clcClasses.noData.contains(corine.toInt))) data.clcClasses(corine.toInt)
    else */
    if (!(globCover.equals("NA") || data.glcClasses.noData.contains(globCover.toInt))) data.glcClasses(globCover.toInt)
    else data.modis(modis.toInt)

  }
}

