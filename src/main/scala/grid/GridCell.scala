package grid

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
import construction._
import utils._
import org.apache.commons.math3.special.Gamma
import wind_energy.WeibullParameters
import squants.UnitOfMeasure
import squants.Quantity

case class Country(val name: String)
/**
 * Class to model observations with a global mean and a mean per month (useful for solar irradiance !)
 */
class MeteoData[A <: Quantity[_]](val mean: A, val perMonth: Array[A])

/**
 * Basic object of our model : A grid cell with detailed information about land cover use, country etc
 *
 * Land Covers : List ( % cell's area , globcover land cover index )
 * Calculate the cell size in km^2
 * Lat,Lon represent the center of the cell
 * =>
 * lat+resolution/2	 ___________  lat+resolution/2
 * lon-resolution/2|						| lon+resolution/2
 *    						 |						|
 *    						 |			o 		|
 *    						 |						|
 *            		 |						|
 *            		   ___________
 * lat-resolution/2						 lat-resolution/2
 * lon-resolution/2				     lon+resolution/2
 *
 */
class DefaultGridCell(val center: GeoPoint, val gridSize: Angle, val landCovers: DetailedLandCover, val protectedArea: Boolean, val country: Country, val elevation: Length) {

  val s = gridSize / 2.0;

  val area = Helper.areaRectangle(center, gridSize)
  val meanLonDistance = (Helper.distance(GeoPoint(center.latitude - s, center.longitude - s), GeoPoint(center.latitude - s, center.longitude + s)) +
    Helper.distance(GeoPoint(center.latitude + s, center.longitude - s), GeoPoint(center.latitude + s, center.longitude + s))) / 2.0
  val meanLatDistance = (Helper.distance(GeoPoint(center.latitude - s, center.longitude - s), GeoPoint(center.latitude + s, center.longitude - s)) +
    Helper.distance(GeoPoint(center.latitude - s, center.longitude + s), GeoPoint(center.latitude + s, center.longitude + s))) / 2.0

  val onshore = elevation.value >= 0
  val offshore = elevation.value < 0
  val altitude = if (offshore) Meters(0) else elevation
  val waterDepth = if (onshore) Meters(0) else -elevation

  def area(lcType : LandCoverType) : Area = landCovers.types.filter(_._2.equals(lcType)).map(_._1).sum * area

}

/**
 *
 * Mean wind speed calculated from data of ERA-Interim dataset
 *
 * Land Cover from : GlobCover2009, and 0.5 km MODIS-based Global Land Cover Climatology when no GlobCover was available
 * Protected Area from world protected area dataset
 *
 * SEE http://www.globalwindatlas.com/datasets.html
 *
 */

class GridCell(val csvLine: Array[String], center: GeoPoint, gridSize: Angle,
    landCovers: DetailedLandCover,
    protectedArea: Boolean,
    country: Country,
    elevation: Length,
    val distanceToCoast: Length,

    val windSpeed: Velocity,
    val windSpeedStandardDeviation: Double,
    val optimalCD: Map[Double, (Boolean, Irradiance)],
    val tau: Pressure,
    val irradiance: MeteoData[Irradiance]) extends DefaultGridCell(center, gridSize, landCovers, protectedArea, country, elevation) {

  override def toString() = "Grid Object center : " + center + ", mean wind speed : " + windSpeed

  def EROI(potential: EnergyGenerationPotential) = potential.EROI(this)

  def dissipation: Irradiance = WattsPerSquareMeter(tau.toPascals * windSpeed.toMetersPerSecond)
  def estimatedDissipation(world: WorldGrid) =
    if (area.toSquareKilometers == 0) WattsPerSquareMeter(0)
    else world.totalDissipation / area * (Math.pow(windSpeed.toMetersPerSecond, 2) * area.toSquareMeters / world.totalSquareSpeedArea)
  
  def suitableArea = WindPotential.suitabilityFactor(this) * area

  /**
   * Shape parameter k[-] and scale parameters c [m/s]
   * of the Weibull distribution
   */
  val weibull = WeibullParameters(windSpeed, windSpeedStandardDeviation)

  def getOptimalCD(e: Double) =
    if (!optimalCD.keySet.contains(e)) WattsPerSquareMeter(0)
    else if (!optimalCD(e)._1) WattsPerSquareMeter(0)
    else optimalCD(e)._2

}
/**
 * Latitude	Longitude	Corine Land Cover	GlobCover	Modis	Urban Factor	Protected area ?	Country	Elevation [m]	Distance to Coast [km]	Uwind	Vwind	Wind	Std Wind	KineticEnergyDissipation Irradiance
 *
 */

object GridCell {
  def velocity(line: Array[String], index: Int) = MetersPerSecond(line(index).toDouble)
  def center(line: Array[String]) = GeoPoint(Degrees(line(0).toDouble), Degrees(line(1).toDouble))
  /**
   * CSV LINE :
   * [0] Latitude, [1] Longitude,
   * [2-24] Detailed Land Covers,
   * [25] U wind, [26] V wind, [27] Mean Speed, [28] SUM ( speed time i - mean speed)^2,
   * [29] # Observations, [30] Standard deviation (= SQRT(28/29))
   * [31] Elevation, [32] Distance to nearest coast, [33] Protected Area
   * [34] Country Name
   */
  def apply(l: Array[String], gridSize: Angle) = {
    val indexes = Array(11, 14, 20, 30, 40, 50, 60, 70, 90, 100, 110, 120, 130, 140, 150, 160, 170, 180, 190, 200, 210, 220, 230)
    val sumLC = (0 until 23).map(i => l(i + 2).toDouble).sum
    val lcs = for (i <- (0 until 23); if (l(i + 2).toDouble > 0)) yield (l(i + 2).toDouble / sumLC, indexes(i))
    val i = 24
    new GridCell(l, center(l), gridSize,
      new DetailedLandCover(lcs.toList),
      l(i + 9).toInt == 1,
      Country(l(i + 10)),
      Meters(l(i + 7).toDouble), Kilometers(l(i + 8).toDouble),
      MetersPerSecond(l(i + 3).toDouble), l(i + 6).toDouble,
      if (l.size > 35) (for (e <- (0 until 40); if (l(e * 2 + 35).toDouble > 0)) yield (e * 0.5, (l(e * 2 + 36).toBoolean, WattsPerSquareMeter(l(e * 2 + 35).toDouble)))).toMap else Map(),
      Pascals(0), new MeteoData[Irradiance](WattsPerSquareMeter(0), (0 until 12).map(i => WattsPerSquareMeter(0.0)).toArray)) //TODO ?!

  }

  /*def apply(l: Array[String], gridSize: Angle) = {
    val lcs = (0 to 23).filter(i => l(i+2).toDouble > 0).map(i => (l(i+2).toDouble / 225.0, GlobCoverClasses.indexes(i))).toList
    val lc = LandCover.landCover(l(2), l(3), l(4))
    new GridCell(l, center(l), gridSize, new DetailedLandCover(lcs),
      l(6).toInt == 1,
      Country(l(7)),
      Meters(l(8).toDouble),
      Helper.windSpeedAt(velocity(l, 12), Meters(10), lc.z0, Meters(100)), l(13).toDouble,
      Kilometers(l(9).toDouble),
      if (l.size > 16) Pascals(l(16).toDouble) else Pascals(0),
      new MeteoData[Irradiance](WattsPerSquareMeter(l(16).toDouble), (0 until 12).map(i => WattsPerSquareMeter(if (l.size > 16 + i) l(16 + i).toDouble else 0.0)).toArray))
  }*/
}
