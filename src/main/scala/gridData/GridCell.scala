package gridData

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
import windEnergy.WeibullParameters
import squants.UnitOfMeasure
import squants.Quantity

case class Country(val name: String)

class MeteoData[A <: Quantity[_]](val mean : A, val perMonth : Array[A])

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

class DefaultGridCell(val center: GeoPoint, val gridSize: Angle, val lc: LandCoverClass,
    val urbanFactor: Double, val protectedArea: Boolean, val country: Country) {

  val s = gridSize / 2.0;
  val lowerLeftCorner = GeoPoint(center.latitude - s, center.longitude - s)
  val upperRightCorner = GeoPoint(center.latitude + s, center.longitude + s)
  val area = Helper.areaRectangle(lowerLeftCorner, upperRightCorner)
  val meanLonDistance = (Helper.distance(GeoPoint(center.latitude - s, center.longitude - s), GeoPoint(center.latitude - s, center.longitude + s)) +
    Helper.distance(GeoPoint(center.latitude + s, center.longitude - s), GeoPoint(center.latitude + s, center.longitude + s))) / 2.0
  val meanLatDistance = (Helper.distance(GeoPoint(center.latitude - s, center.longitude - s), GeoPoint(center.latitude + s, center.longitude - s)) +
    Helper.distance(GeoPoint(center.latitude - s, center.longitude + s), GeoPoint(center.latitude + s, center.longitude + s))) / 2.0

}

class GridCellSolar(center: GeoPoint, gridSize: Angle, lc: LandCoverClass, urbanFactor: Double,
    protectedArea: Boolean, country: Country, val irradiance: Irradiance) extends DefaultGridCell(center, gridSize, lc, urbanFactor, protectedArea, country) {
  def pvPotential = Watts(irradiance.toWattsPerSquareMeter) * Hours(8760.0)
}
object GridCellSolar {
  def apply(l: Array[String], gridSize: Angle) = {
    new GridCellSolar(GeoPoint(Degrees(l(0).toDouble), Degrees(l(1).toDouble)),
      gridSize, LandCover.landCover(l(2), l(3), l(4)), l(5).toDouble / 225.0, l(6).toInt == 1,
      Country(l(7)),
      WattsPerSquareMeter(l(8).toDouble))
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
 *
 */

class GridCell(val csvLine: Array[String], center: GeoPoint, gridSize: Angle, lc: LandCoverClass, urbanFactor: Double,
    protectedArea: Boolean, country: Country,
    val windSpeed: Velocity,
    val windSpeedStandardDeviation: Double,
    val kineticEnergyDissipation: Irradiance,
    val irradiance : MeteoData[Irradiance],
    val elevation: Length, val distanceToCoast: Length) extends DefaultGridCell(center, gridSize, lc, urbanFactor, protectedArea, country) {

  override def toString() = "Grid Object center : " + center + ", mean wind speed : " + windSpeed + ", land cover : " + lc

  val onshore = elevation.value >= 0
  val offshore = elevation.value < 0
  def EROI(potential: EnergyGenerationPotential) = potential.EROI(this)

  val effectiveArea = area * (1.0 - urbanFactor)

  val h0 = Meters(10)
  val hubHeight = if (onshore) Meters(80) else Meters(90)

  def windSpeedAt(height: Length): Velocity = Math.log(height / lc.z0) / Math.log(h0 / lc.z0) * windSpeed
  val windSpeedHub = windSpeedAt(hubHeight)
  /**
   * Shape parameter k[-] and scale parameters c [m/s]
   * of the Weibull distribution
   */
  val weibull = WeibullParameters(windSpeed, windSpeedStandardDeviation, if (onshore) Meters(80) else Meters(90))
}
/**
 * Latitude	Longitude	Corine Land Cover	GlobCover	Modis	Urban Factor	Protected area ?	Country	Elevation [m]	Distance to Coast [km]	Uwind	Vwind	Wind	Std Wind	KineticEnergyDissipation Irradiance
 *
 */
object GridCell {
  def apply(l: Array[String], gridSize: Angle) = {
    new GridCell(l, center(l), gridSize, LandCover.landCover(l(2), l(3), l(4)), l(5).toDouble / 225.0, l(6).toInt == 1,
      Country(l(7)),
      velocity(l, 12), l(13).toDouble,
      WattsPerSquareMeter(l(14).toDouble),
      new MeteoData[Irradiance](WattsPerSquareMeter(l(15).toDouble), (0 until 12).map(i => WattsPerSquareMeter(if(l.size > 16+i) l(16+i).toDouble else 0.0)).toArray),
      Meters(l(8).toDouble), Kilometers(l(9).toDouble))
  }

  def velocity(line: Array[String], index: Int) = MetersPerSecond(line(index).toDouble)
  def center(line: Array[String]) = GeoPoint(Degrees(line(0).toDouble), Degrees(line(1).toDouble))

}
