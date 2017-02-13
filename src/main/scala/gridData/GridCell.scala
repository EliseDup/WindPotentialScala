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
  val area = Helper.areaRectangle(center, gridSize)
  val meanLonDistance = (Helper.distance(GeoPoint(center.latitude - s, center.longitude - s), GeoPoint(center.latitude - s, center.longitude + s)) +
    Helper.distance(GeoPoint(center.latitude + s, center.longitude - s), GeoPoint(center.latitude + s, center.longitude + s))) / 2.0
  val meanLatDistance = (Helper.distance(GeoPoint(center.latitude - s, center.longitude - s), GeoPoint(center.latitude + s, center.longitude - s)) +
    Helper.distance(GeoPoint(center.latitude - s, center.longitude + s), GeoPoint(center.latitude + s, center.longitude + s))) / 2.0

}

class GridCellSolar(center: GeoPoint, gridSize: Angle, lc: LandCoverClass, urbanFactor: Double,
    protectedArea: Boolean, country: Country, val irradiance: MeteoData[Irradiance]) extends DefaultGridCell(center, gridSize, lc, urbanFactor, protectedArea, country) {
  def pvPotential = Watts(irradiance.mean.toWattsPerSquareMeter) * Hours(8760.0)
}

object GridCellSolar {
  def apply(l: Array[String], gridSize: Angle) = {
    new GridCellSolar(GeoPoint(Degrees(l(0).toDouble), Degrees(l(1).toDouble)),
      gridSize, LandCover.landCover(l(2), l(3), l(4)), l(5).toDouble / 225.0, l(6).toInt == 1,
      Country(l(7)),
       new MeteoData[Irradiance](WattsPerSquareMeter(l(8).toDouble), (0 until 12).map(i => WattsPerSquareMeter(if(l.size > 8+i) l(8+i).toDouble else 0.0)).toArray))
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
    val elevation: Length, 
    val distanceToCoast: Length,
    val tau : Pressure) extends DefaultGridCell(center, gridSize, lc, urbanFactor, protectedArea, country) {

  override def toString() = "Grid Object center : " + center + ", mean wind speed : " + windSpeed + ", land cover : " + lc

  val onshore = elevation.value >= 0
  val offshore = elevation.value < 0
  def EROI(potential: EnergyGenerationPotential) = potential.EROI(this)
  
  def dissipation : Irradiance = WattsPerSquareMeter(tau.toPascals * windSpeed.toMetersPerSecond)
 
  val h0 = Meters(100)
  
  def windSpeedAt(height: Length): Velocity = Math.log(height / lc.z0) / Math.log(h0 / lc.z0) * windSpeed
  
  /**
   * Shape parameter k[-] and scale parameters c [m/s]
   * of the Weibull distribution
   */
  val weibull = WeibullParameters(windSpeed, windSpeedStandardDeviation, if (onshore) Meters(80) else Meters(90), lc.z0)
  
  def altitude = if(offshore) Meters(0) else elevation
  def waterDepth = if(onshore) Meters(0) else -elevation
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
      Meters(l(8).toDouble), Kilometers(l(9).toDouble),
      if(l.size > 16) Pascals(l(16).toDouble) else Pascals(0))
  }

  def velocity(line: Array[String], index: Int) = MetersPerSecond(line(index).toDouble)
  def center(line: Array[String]) = GeoPoint(Degrees(line(0).toDouble), Degrees(line(1).toDouble))
  /**
   * CSV LINE :
   * [0] Latitude, [1] Longitude, [2] U wind, [3] V wind, [4] Mean Speed, [5] SUM ( speed time i - mean speed)^2, 
   * [6] # Observations, [7] Standard deviation (= SQRT(5/6))
   * [8] Corine Land Cover, [9] Modis Land Cover, [10] GLobCover, [11] # Urban Cells / (15*15), [12] Elevation, [13] Distance to nearest coast, [14] Protected Area
   * [15] Country Name
   */
  def applyNew(l: Array[String], gridSize: Angle) = {
    new GridCell(l, center(l), gridSize,
        LandCover.landCover(l(8), l(9), l(10)), l(11).toDouble / 225.0, l(14).toInt == 1, Country(l(15)),
        MetersPerSecond(4), l(7).toDouble,
        Meters(l(12).toDouble), Kilometers(l(13).toDouble),Pascals(0))
       
        
  }
}
