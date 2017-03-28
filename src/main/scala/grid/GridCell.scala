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
import wind_energy._
import squants.UnitOfMeasure
import squants.Quantity

case class Country(val name: String)
/**
 * Class to model observations with a global mean and a mean per month (useful for solar irradiance !)
 */
class MeteoData[A <: Quantity[_]](val mean: A, val perMonth: Array[A]) {
  def month(index: Int): A = {
    if (perMonth.isEmpty) mean else perMonth(index)
  }
}

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
class DefaultGridCell(val center: GeoPoint, val gridSize: Angle, val landCovers: DetailedLandCover, val protectedArea: Double, val country: Country, val elevation: Length) {

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

  def area(lcType: LandCoverType): Area = landCovers.types.filter(_._2.equals(lcType)).map(_._1).sum * area

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
    protectedArea: Double,
    country: Country,
    elevation: Length,
    val distanceToCoast: Length,
    val wind71m: WindProfile,
    val wind125m: WindProfile,
    val irradiance: MeteoData[Irradiance],
    val optimalCD: Map[Double, (Boolean, Irradiance)],
    val tau: Pressure) extends DefaultGridCell(center, gridSize, landCovers, protectedArea, country, elevation) {

  override def toString() = "Grid Object center : " + center
  
  val wind100m = new WindProfile( (wind71m.mean+wind125m.mean)/2.0, (wind71m.std+wind125m.std)/2.0, Meters(100))
 
  def EROI(potential: EnergyGenerationPotential) = potential.EROI(this)

  def estimatedDissipation(world: WorldGrid) =
    if (area.toSquareKilometers == 0) WattsPerSquareMeter(0)
    else world.totalDissipation / area * (Math.pow(wind125m.mean.toMetersPerSecond, 2) * area.toSquareMeters / world.totalSquareSpeedArea)

  def suitableArea = WindPotential.suitabilityFactor(this) * area

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
   * [2] Protected Areas,
   * [3-25] Detailed Land Covers,
   * [26] Elevation, [27] Distance to nearest coast, [28] Country Name
   * [29] Mean wind 125m , [30] Std wind 125 m,
   * [31] Mean wind 71m, [32] Std wind 71 m
   * [33] Wind turbulent shear stress
   * 
   * Then if the optimization was made :
   * [34] -> [116] Pair of (optimal installed capacity, boolean) for EROI 0 -> 20 by 0.5
   * 
   */
  def apply(l: Array[String], gridSize: Angle) = {
    val indexes = Array(11, 14, 20, 30, 40, 50, 60, 70, 90, 100, 110, 120, 130, 140, 150, 160, 170, 180, 190, 200, 210, 220, 230)
    val sumLC = (3 until 26).map(i => l(i).toDouble).sum
    val lcs = for (i <- (3 until 26); if (l(i).toDouble > 0)) yield (l(i).toDouble / sumLC, indexes(i-3))
    
    new GridCell(l, center(l), gridSize,
      new DetailedLandCover(lcs.toList),
      l(2).toDouble / 100.0,
      Country(l(28)),
      Meters(l(26).toDouble), Kilometers(l(27).toDouble),
      new WindProfile(MetersPerSecond(l(31).toDouble), l(32).toDouble,Meters(71)),
      new WindProfile(MetersPerSecond(l(29).toDouble), l(30).toDouble,Meters(125)),
      new MeteoData[Irradiance](WattsPerSquareMeter(0), Array()),
      if (l.size > 34) (for (e <- (0 until 40); if (l(e * 2 + 34).toDouble > 0)) yield (e * 0.5, (l(e * 2 + 35).toBoolean, WattsPerSquareMeter(l(e * 2 + 34).toDouble)))).toMap else Map(),
      Pascals(l(33).toDouble))
  }
}
