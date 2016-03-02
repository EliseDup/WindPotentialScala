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

class GridCell(val center: GeoPoint, val gridSize: Angle,
    val windSpeed: Velocity,
    val irradiance: Irradiance,
    val lc: LandCoverClass,
    val elevation: Length, val distanceToCoast: Length,
    val urbanFactor: Double) {

  override def toString() = "Grid Object center : " + center + ", mean wind speed : " + windSpeed + ", land cover : " + lc

  val onshore = elevation.value >= 0
  val offshore = elevation.value < 0
  def EROI(potential: RenewablePotential) = potential.EROI(this)

  def windSpeedMonth(month: Int)  = windSpeed
  
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

  val effectiveArea = area * (1.0 - urbanFactor)

}

object GridCell {
  def apply(line: String, data: WorldGrid) = {
    val l = line.split("\t")
    new GridCell(center(l), data.gridSize, velocity(l, 4), WattsPerSquareMeter(0), lcClass(l, data), Meters(l(9).toDouble), Kilometers(l(10).toDouble), l(8).toDouble / 225.0)
  }
  def applyDetails(line: String, data: WorldGrid) = {
    val l = line.split("\t")
    val speeds = (2 until 2 + 12).map(i => velocity(l, i)).toList
    new GridCellPerMonth(center(l), data.gridSize,
      speeds, lcClass(l, data), Meters(l(18).toDouble), Kilometers(l(19).toDouble), l(17).toDouble / (15 * 15))
  }

  def velocity(line: Array[String], index: Int) = MetersPerSecond(line(index).toDouble)
  def center(line: Array[String]) = GeoPoint(Degrees(line(0).toDouble), Degrees(line(1).toDouble))

  // 5 = Corine, 6 = GlobCover, 7 = Modis
  def lcClass(line: Array[String], data: WorldGrid) = {
    val corine = line(5); val globCover = line(6); val modis = line(7);
    /*if (!(corine.equals("NA") || CorineCoverClasses.noData.contains(corine.toInt))) CorineCoverClasses(corine.toInt)
    else */
    if (!(globCover.equals("NA") || GlobCoverClasses.noData.contains(globCover.toInt))) GlobCoverClasses(globCover.toInt)
    else ModisCoverClasses(modis.toInt)

  }
}

class GridCellPerMonth(center: GeoPoint, gridSize: Angle,
  windSpeeds: List[Velocity],
  lc: LandCoverClass,
  elevation: Length, distanceToCoast: Length, urbanFactor: Double)
    extends GridCell(center, gridSize,
      windSpeeds.foldLeft(MetersPerSecond(0))(_ + _) / windSpeeds.size, WattsPerSquareMeter(0), lc, elevation, distanceToCoast, urbanFactor) {

  override def windSpeedMonth(month: Int) = windSpeeds(month)

}
