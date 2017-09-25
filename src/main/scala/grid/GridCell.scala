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
import solar_energy._
import squants.UnitOfMeasure
import squants.Quantity

case class Country(val name: String, val map: List[(String, Double)] = List()) {
  val countries = map.map(_._1)
  def proportion(c: String): Double = {
    if (map.nonEmpty && countries.contains(c)) map.find(_._1.equals(c)).get._2
    else if (name == "c") 1.0 else 0.0
  }
  def proportion(c: List[String]): Double = c.map(proportion(_)).sum
  def isCountry(c: String): Boolean = if (map.nonEmpty) countries.contains(c) else name.equals(c)

  def isCountry(c: List[String]): Boolean = {
    if (map.isEmpty) name.equals(c)
    else if (c.size == 1) isCountry(c(0))
    else c.exists(i => countries.contains(i))
  }
}
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
class DefaultGridCell(val center: GeoPoint, val gridSize: Angle, val landCovers: DetailedLandCover, val protectedArea: Double, val country: Country,
    val elevation: Length, val distanceToCoast: Length) {

  val s = gridSize / 2.0;

  val area = Helper.areaRectangle(center, gridSize)
  val meanLonDistance = (Helper.distance(GeoPoint(center.latitude - s, center.longitude - s), GeoPoint(center.latitude - s, center.longitude + s)) +
    Helper.distance(GeoPoint(center.latitude + s, center.longitude - s), GeoPoint(center.latitude + s, center.longitude + s))) / 2.0
  val meanLatDistance = (Helper.distance(GeoPoint(center.latitude - s, center.longitude - s), GeoPoint(center.latitude + s, center.longitude - s)) +
    Helper.distance(GeoPoint(center.latitude - s, center.longitude + s), GeoPoint(center.latitude + s, center.longitude + s))) / 2.0

  val onshore = distanceToCoast.value <= 0
  val offshore = !onshore
  val EEZ = !country.name.equals("NA")
  val offshoreEEZ = offshore && EEZ
  val altitude = if (offshore) Meters(0) else elevation
  val waterDepth = if (onshore) Meters(0) else -elevation

  val hubAltitude = altitude + Meters(100)

  def proportion(lcType: LandCoverType): Double = landCovers.types.filter(_._2.equals(lcType)).map(_._1).sum
  def area(lcType: LandCoverType): Area = proportion(lcType) * area
  val protectedA = area * protectedArea

  def isCountry(c: String) = country.isCountry(c); def isCountry(c: List[String]) = country.isCountry(c);
  def proportion(c: String): Double = country.proportion(c)
  def area(c: String): Area = country.proportion(c) * area; def area(c: List[String]): Area = country.proportion(c) * area
}

object DefaultGridCell {
  /**
   * CSV LINE :
   * [0] Latitude, [1] Longitude,
   * [2] Protected Areas,
   * [3-25] Detailed Land Covers,
   * [26] Elevation, [27] Distance to nearest coast, [28] Country Name
   */
  def center(line: Array[String]) = GeoPoint(Degrees(line(0).toDouble), Degrees(line(1).toDouble))
  def velocity(line: Array[String], index: Int) = MetersPerSecond(line(index).toDouble)
  def irradiance(line: Array[String], index: Int) = WattsPerSquareMeter(line(index).toDouble)

  val indexes = Array(11, 14, 20, 30, 40, 50, 60, 70, 90, 100, 110, 120, 130, 140, 150, 160, 170, 180, 190, 200, 210, 220, 230)
  def lcs(l: Array[String]) = {
    val sumLC = (3 until 26).map(i => l(i).toDouble).sum
    val lcs = for (i <- (3 until 26); if (l(i).toDouble > 0)) yield (l(i).toDouble / sumLC, indexes(i - 3))
    new DetailedLandCover(lcs.toList)
  }
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
    distanceToCoast: Length,
    val wind71m: WindProfile,
    val wind125m: WindProfile,
    val irradiance: MeteoData[Irradiance],
    val optimalCD: Map[Double, (Velocity, Double)],
    val keDissipation: Irradiance) extends DefaultGridCell(center, gridSize, landCovers, protectedArea, country, elevation, distanceToCoast) {

  override def toString() = "Grid Object center : " + center

  import SolarPower._
  import WindPower._
  /**
   * WIND
   */
  val wind100m = new WindProfile((wind71m.mean + wind125m.mean) / 2.0, (wind71m.std + wind125m.std) / 2.0, Meters(100))

  def estimatedDissipation(world: WorldGrid) =
    if (area.toSquareKilometers == 0) WattsPerSquareMeter(0)
    else world.totalDissipation / area * (Math.pow(wind125m.mean.toMetersPerSecond, 2) * area.toSquareMeters / world.totalSquareSpeedArea)

  def countrySuitableArea(c: List[String]) = WindPotential().suitabilityFactor(this) * area * country.proportion(c)

  def suitableArea(potential: EnergyGenerationPotential) = potential.suitabilityFactor(this) * area
  def suitableArea(suitable: Boolean, potential: EnergyGenerationPotential = WindPotential()): Area = if (suitable) suitableArea(potential) else area
  def installedCapacityDensity(vr: Velocity, n: Double, cp: Double = 0.5) = WattsPerSquareMeter(cp * 0.5 * 1.225 * Math.PI / 4 * Math.pow(vr.toMetersPerSecond, 3) / Math.pow(n, 2))

  def getOptimalCD(e: Double): Irradiance =
    if (!optimalCD.keySet.contains(e)) WattsPerSquareMeter(0)
    else installedCapacityDensity(optimalCD(e)._1, optimalCD(e)._2)

  def getOptimalVrN(e: Double): (Velocity, Double) = optimalCD.get(e).getOrElse((MetersPerSecond(0), 15))
  def optimalRatedSpeed(eroi_min: Double) = optimalCD.get(eroi_min).getOrElse((MetersPerSecond(0), 15))._1
  def optimalN(eroi_min: Double) = optimalCD.get(eroi_min).getOrElse((MetersPerSecond(0), 15.0))._2

  /**
   * SOLAR
   */
  val yearlyClearnessIndex = {
    if (Math.abs(center.latitude.toDegrees) >= 65) 0.0
    else irradiance.mean / yearlyRadiation(center.latitude)
  }
  val monthlyClearnessIndex =
    if (Math.abs(center.latitude.toDegrees) >= 65) (0 until 12).map(m => 0.0).toList
    else (0 until 12).map(m => irradiance.perMonth(m) / monthlyRadiation(m, center.latitude)).toList

  // Daily Clearness Index 
  val kmin = 0.05; val kmax = monthlyClearnessIndex.map(kav => 0.613 + 0.267 * kav - 11.9 * Math.pow(kav - 0.75, 8))
  val epsilon = (0 until 12).map(i => (kmax(i) - kmin) / (kmax(i) - monthlyClearnessIndex(i))).toList
  val sigma = (0 until 12).map(i => -1.498 + (1.184 * epsilon(i) - 27.182 * Math.exp(-1.5 * epsilon(i))) / (kmax(i) - kmin)).toList
  def dailyClearnessIndex(ndk: Int, ndm: Int, m: Int) = {
    val alpha = (ndk - 0.5) / ndm
    (1 / sigma(m)) * (Math.log((1 - alpha) * Math.exp(sigma(m) * kmin) + alpha * (Math.exp(sigma(m) * kmax(m)))))
  }
  
  val directIrradiance = irradiance.mean * (1 - diffuseFraction(yearlyClearnessIndex))
  val monthlyDirectIrradiance = (0 until 12).map(m => irradiance.month(m) * (1 - diffuseFraction(monthlyClearnessIndex(m)))).toList

  // def directRadiation = Thermodynamics.diffuseFraction(clearnessIndex, d, h)
}
/**
 * Latitude	Longitude	Corine Land Cover	GlobCover	Modis	Urban Factor	Protected area ?	Country	Elevation [m]	Distance to Coast [km]	Uwind	Vwind	Wind	Std Wind	KineticEnergyDissipation Irradiance
 *
 */

object GridCell {
  /**
   * [29] Mean wind 125m , [30] Std wind 125 m,
   * [31] Mean wind 71m, [32] Std wind 71 m
   * [33] KE dissipation
   * [34] Yearly Irradiance
   * [35 -> 35+12] Monthly Irradiance
   *
   * TODO FIX THIS !
   * Then if the optimization was made :
   * [34] -> [116] Pair of (optimal installed capacity, boolean) for EROI 0 -> 20 by 0.5
   *
   * So 40 is the first with old version !
   */
  def countryList(l: Array[String]): List[(String, Double)] = {
    val countryIndex = 47
    if (l.size > countryIndex + 1) {
      val xs = (countryIndex until l.size).map(i => l(i).toString).filter(i => !i.equals("")).toList
      xs.toList.distinct.map(x => (x.toString, xs.count(_ == x.toString) / 9.0))

    } else List()
  }
  def apply(l: Array[String], gridSize: Angle, eroi_min: List[Double], optiIndex: Int = 34, optiWind: Boolean = false, solar: Boolean = true) = {
    new GridCell(l, DefaultGridCell.center(l), gridSize,
      DefaultGridCell.lcs(l),
      l(2).toDouble / 100.0,
      Country(l(28), countryList(l)),
      Meters(l(26).toDouble), Kilometers(l(27).toDouble),
      new WindProfile(MetersPerSecond(l(31).toDouble), l(32).toDouble, Meters(71)),
      new WindProfile(MetersPerSecond(l(29).toDouble), l(30).toDouble, Meters(125)),

      if (solar) new MeteoData[Irradiance](DefaultGridCell.irradiance(l, 34), (1 to 12).toList.map(i => DefaultGridCell.irradiance(l, i + 34)).toArray)
      else new MeteoData(WattsPerSquareMeter(0), Array()),

      if (optiWind && l.size > optiIndex + 1 && !l(optiIndex).equals("")) (for (e <- (0 until eroi_min.size); if (l(e * 3 + optiIndex + 2).toBoolean)) yield (eroi_min(e), (MetersPerSecond(l(e * 3 + optiIndex).toDouble), l(e * 3 + optiIndex + 1).toDouble))).toMap else Map(),
      WattsPerSquareMeter(l(33).toDouble))
  }
}
