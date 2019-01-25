package wind_solar

import utils._
import grid._
import squants.space._
import squants.radio._
import squants.energy._
import squants.motion._
import squants.time._
import solar_energy._
import wind_energy._

object Grid {
  import PlotHelper._
  def main(args: Array[String]): Unit = {
    var t = System.currentTimeMillis()
    val grid = Grid()
    println("Grid loaded in " + (System.currentTimeMillis() - t) / 1000.0 + " seconds")
    grid.plot_eroi_potential(grid.cells, List(OnshoreWindTechnology, OffshoreWindTechnology,CSPTowerStorage12h, CSPParabolic), 1)
  }

  def apply(name: String) = new Grid(name, Degrees(0.75), (2 until 40).map(_ * 0.5).toList)
  def apply(): Grid = apply("runs_history/wind_solar/wind_solar_0_75")
}

class Grid(val name: String, val gridSize: Angle, val eroi_min: List[Double]) {
  import Helper._
  import PlotHelper._
  
  val cells: List[Cell] = Helper.getLines(name).map(Cell(_, gridSize, eroi_min))

  def country(c: String) = cells.filter(_.country.equalsIgnoreCase(c))
  def countries(c: List[String]) = cells.filter(x => c.contains(x.country))
  val eu28countries = Helper.getLines("../model_data/countries/EU28", "\t").map(_(0))
  def eu28 = cells.filter(g => eu28countries.contains(g.country))

  def eroi_potential(cells : List[Cell], tech : RenewableTechnology, eroi_min : Double) = listValueVSCumulated(cells.filter(c => tech.eroi(c, eroi_min) >= eroi_min).map(c => (tech.eroi(c, eroi_min), (tech.potential(c, eroi_min)*Hours(365*24)).to(Exajoules))))
  def plot_eroi_potential(cells : List[Cell], techs : List[RenewableTechnology], eroi_min : Double) {
    val list = techs.map(t => {
      val res=eroi_potential(cells, t, eroi_min)
      (res._1,res._2, t.name)
    })
    plotXY(list, xLabel = "Cumulated Potential [EJ/year]", yLabel = "EROI", legend= true)
  }
  def eroi_netpotential(cells : List[Cell], tech : RenewableTechnology, eroi_min : Double) = listValueVSCumulated(cells.filter(c => tech.eroi(c, eroi_min) >= eroi_min).map(c => (tech.eroi(c, eroi_min), tech.netYearlyProduction(c, eroi_min).to(Exajoules))))
  def plot_eroi_netpotential(cells : List[Cell], techs : List[RenewableTechnology], eroi_min : Double) {
    val list = techs.map(t => {
      val res=eroi_netpotential(cells, t, eroi_min)
      (res._1,res._2, t.name)
    })
    plotXY(list, xLabel = "Cumulated Potential [EJ/year]", yLabel = "EROI", legend= true)
  }
  
  def write(logFile: String) {
    val out_stream = new java.io.PrintStream(new java.io.FileOutputStream(logFile))
    val techs = List(PVPoly, CSPParabolicStorage12h)
    cells.map(c => out_stream.print(c.center.latitude.toDegrees + "\t" + c.center.longitude.toDegrees + 
        "\t" + CSPTowerStorage12h.eroi(c, 1) + "\t" + OnshoreWindTechnology.eroi(c, 1)
        +"\n"))
    out_stream.close()
  }
}

/**
 * Cell combines previous SolarCell and GridCell objects :)
 *
 * class SolarCell(val center: GeoPoint, val resolution: Angle, val ghi: Irradiance, val dni: Irradiance,
 *  val landCover: LandCoverType, val distanceToCoast: Length, val elevation: Length,
 *  val country: String, val protected_area: Boolean = false, val slope: SlopeGradients)
 */
/**
 * class GridCell(val csvLine: Array[String], center: GeoPoint, gridSize: Angle,
 * landCovers: DetailedLandCover,
 * protectedArea: Double,
 * country: Country,
 * elevation: Length,
 * distanceToCoast: Length,
 * val wind71m: WindProfile,
 * val wind125m: WindProfile,
 * val irradiance: MeteoData[Irradiance],
 * val optimalCD: Map[Double, (Velocity, Double)],
 * val keDissipation: Irradiance) extends DefaultGridCell(center, gridSize, landCovers, protectedArea, country, elevation, distanceToCoast) {
 *
 */

class Cell(val center: GeoPoint,
    val resolution: Angle,
    val ghi: Irradiance,
    val dni: Irradiance,
    val slope: SlopeGradients,
    val landCovers: DetailedLandCover,
    val protectedArea: Double,
    val country: String,
    val elevation: Length,
    val distanceToCoast: Length,
    val wind71m: WindProfile,
    val wind125m: WindProfile,
    val keDissipation : Irradiance,
    val optimalCD: Map[Double, (Velocity, Double)]) {

  val s = resolution / 2.0;
  val area = Helper.areaRectangle(center, resolution)
  val meanLonDistance = (Helper.distance(GeoPoint(center.latitude - s, center.longitude - s), GeoPoint(center.latitude - s, center.longitude + s)) +
    Helper.distance(GeoPoint(center.latitude + s, center.longitude - s), GeoPoint(center.latitude + s, center.longitude + s))) / 2.0
  val meanLatDistance = (Helper.distance(GeoPoint(center.latitude - s, center.longitude - s), GeoPoint(center.latitude + s, center.longitude - s)) +
    Helper.distance(GeoPoint(center.latitude - s, center.longitude + s), GeoPoint(center.latitude + s, center.longitude + s))) / 2.0

  val onshore = distanceToCoast.value <= 0
  val offshore = !onshore
  val EEZ = !country.equals("NA")
  val offshoreEEZ = offshore && EEZ
  val altitude = if (offshore) Meters(0) else elevation
  val waterDepth = if (onshore) Meters(0) else -elevation

  val hubAltitude = altitude + Meters(100)

  def proportion(lcType: LandCoverType): Double = landCovers.types.filter(_._2.equals(lcType)).map(_._1).sum
  def area(lcType: LandCoverType): Area = proportion(lcType) * area
  val protectedA = area * protectedArea

  val excludedCountries = List("NA", "Antarctica", "Greenland", "French Southern & Antarctic Lands")
  def suitabilityFactor(tech: RenewableTechnology) = {
    if (excludedCountries.contains(country) || country.contains("Is.") || country.contains("Islands")) 0.0
    tech.suitabilityFactor(this) * protectedArea
  }
  /**
   *  WIND
   */
  val wind100m = new WindProfile((wind71m.mean + wind125m.mean) / 2.0, (wind71m.std + wind125m.std) / 2.0, Meters(100))
  def installedCapacityDensity(vr: Velocity, n: Double, cp: Double = 0.5) = WattsPerSquareMeter(cp * 0.5 * 1.225 * Math.PI / 4 * Math.pow(vr.toMetersPerSecond, 3) / Math.pow(n, 2))

  def getOptimalCD(e: Double): Irradiance =
    if (!optimalCD.keySet.contains(e)) WattsPerSquareMeter(0)
    else installedCapacityDensity(optimalCD(e)._1, optimalCD(e)._2)
  def getOptimalVrN(e: Double): (Velocity, Double) = optimalCD.get(e).getOrElse((MetersPerSecond(0), 15))
  def optimalRatedSpeed(eroi_min: Double) = getOptimalVrN(eroi_min)._1
  def optimalN(eroi_min: Double) = getOptimalVrN(eroi_min)._2

  /**
   * Solar
   */

}
/**
 * Text file:
 * 0 : lat, 1 : lon, 2 : ghi [kWh/m2/day], 3, dni [kWh/m2/day],
 * 4 -> 11 : slope gradients [/1000]
 * 12 Protected Areas [/100],
 * 13 -> 35 : Detailed Land Covers,
 * 36 : Elevation, 37 : Distance to nearest coast, 38 : Country Name,
 * 39-40 : WindProfile 70m
 * 41-42 : WindProfile 125m
 * 43 : kinetic energy dissipation 
 * 44 -> the end : optimal vr, optimal n, true/false
 *
 */
object Cell {
  val optiIndex = 44
  val indexes = Array(11, 14, 20, 30, 40, 50, 60, 70, 90, 100, 110, 120, 130, 140, 150, 160, 170, 180, 190, 200, 210, 220, 230)
  def lcs(l: Array[String], start: Int) = {
    val sumLC = (start until start + indexes.size).map(i => l(i).toDouble).sum
    val lcs = for (i <- (start until start + indexes.size); if (l(i).toDouble > 0)) yield (l(i).toDouble / sumLC, indexes(i - start))
    new DetailedLandCover(lcs.toList)
  }
  def apply(l: Array[String], resolution: Angle, eroi_min: List[Double]) = {

    new Cell(
      GeoPoint(Degrees(l(1).toDouble), Degrees(l(0).toDouble)),
      resolution,
      WattsPerSquareMeter(l(2).toDouble / 24 * 1000), WattsPerSquareMeter(l(3).toDouble / 24 * 1000),
      SlopeGradients(l, 4, 11),
      lcs(l, 13),
      l(12).toDouble / 100.0,
      l(38).toString,
      Meters(l(36).toDouble), Kilometers(l(37).toDouble),
      new WindProfile(MetersPerSecond(l(39).toDouble), l(40).toDouble, Meters(71)),
      new WindProfile(MetersPerSecond(l(41).toDouble), l(42).toDouble, Meters(125)),
      WattsPerSquareMeter(l(43).toDouble),
      if (l.size > 44 + 1 && !l(44).equals("")) (for (e <- (0 until eroi_min.size); if (l(e * 3 + optiIndex + 2).toBoolean)) yield (eroi_min(e), (MetersPerSecond(l(e * 3 + optiIndex).toDouble), l(e * 3 + optiIndex + 1).toDouble))).toMap
      else Map())
  }
}