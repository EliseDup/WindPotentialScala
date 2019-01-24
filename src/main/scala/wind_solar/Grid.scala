package wind_solar

import utils._
import grid._
import squants.space._
import squants.radio._
import solar_energy._
import squants.motion.Velocity
import wind_energy.WindProfile
import squants.motion.MetersPerSecond

object Grid {
  import PlotHelper._
  def main(args: Array[String]): Unit = {
    val grid = Grid()
    plotXY(grid.cells.map(_.dni.toWattsPerSquareMeter), grid.cells.map(_.ghi.toWattsPerSquareMeter)) 
  }
  
  def apply(name: String) = new Grid(name, Degrees(0.75), (2 until 40).map(_ * 0.5).toList)
  def apply(): Grid = apply("runs_history/wind_solar/wind_solar_0_75")
}
class Grid(val name: String, val gridSize: Angle, val eroi_min: List[Double]) {
  val cells: List[Cell] = Helper.getLines(name).map(Cell(_, gridSize, eroi_min))
  
}
/**
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
    val country: Country,
    val elevation: Length,
    val distanceToCoast: Length,
    val wind71m: WindProfile,
    val wind125m: WindProfile,
    val optimalCD: Map[Double, (Velocity, Double)]) {
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
 * 43 ? kinetic energy dissipation ?
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
      Country(l(38)), //, countryList(l, countryIndex)),
      Meters(l(36).toDouble), Kilometers(l(37).toDouble),
      new WindProfile(MetersPerSecond(l(39).toDouble), l(40).toDouble, Meters(71)),
      new WindProfile(MetersPerSecond(l(41).toDouble), l(42).toDouble, Meters(125)),
      if (l.size > 44 + 1 && !l(44).equals("")) (for (e <- (0 until eroi_min.size); if (l(e * 3 + optiIndex + 2).toBoolean)) yield (eroi_min(e), (MetersPerSecond(l(e * 3 + optiIndex).toDouble), l(e * 3 + optiIndex + 1).toDouble))).toMap
      else Map())
  }
}