package solar_energy

import squants.energy._
import squants.time.Hours
import squants.space._
import squants.radio._
import utils._
import grid._
import scala.io.Source
import java.io.PrintStream

object SolarGrid {
  def _0_1deg = apply("../resources/data_solar/0_1deg_protected", Degrees(0.1))
  def _0_5deg = apply("../resources/data_solar/0_5deg", Degrees(0.5))

  def apply(name: String, res: Angle) = {
    val list = Source.fromFile(name).getLines().toList
    new SolarGrid(list.map(SolarCell(_, res)).toList)
  }
}

class SolarGrid(val cells: List[SolarCell]) {
  def country(c: String) = cells.filter(_.country.equalsIgnoreCase(c))

  def write(logFile: String) {
    val out_stream = new PrintStream(new java.io.FileOutputStream(logFile))

    cells.map(c => out_stream.print(c.center.latitude.toDegrees + "\t" + c.center.longitude.toDegrees + "\t" +
      c.distanceToCoast.toKilometers + "\n"))
    out_stream.close()
  }
}

class SolarCell(val center: GeoPoint, val resolution: Angle, val ghi: Irradiance, val dni: Irradiance,
    val landCover: LandCoverType, val distanceToCoast: Length, val elevation: Length,
    val slope_geq45: Double, val slope_geq0_5: Double,
    val country: String, val protected_area: Boolean = false) {

  val area = Helper.areaRectangle(center, resolution)
  val suitableArea = if (protected_area) SquareKilometers(0) else landCover.solarFactor.mean * area * (1.0 - slope_geq45)
  def area(lcType: LandCoverType): Area = if (lcType.equals(landCover)) area else SquareKilometers(0)

  // Actual area occupied by pv panles / heliostat / ...
  def panelArea(tech: Technology) = suitableArea / tech.occupationRatio
  def potential(tech: Technology) = panelArea(tech) * (if (tech.directOnly) dni else ghi) * tech.efficiency * tech.performanceRatio

  val pvPotential = potential(PVTechnology)
  val installedPV = panelArea(PVTechnology) * PVTechnology.efficiency * WattsPerSquareMeter(1000)

  def eroiPV: Double = {
    if (ghi.value == 0 || suitableArea.value == 0) 0.0
    else {
      val out = Hours(25 * 365 * 24) * pvPotential
      out / PVPoly.embodiedEnergy(installedPV, out)
    }
  }
}

object SolarCell {
  def apply(line: String, resolution: Angle) = {
    val i = line.split("\t")
    new SolarCell(GeoPoint(Degrees(i(1).toDouble), Degrees(i(0).toDouble)), resolution,
      WattsPerSquareMeter(i(2).toDouble / 24 * 1000), WattsPerSquareMeter(i(3).toDouble / 24 * 1000),
      GlobCoverClasses.landCoverType(i(4).toDouble.toInt), Kilometers(i(5).toDouble), Meters(i(6).toDouble),
      slope(i(7)), slope(i(8)),
      i(9).toString, i(10).toInt == 1)
  }
  def slope(i: String) = {
    val s = i.toDouble
    if (s > 100) println("Slope > 100 !" + s)
    math.max(0, s / 100.0)
  }
}

object SolarUtils {
  def areaList(list: List[SolarCell]) = list.map(_.area).foldLeft(SquareKilometers(0))(_ + _)
  def suitableAreaList(list: List[SolarCell]) = list.map(_.suitableArea).foldLeft(SquareKilometers(0))(_ + _)
}

class Technology(val efficiency: Double, val performanceRatio: Double, val occupationRatio: Double, val directOnly: Boolean)

object PVTechnology extends Technology(0.24, 0.883, 5, false)
object CSPTechnology extends Technology(0.16, 1, 7.5, true)