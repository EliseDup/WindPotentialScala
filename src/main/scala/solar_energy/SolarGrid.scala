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

  def _0_1deg = apply("../resources/data_solar/0_1deg", Degrees(0.1))
  def _0_5deg = apply("../resources/data_solar/0_5deg", Degrees(0.5))
  def _0_5deg_total = apply("../resources/data_solar/0_5deg_total", Degrees(0.5))

  def apply(name: String, res: Angle) = {
    val list = Source.fromFile(name).getLines().toList
    new SolarGrid(list.map(SolarCell(_, res)).toList)
  }

}

class SolarGrid(val cells: List[SolarCell]) {
  def country(c: String) = cells.filter(_.country.equalsIgnoreCase(c))
  def countries(c: List[String]) = cells.filter(x => c.contains(x.country))
  val eu28countries = Helper.getLines("../model_data/countries/EU28", "\t").map(_(0))
  def eu28 = cells.filter(g => eu28countries.contains(g.country))

  def write(logFile: String) {
    val out_stream = new PrintStream(new java.io.FileOutputStream(logFile))
    val techs = List(PVPoly, CSPParabolicStorage12h)

    cells.map(c => out_stream.print(c.center.latitude.toDegrees + "\t" + c.center.longitude.toDegrees + "\t" +
      (if (c.suitabilityFactor(CSPParabolic) > 0) CSPParabolic.max_eroi_sm(c.dni) else "0.0") + "\t" +
      (if (c.suitabilityFactor(CSPParabolicStorage12h) > 0) CSPParabolicStorage12h.max_eroi_sm(c.dni) else "0.0") + "\t" +
      (if (c.suitabilityFactor(CSPTowerStorage12h) > 0) CSPTowerStorage12h.max_eroi_sm(c.dni) else "0.0") + "\t" +

      (if (c.suitabilityFactor(techs(0)) == 0) 0.0 else techs.indexOf(c.bestTechnology(techs)) + 1).toDouble + "\t" +
      c.dni.toWattsPerSquareMeter * 8.76 + "\t" + c.ghi.toWattsPerSquareMeter * 8.76 + "\t" +
      (if (c.dni > c.ghi) 1.0 else 0.0) + "\t" + c.eroi(techs) +
      "\t" + PVPoly.capacityFactor(c.ghi, c.panelArea(PVPoly)) * 100 + "\t" +
      CSPParabolicStorage12h.capacityFactor(c.dni, c.panelArea(CSPParabolicStorage12h)) * 100 + "\n"))
    // cells.filter(_.dni.toWattsPerSquareMeter >= 200).map(c => out_stream.print(c.center.latitude.toDegrees + "\t" + c.center.longitude.toDegrees + "\t" +
    //    + c.dni.toWattsPerSquareMeter*8.76 + "\n"))
    out_stream.close()
  }
  def writePotential(eroi_min: Double) {
    val out_stream = new PrintStream(new java.io.FileOutputStream("potential_" + eroi_min))
    val techs = List(PVPoly, CSPParabolicStorage12h, CSPTowerStorage12h, CSPParabolic)
    cells.map(c => out_stream.print(c.center.latitude.toDegrees + "\t" + c.center.longitude.toDegrees + "\t" +

      (if (c.potential(techs).value > 0 && c.eroi(techs) > eroi_min) {
        math.min((techs.indexOf(c.bestTechnology(techs)) + 1).toDouble, 2.0)
      } else {
        "0.0"
      }) + "\n"))
    out_stream.close()

  }
}

class SolarCell(val center: GeoPoint, val resolution: Angle, val ghi: Irradiance, val dni: Irradiance,
    val landCover: LandCoverType, val distanceToCoast: Length, val elevation: Length,
    val country: String, val protected_area: Boolean = false, val slope: SlopeGradients) {

  val excludedCountries = List("NA", "Antarctica", "Greenland", "French Southern & Antarctic Lands")
  val onshore = distanceToCoast.value <= 0
  def suitabilityFactor(tech: SolarTechnology) = {
    if (protected_area || excludedCountries.contains(country) || country.contains("Is.") || country.contains("Islands")) 0.0
    else if ((tech.directOnly && dni.value == 0) || ghi.value == 0) 0.0
    else landCover.solarFactor.mean * slope.slope_leq(tech.maximumSlope)
  }

  val area = Helper.areaRectangle(center, resolution)
  def suitableArea(tech: SolarTechnology): Area = suitabilityFactor(tech) * area
  def suitableArea(techs: List[SolarTechnology]): Area = suitableArea(techs(techs.indexOf(bestTechnology(techs))))

  def area(lcType: LandCoverType): Area = if (lcType.equals(landCover)) area else SquareKilometers(0)

  // Actual area occupied by pv panels / heliostat / ...
  def panelArea(tech: SolarTechnology): Area = suitableArea(tech) / tech.occupationRatio
  def potential(tech: SolarTechnology): Power = tech.potential(if (tech.directOnly) dni else ghi, panelArea(tech))
  def grossYearlyProduction(tech: SolarTechnology): Energy = potential(tech) * Hours(365 * 24)
  def netYearlyProduction(tech: SolarTechnology): Energy = {
    val solar = if (tech.directOnly) dni else ghi
    val aperture = panelArea(tech)
    val power = tech.ratedPower(aperture, solar)
    val gross = grossYearlyProduction(tech)
    gross - tech.ee.embodiedEnergy(power, gross) / tech.ee.lifeTime
  }
  // Technology that maximizes the EROI
  def bestTechnology(techs: List[SolarTechnology]): SolarTechnology = techs(techs.zipWithIndex.map(i => (eroi(i._1), i._2)).maxBy(_._1)._2)

  def potential(techs: List[SolarTechnology]): Power = potential(bestTechnology(techs))
  def netYearlyProduction(techs: List[SolarTechnology]): Energy = netYearlyProduction(bestTechnology(techs))
  def grossYearlyProduction(techs: List[SolarTechnology]): Energy = grossYearlyProduction(bestTechnology(techs))

  def installedCapacity(tech: SolarTechnology) = panelArea(tech) * tech.designEfficiency * tech.designPointIrradiance // / tech.solarMultiple //* (if (tech.directOnly) dni else WattsPerSquareMeter(1000))

  def eroi(techs: List[SolarTechnology]): Double = techs.map(eroi(_)).max

  def eroi(tech: SolarTechnology): Double = {
    if ((tech.directOnly && dni.value == 0) || ghi.value == 0 || suitableArea(tech).value == 0) 0.0
    else {
      tech.eroi(if (tech.directOnly) dni else ghi)
      //     val out_year = Hours(365 * 24) * potential(tech)
      //     tech.ee.lifeTime * (out_year / tech.ee.embodiedEnergy(installedCapacity(tech), out_year))
    }
  }

}

object SolarCell {
  def apply(line: String, resolution: Angle) = {
    val i = line.split("\t")
    new SolarCell(GeoPoint(Degrees(i(1).toDouble), Degrees(i(0).toDouble)), resolution,
      WattsPerSquareMeter(i(2).toDouble / 24 * 1000), WattsPerSquareMeter(i(3).toDouble / 24 * 1000),
      GlobCoverClasses.landCoverType(i(4).toDouble.toInt), Kilometers(i(5).toDouble), Meters(i(6).toDouble),
      i(7).toString, i(8).toInt == 1, SlopeGradients(i))
  }
  def slope(i: String) = {
    val s = i.toDouble
    if (s > 100) println("Slope > 100 !" + s)
    math.max(0, s / 100.0)
  }
}

object SolarUtils {
  def areaList(list: List[SolarCell]) = list.map(_.area).foldLeft(SquareKilometers(0))(_ + _)
  def suitableAreaList(list: List[SolarCell], tech: SolarTechnology) = list.map(_.suitableArea(tech)).foldLeft(SquareKilometers(0))(_ + _)
}

/*SLOPES	Slope class
CL1 % ≤ slope ≤ 0.5 %
CL2	0.5 % ≤ slope ≤ 2 %
CL3	2 % ≤ slope ≤ 5 %
CL4	5 % ≤ slope ≤ 10 %
CL5	10 % ≤ slope ≤ 15 %
CL6	15 % ≤ slope ≤ 30 %
CL7	30 % ≤ slope ≤ 45 %
CL8	Slope > 45 %*/

class SlopeGradients(val gradients: List[((Double, Double), Double)]) {
  // Count the percentage of area with slope less that a given threshold
  // If include, the interval contains the given percentage, if not it is strictly less
  val total = gradients.map(_._2).sum
  def slope_leq(percent: Double, include: Boolean = true) = {
    assert(percent <= 100)
    val lastIndex =
      if (gradients.exists(_._1._2 == percent)) gradients.indexWhere(i => i._1._2 == percent)
      else if (include) gradients.indexWhere(i => percent >= i._1._1 && percent < i._1._2)
      else if (percent <= 0.5 / 100) 0
      else gradients.indexWhere(i => percent >= i._1._1 && percent < i._1._2) - 1
    (0 to lastIndex).map(gradients(_)._2).sum
  }
  def slope_geq(percent: Double, include: Boolean = true) = {
    assert(percent <= 100)
    val lastIndex =
      if (gradients.exists(_._1._1 == percent)) gradients.indexWhere(i => i._1._1 == percent)
      else if (include) gradients.indexWhere(i => percent >= i._1._1 && percent < i._1._2)
      else if (percent >= 45) gradients.size - 1
      else gradients.indexWhere(i => percent >= i._1._1 && percent < i._1._2) + 1
    (lastIndex until gradients.size).map(gradients(_)._2).sum
  }
  override def toString() = {
    var res = "Slope Gradients, total = " + total * 100 + "% \n"
    for (g <- gradients) res = res + (g._1._1.toString + "<=slope<=" + g._1._2.toString + " : " + (math.ceil(g._2 * 100).toInt).toString + "%" + "\n")
    res
  }
}
object SlopeGradients {
  val classes = List((0.0, 0.5), (0.5, 2.0), (2.0, 5.0), (5.0, 10.0), (10.0, 15.0), (15.0, 30.0), (30.0, 45.0), (45.0, 100.0))
  def degreesToPercent(d: Angle): Double = math.tan(d.toRadians) * 100
  def percentToDegrees(p: Double): Angle = {
    assert(p <= 100)
    Radians(math.atan(p / 100.0))
  }
  def apply(line: Array[String]) = {
    val slopes = (9 to 16).toList.map(line(_).toDouble / 10000.0)
    new SlopeGradients((0 to 7).toList.map(i => (classes(i), slopes(i))))
  }
}
    
    