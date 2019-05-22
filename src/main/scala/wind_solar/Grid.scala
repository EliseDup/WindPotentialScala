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
import wind_energy.WindTechnology

object Grid {
  def apply(name: String) = new Grid(name, Degrees(0.75), (2 until 40).map(_ * 0.5).toList)
  def apply(): Grid = apply("runs_history/wind_solar_2019/wind_solar_0_75")
  def eu(): Grid = apply("runs_history/wind_solar_2019/eu28_wind_solar_0_75")
  
  def main(args: Array[String]): Unit = {
    val grid = Grid()
    // grid.write("sites")
  }
}

class Grid(val name: String, val gridSize: Angle, val eroi_min: List[Double]) {
  import Helper._
  import PlotHelper._

  val cells: List[Cell] = Helper.getLines(name).map(Cell(_, gridSize, eroi_min))

  def country(c: String) = cells.filter(_.isCountry(c))
  def countries(c: List[String]) = cells.filter(_.isCountry(c))

  val eu28countries = Helper.getLines("../model_data/countries/EU28", "\t").map(_(0))
  def eu28 = cells.filter(c => eu28countries.contains(c.country)) //_.isCountry(eu28countries))

  // List eroi vs cumulated potential for a given renewable technology
  def eroi_potential(cells: List[Cell], tech: RenewableTechnology, eroi_min: Double) = listValueVSCumulated(cells.filter(c => tech.eroi(c, eroi_min) >= eroi_min).map(c => (tech.eroi(c, eroi_min), (tech.potential(c, eroi_min) * Hours(365 * 24)).to(Exajoules))))
  def eroi_netpotential(cells: List[Cell], tech: RenewableTechnology, eroi_min: Double): (List[Double], List[Double]) = eroi_netpotential(cells, List(tech), eroi_min)
  def eroi_netpotential(cells: List[Cell], techs: List[RenewableTechnology], eroi_min: Double): (List[Double], List[Double]) = listValueVSCumulated(cells.map(c => (techs.map(tech => (tech.eroi(c, eroi_min), tech.netYearlyProduction(c, eroi_min).to(Exajoules))).maxBy(_._1))))

  def netpotential(cells: List[Cell], tech: RenewableTechnology, eroi_min: Double): Double = netpotential(cells, List(tech), eroi_min)
  // List eroi vs cumulated net potential for the technology maximising the EROI
  def netpotential(cells: List[Cell], techs: List[RenewableTechnology], eroi_min: Double): Double = cells.map(c => techs.map(t => (t.eroi(c, eroi_min), t.netYearlyProduction(c, eroi_min).to(Petajoules))).maxBy(_._1)._2).sum
  // List eroi vs cumulated net potential for the sum of the technologies (for technologies that can be combined, i.e. solar and wind)
  def sum_netpotential(cells: List[Cell], techs: List[List[RenewableTechnology]], eroi_min: Double): Double =
    cells.map(c => techs.map(tech => tech.map(t => (t.eroi(c, eroi_min), t.netYearlyProduction(c, eroi_min).to(Exajoules))).maxBy(_._1)._2).sum).sum
  
  def plot_eroi_netpotential(cells: List[Cell], techs: List[RenewableTechnology], eroi_min: Double) {
    val list = techs.map(t => {
      val res = eroi_netpotential(cells, t, eroi_min)
      (res._1, res._2, t.name)
    })
    plotXY(list, xLabel = "Net Potential [EJ/year]", yLabel = "EROI", legend = true)
  }

  
  // All functions to write data in a text file to plot them afterwards
  def write(logFile: String, inclusion: List[Cell] = List()) {
    val out_stream = new java.io.PrintStream(new java.io.FileOutputStream(logFile))
    val techs = List(PVPoly, CSPParabolicStorage12h)
    cells.filter(c => (OnshoreWindTechnology.suitabilityFactor(c)  >0 ||
          OffshoreWindTechnology.suitabilityFactor(c) >0 ||
          PVMono.suitabilityFactor(c) >0 ||
          CSPParabolic.suitabilityFactor(c) > 0)).map(c => out_stream.print(c.center.latitude.toDegrees + "\t" + c.center.longitude.toDegrees +
      "\t" +
      (if (inclusion.isEmpty || inclusion.contains(c)) c.area.toSquareKilometers + "\t" + 
          OnshoreWindTechnology.suitabilityFactor(c) + "\t" + 
          OffshoreWindTechnology.suitabilityFactor(c) + "\t" + 
          PVMono.suitabilityFactor(c) + "\t" +
          CSPParabolic.suitabilityFactor(c) + "\t" +
          c.wind100m.c.toMetersPerSecond + "\t" + c.wind100m.k + "\t" + c.ghi.toWattsPerSquareMeter + "\t" + c.dni.toWattsPerSquareMeter
      else "0.0" + "\t" + "0.0" + "\t" + "0.0" + "\t" + "0.0" + "\t" + "0.0" + "\t" + "0.0" + "\t" + "0.0")
      + "\n"))
    out_stream.close()
  }
  
  def writePotential(tech: RenewableTechnology, eroi_min: List[Double], inclusion: List[Cell] = List()) { eroi_min.map(e => writePotential(tech, e, inclusion)) }
  def writePotential(tech: RenewableTechnology, eroi_min: Double, inclusion: List[Cell]) {
    println("Write potential " + tech.name + " " + eroi_min)
    val out_stream = new java.io.PrintStream(new java.io.FileOutputStream("../WindPotentialPython/wind_solar_grid/" + tech.name + "_" + eroi_min.toInt))
    cells.map(c => out_stream.print(c.center.latitude.toDegrees + "\t" + c.center.longitude.toDegrees +
      "\t" +
      (if ((inclusion.isEmpty || inclusion.contains(c)) && tech.suitabilityFactor(c) > 0 && tech.netYearlyProduction(c, eroi_min).value > 0)
        tech.eroi(c, eroi_min) + "\t" +
        tech.potential(c, eroi_min).to(Megawatts) / (tech.suitabilityFactor(c) * c.area.toSquareKilometers) + "\t" +
        tech.potential(c, eroi_min).to(Megawatts) / (c.area.toSquareKilometers) + "\t" +
        tech.netYearlyProduction(c, eroi_min).to(GigawattHours) / (tech.suitabilityFactor(c) * c.area.toSquareKilometers) + "\t" +
        tech.netYearlyProduction(c, eroi_min).to(GigawattHours) / (c.area.toSquareKilometers) + "\t" +
        (if (tech.potential(c, eroi_min).value > 0) (tech.ratedPower(c, eroi_min).toMegawatts / c.area.toSquareKilometers) else "0.0")
      else "0.0" + "\t" + "0.0" + "\t" + "0.0" + "\t" + "0.0" + "\t" + "0.0")
      + "\n"))
    out_stream.close()
  }
  def writeWindPotential(eroi_min: Double, inclusion: List[Cell]) {
    val techs = List(OnshoreWindTechnology, OffshoreWindTechnology)
    val out_stream = new java.io.PrintStream(new java.io.FileOutputStream("../WindPotentialPython/wind_solar_grid/wind_" + eroi_min.toInt))
    cells.map(c => out_stream.print(c.center.latitude.toDegrees + "\t" + c.center.longitude.toDegrees + "\t" +
      (if (c.isCountry(eu28countries) && OnshoreWindTechnology.suitabilityFactor(c) > 0 && OnshoreWindTechnology.netYearlyProduction(c, eroi_min).value > 0) {
        OnshoreWindTechnology.eroi(c, eroi_min) + "\t" + OnshoreWindTechnology.potential(c, eroi_min).to(Megawatts) / (OnshoreWindTechnology.suitabilityFactor(c) * c.area.toSquareKilometers) + "\t" +
          OnshoreWindTechnology.potential(c, eroi_min).to(Megawatts) / (c.area.toSquareKilometers) + "\t" +
          OnshoreWindTechnology.netYearlyProduction(c, eroi_min).to(GigawattHours) / (OnshoreWindTechnology.suitabilityFactor(c) * c.area.toSquareKilometers) + "\t" +
          OnshoreWindTechnology.netYearlyProduction(c, eroi_min).to(GigawattHours) / (c.area.toSquareKilometers)
      } else if (c.isCountry(eu28countries) && OffshoreWindTechnology.suitabilityFactor(c) > 0 && OffshoreWindTechnology.netYearlyProduction(c, eroi_min).value > 0) {
        OffshoreWindTechnology.eroi(c, eroi_min) + "\t" +
          OffshoreWindTechnology.potential(c, eroi_min).to(Megawatts) / (OffshoreWindTechnology.suitabilityFactor(c) * c.area.toSquareKilometers) + "\t" +
          OffshoreWindTechnology.potential(c, eroi_min).to(Megawatts) / (c.area.toSquareKilometers) + "\t" +
          OffshoreWindTechnology.netYearlyProduction(c, eroi_min).to(GigawattHours) / (OffshoreWindTechnology.suitabilityFactor(c) * c.area.toSquareKilometers) + "\t" +
          OffshoreWindTechnology.netYearlyProduction(c, eroi_min).to(GigawattHours) / (c.area.toSquareKilometers)

      } else {
        "0.0" + "\t" + "0.0" + "\t" + "0.0" + "\t" + "0.0" + "\t" + "0.0"
      })
      + "\n"))
    out_stream.close()
  }
  def writeTechnology(eroi_min: List[Double], inclusion: List[Cell] = List()) {
    eroi_min.map(e => writeTechnology(e, inclusion))
  }
  def writeTechnology(eroi_min: Double, inclusion: List[Cell]) {
    println("Write Technology " + eroi_min)
    // 1 : Wind
    // 2 : PV
    // 3 : CSP
    // 4 : Wind + PV
    // 5 : Wind + CSP
    def indexTechnology(c: Cell, e: Double): String = {
      val res =
        (if (OnshoreWindTechnology.netYearlyProduction(c, e).value > 0 || OffshoreWindTechnology.netYearlyProduction(c, e).value > 0) 1 else 0) +
          (if (PVMono.netYearlyProduction(c, e).value > 0 && PVMono.eroi(c, e) > CSPTowerStorage12h.eroi(c, e)) 10 else 0) +
          (if (CSPTowerStorage12h.netYearlyProduction(c, e).value > 0 && PVMono.eroi(c, e) < CSPTowerStorage12h.eroi(c, e)) 100 else 0)
      indexToString(res)
    }
    def indexToString(res: Int): String = {
      if (res == 1) "2.0" else if (res == 10) "4.0" else if (res == 100) "6.0" else if (res == 11) "8.0" else if (res == 101) "10.0" else "0.0"
    }

    val out_stream = new java.io.PrintStream(new java.io.FileOutputStream("../WindPotentialPython/wind_solar_grid/technology" + "_" + eroi_min.toInt))
    cells.map(c => out_stream.print(c.center.latitude.toDegrees + "\t" + c.center.longitude.toDegrees +
      "\t" +
      (if ((inclusion.isEmpty || inclusion.contains(c))) indexTechnology(c, eroi_min) else "0.0")
      + "\n"))
    out_stream.close()

  }

  def printArea(list: List[Cell], factor: Double = 1 / 1E3, sep: String = "\t", header: Boolean = true) {
    if (header)
      println("Total" + "\t" + "Onshore" + "\t" + "Offshore" + "\t" + "Slope > 2%" + "\t" + "Slope > 30%" + "\t" + "Protected"
        + "\t" + "Suitable PV" + "\t" + "Suitable CSP" + "\t" + "Suitable Onshore Wind" + "\t" + "Suitable Offshore Wind" + "\t" + "Sparse " + "\t" + "Forest" + "\t" + "Croplands" + "\t" +
        "Shrubland" + "\t" + "MosaicVegetationCroplands" + "\t" + "MosaicGrasslandForestShrubland" + "\t" + "Urban" + "\t" + "Flooded + Waters + Ice" + "\t" + "Offshore, distance to coast < 5 Nm"
        + "\t" + "Offshore, distance to coast 5 - 20 Nm" + "\t" + "Offshore, distance to coast > 20 Nm" + "\t" + "Offshore, water depth > 1000 m")
    val onshore = list.filter(_.onshore)
    val offshore = list.filter(_.offshoreEEZ)

    pr(list.map(_.area.toSquareKilometers).sum * factor, sep)
    pr(onshore.map(_.area.toSquareKilometers).sum * factor, sep)
    pr(offshore.map(_.area.toSquareKilometers).sum * factor, sep)

    pr(onshore.map(i => i.area.toSquareKilometers * i.slope.slope_geq(2, true)).sum * factor, sep)
    pr(onshore.map(i => i.area.toSquareKilometers * i.slope.slope_geq(30, true)).sum * factor, sep)
    pr(onshore.map(i => i.protectedA.toSquareKilometers).sum * factor, sep)
    pr(onshore.map(c => PVMono.suitabilityFactor(c) * c.area.toSquareKilometers).sum * factor, sep)
    pr(onshore.map(c => CSPParabolic.suitabilityFactor(c) * c.area.toSquareKilometers).sum * factor, sep)
    pr(onshore.map(c => OnshoreWindTechnology.suitabilityFactor(c) * c.area.toSquareKilometers).sum * factor, sep)
    pr(offshore.map(c => OffshoreWindTechnology.suitabilityFactor(c) * c.area.toSquareKilometers).sum * factor, sep)

    pr(onshore.map(g => g.area(SparseVegetation) + g.area(Grassland) + g.area(BareAreas)).foldLeft(SquareKilometers(0))(_ + _).toSquareKilometers * factor, sep)
    pr(onshore.map(g => g.area(Forests)).foldLeft(SquareKilometers(0))(_ + _).toSquareKilometers * factor, sep)
    pr(onshore.map(g => g.area(CropLands)).foldLeft(SquareKilometers(0))(_ + _).toSquareKilometers * factor, sep)
    pr(onshore.map(g => g.area(Shrubland)).foldLeft(SquareKilometers(0))(_ + _).toSquareKilometers * factor, sep)
    pr(onshore.map(g => g.area(MosaicVegetationCroplands)).foldLeft(SquareKilometers(0))(_ + _).toSquareKilometers * factor, sep)
    pr(onshore.map(g => g.area(MosaicGrasslandForestShrubland)).foldLeft(SquareKilometers(0))(_ + _).toSquareKilometers * factor, sep)
    pr(onshore.map(g => g.area(UrbanAreas)).foldLeft(SquareKilometers(0))(_ + _).toSquareKilometers * factor, sep)
    pr(onshore.map(g => g.area(FloodedAreas) + g.area(WaterBodies) + g.area(Ice)).foldLeft(SquareKilometers(0))(_ + _).toSquareKilometers * factor, sep)

    pr(offshore.filter(_.distanceToCoast.toNauticalMiles < 5).map(_.area).foldLeft(SquareKilometers(0))(_ + _).toSquareKilometers * factor, sep)
    pr(offshore.filter(c => c.distanceToCoast.toNauticalMiles >= 5 && c.distanceToCoast.toNauticalMiles <= 20).map(_.area).foldLeft(SquareKilometers(0))(_ + _).toSquareKilometers * factor, sep)
    pr(offshore.filter(_.distanceToCoast.toNauticalMiles > 20).map(_.area).foldLeft(SquareKilometers(0))(_ + _).toSquareKilometers * factor, sep)
    pr(offshore.filter(_.waterDepth.toMeters > 1000).map(_.area).foldLeft(SquareKilometers(0))(_ + _).toSquareKilometers * factor, sep)

    println()
  }

  def pr(s: String, sep: String) {
    print(s + sep)
  }
  def pr(s: Double, sep: String) {
    print(s + sep)
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
    val keDissipation: Irradiance,
    val optimalCD: Map[Double, (Velocity, Double)]) {

  override def toString() = "Grid Object center : " + center

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

  def isCountry(c: String) = country.equals(c); //country.isCountry(c); 
  def isCountry(c: List[String]) = c.contains(country)
  def proportion(c: String): Double = if (isCountry(c)) 1.0 else 0.0;
  def proportion(c: List[String]): Double = if (isCountry(c)) 1.0 else 0.0;
  def area(c: String): Area = proportion(c) * area; def area(c: List[String]): Area = proportion(c) * area

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
      GeoPoint(Degrees(l(0).toDouble), Degrees(l(1).toDouble)),
      resolution,
      WattsPerSquareMeter(l(2).toDouble / 24 * 1000), WattsPerSquareMeter(l(3).toDouble / 24 * 1000),
      SlopeGradients(l, 4, 11),
      lcs(l, 13),
      l(12).toDouble / 100.0,
      //Country(l(38), countryList(l, 161)),
      l(38),
      Meters(l(36).toDouble), Kilometers(l(37).toDouble),
      new WindProfile(MetersPerSecond(l(39).toDouble), l(40).toDouble, Meters(71)),
      new WindProfile(MetersPerSecond(l(41).toDouble), l(42).toDouble, Meters(125)),
      WattsPerSquareMeter(l(43).toDouble),
      if (l.size > 44 + 1 && !l(44).equals("")) (for (e <- (0 until eroi_min.size); if (l(e * 3 + optiIndex + 2).toBoolean)) yield (eroi_min(e), (MetersPerSecond(l(e * 3 + optiIndex).toDouble), l(e * 3 + optiIndex + 1).toDouble))).toMap
      else Map())
  }
  def countryList(l: Array[String], countryIndex: Int): List[(String, Double)] = {
    if (l.size > countryIndex + 1) {
      val xs = (countryIndex until l.size).map(i => l(i).toString).filter(i => !i.equals("")).toList
      xs.toList.distinct.map(x => (x.toString, xs.count(_ == x.toString) / 9.0))
    } else List()
  }
}