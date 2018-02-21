package solar_energy

import utils.PlotHelper
import grid.WorldGrid
import squants.energy._
import utils._
import squants.time.Hours
import squants.space._
import squants.radio._
import grid._
import scala.io.Source
import java.io.PrintStream

object Test {
  import SolarUtils._
  import DayMonth._
  import PlotHelper._
  import SolarPotential._
  import SolarGrid._
  def main(args: Array[String]): Unit = {
    println(TonOilEquivalent(9384E6).to(TerawattHours))
    println(TonOilEquivalent(9384E6).to(Exajoules))
    println(TerawattHours(25000).to(Exajoules))
    val grid = _0_5deg
    EROIPV(grid.cells)
    // PlotHelper.histogram(potential)
    PlotHelper.cumulativeDensity(List((grid.cells.map(_.slope45), "45"), (grid.cells.map(_.slope0_5), "0_5")))

    def printArea(grid: SolarGrid) {
      val list = grid.cells
      println(list.size)
      println("Total " + area(list).toSquareKilometers / 1E6)
      println("Suitable " + list.map(_.suitableAreaPV.toSquareKilometers).sum / 1E6)
      println("Sparse " + list.map(g => g.area(SparseVegetation) + g.area(Grassland) + g.area(BareAreas)).foldLeft(SquareKilometers(0))(_ + _).toSquareKilometers / 1E6)
      println("Forest " + list.map(g => g.area(Forests)).foldLeft(SquareKilometers(0))(_ + _).toSquareKilometers / 1E6)
      println("Croplands " + list.map(g => g.area(CropLands)).foldLeft(SquareKilometers(0))(_ + _).toSquareKilometers / 1E6)
      println("Shrubland " + list.map(g => g.area(Shrubland)).foldLeft(SquareKilometers(0))(_ + _).toSquareKilometers / 1E6)
      println("MosaicVegetationCroplands " + list.map(g => g.area(MosaicVegetationCroplands)).foldLeft(SquareKilometers(0))(_ + _).toSquareKilometers / 1E6)
      println("MosaicGrasslandForestShrubland" + list.map(g => g.area(MosaicGrasslandForestShrubland)).foldLeft(SquareKilometers(0))(_ + _).toSquareKilometers / 1E6)
      println("Urban " + list.map(g => g.area(UrbanAreas)).foldLeft(SquareKilometers(0))(_ + _).toSquareKilometers / 1E6)
      println("Flooded + Waters + Ice " + list.map(g => g.area(FloodedAreas) + g.area(WaterBodies) + g.area(Ice)).foldLeft(SquareKilometers(0))(_ + _).toSquareKilometers / 1E6)
    }

    def plotEROI(w: WorldGrid, g: List[GridCell]) {

      PlotHelper.cumulativeDensity(List((g.map(_.irradiance.month(0).toWattsPerSquareMeter), "January"), (g.map(_.irradiance.month(6).toWattsPerSquareMeter), "July")), legend = true,
        xLabel = "% Sites", yLabel = "Monthly Irradiance [W/m²]")
      w.writeGrid("jan_ju")

      val eroiMax1 = Math.ceil(g.map(i => SolarPotential.eff_17.eroi(i, 1, true)).max).toInt
      val eroiMax2 = Math.ceil(g.map(i => SolarPotential.eff_24.eroi(i, 1, true)).max).toInt

      PlotHelper.plotXY(List(
        SolarPotential.eff_17.potential_eroi((2 to 2 * eroiMax1).map(_ * 0.5).toList, true, g, "0.17"),
        SolarPotential.eff_24.potential_eroi((2 to 2 * eroiMax2).map(_ * 0.5).toList, true, g, "0.24")),
        xLabel = "Solar Potential [EJ/year]", yLabel = "EROI",
        legend = true)

    }
    def EROIPV(g: List[SolarCell]) = {
      val res = Helper.listValueVSCumulated(g.filter(g => g.pvPotential.value > 0 && g.eroiPV >= 1).map(g => (g.eroiPV, (g.pvPotential * Hours(365 * 24)).to(TerawattHours))))
      PlotHelper.plotXY(List((res._1, res._2, "PV")), xLabel = "PV Potential [TWh/year]", yLabel = "EROI")
      //  PlotHelper.cumulativeDensity(List( (g.map(c => (c.pvPotential *Hours(365*24)).to(TerawattHours)),"PV")), yLabel="PV Potential [TWh]")
      //  PlotHelper.cumulativeDensity(g.map(c => (c.cspPotential *Hours(365*24)).to(TerawattHours)))
    }
  }
}

object SolarUtils {
  def area(list: List[SolarCell]) = list.map(_.area).foldLeft(SquareKilometers(0))(_ + _)
  def suitableAreaPV(list: List[SolarCell]) = list.map(_.suitableAreaPV).foldLeft(SquareKilometers(0))(_ + _)
  def suitableAreaCSP(list: List[SolarCell]) = list.map(_.suitableAreaPV).foldLeft(SquareKilometers(0))(_ + _)

}
object SolarGrid {
  def _1deg = apply("data_solar/solar_countries_1deg", Degrees(1))
  def _1deg_total = apply("data_solar/1deg_total", Degrees(1))
  def _0_1deg = apply("data_solar/solar_countries_0_1deg", Degrees(0.1))
  def _0_5deg = apply("data_solar/grid_0_5deg", Degrees(0.5))
  def _0_5deg_total = apply("data_solar/solar_0_5deg_total", Degrees(0.5))

  def _0_05deg = apply("data_solar/solar_countries_0_05deg", Degrees(0.05))

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
      c.powerDensity(PVTechnology).toWattsPerSquareMeter + "\t" +
      c.powerDensity(CSPTechnology).toWattsPerSquareMeter + "\n"))
    out_stream.close()
  }
}

class SolarCell(val center: GeoPoint, val resolution: Angle, val ghi: Irradiance, val dni: Irradiance,
    val landCover: LandCoverType, val distanceToCoast: Length, val slope45: Double, val slope0_5: Double,
    val country: String, val protected_area: Boolean = false) {
  val area = Helper.areaRectangle(center, resolution)
  val suitableAreaPV = landCover.solarFactor.mean * area * (1 - slope45)
  val suitableAreaCSP = landCover.solarFactor.mean * area * (1 - slope0_5)
  def area(lcType: LandCoverType): Area = if (lcType.equals(landCover)) area else SquareKilometers(0)

  def powerDensity(tech: Technology) = 1.0 / tech.occupationRatio * (if (tech.directOnly) dni else ghi) * tech.efficiency * tech.performanceRatio

  val pvPotential = suitableAreaPV * powerDensity(PVTechnology)
  val installedPV = suitableAreaPV * PVTechnology.efficiency * WattsPerSquareMeter(1000) / PVTechnology.occupationRatio

  val cspPotential = suitableAreaCSP * powerDensity(CSPTechnology)
  val maxPotential = if (powerDensity(PVTechnology) > powerDensity(CSPTechnology)) pvPotential else cspPotential

  def eroiPV: Double = {
    if (ghi.value == 0 || suitableAreaPV.value == 0) 0.0
    else {
      val out = Hours(25 * 365 * 24) * pvPotential
      out / EmbodiedEnergyPV.inputs(installedPV, out)
    }
  }
}

object SolarCell {
  def apply(line: String, resolution: Angle) = {
    val i = line.split("\t")
println(line)
    new SolarCell(GeoPoint(Degrees(i(1).toDouble), Degrees(i(0).toDouble)), resolution,
      WattsPerSquareMeter(i(2).toDouble / 24 * 1000), WattsPerSquareMeter(i(3).toDouble / 24 * 1000),
      GlobCoverClasses.landCoverType(i(4).toDouble.toInt), Kilometers(i(5).toDouble),
      slope(i(6).toDouble), slope(i(7).toDouble), i(8).toString,
      if (i.size >= 10) i(9).toInt == 1 else false)
  }
  def slope(i: Double) = {
    val res = if (i < 0) 0.0 else i/100.0
    if(res > 1.0){
      print(i)
    }
    res}
}

class Technology(val efficiency: Double, val performanceRatio: Double, val occupationRatio: Double, val directOnly: Boolean)

object PVTechnology extends Technology(0.17, 0.81, 5, false)
object CSPTechnology extends Technology(0.16, 1, 7.5, true)