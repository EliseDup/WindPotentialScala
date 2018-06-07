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
import wind_energy.WindPotential

object Test {

  import SolarUtils._
  import DayMonth._
  import PlotHelper._
  import SolarPotential._
  import SolarGrid._
  import Helper._
  import wind_energy.WindFarmEnergyInputs._
  import SolarPower._
  import CSPParabolic._

  def main(args: Array[String]): Unit = {
     
    val grid = _0_5deg
 plotXY(List(listEROI(grid.cells, PVPoly), listEROI(grid.cells, PVMono), listEROI(grid.cells, CSPParabolic), listEROI(grid.cells, CSPParabolicStorage12h), listEROI(grid.cells, CSPTowerStorage12h)),
      xLabel = "Potential [EJ/year]", yLabel = "EROI", legend = true)

  }

  def pr(s: String) = print(s + "\t")
  def pr(s: Double) = print(s + "\t")
  def listEROIVSGHI(tech: SolarTechnology, ghi: List[Double], ratedPower: Power) = {
    val prodPerArea = ghi.map(i => i * 365 * 24 * tech.designEfficiency * tech.performanceRatio)
    val area_17 = ratedPower.toWatts / (1000 * tech.designEfficiency)
    val prodPerYear = prodPerArea.map(i => WattHours(i * area_17))
    val ee = prodPerYear.map(i => tech.ee.embodiedEnergy(ratedPower, i))
    prodPerYear.map(i => (i * 30) / tech.ee.embodiedEnergy(ratedPower, i))
  }

  def printMeanGHI(c: String, grid: SolarGrid) {
    val cells = grid.country(c)
    println(c + "\t" + cells.map(_.ghi.toWattsPerSquareMeter).sum / cells.size)
  }

  def printArea(list: List[SolarCell], factor: Double = 1 / 1E6) {

    println(list.size)
    println("Total " + "\t" + areaList(list).toSquareKilometers * factor)
    println("Slope <= 2% " + "\t" + list.map(i => i.area.toSquareKilometers * i.slope.slope_leq(2, true)).sum * factor)
    println("Slope <= 30% " + "\t" + list.map(i => i.area.toSquareKilometers * i.slope.slope_leq(30, true)).sum * factor)
    println("Protected" + "\t" + list.filter(_.protected_area).map(i => i.area.toSquareKilometers).sum * factor)
    println("Suitable PV" + "\t" + list.map(_.suitableArea(PVMono).toSquareKilometers).sum * factor)
    println("Suitable CSP" + "\t" + list.map(_.suitableArea(CSPParabolic).toSquareKilometers).sum * factor)

    println("Sparse " + "\t" + list.map(g => g.area(SparseVegetation) + g.area(Grassland) + g.area(BareAreas)).foldLeft(SquareKilometers(0))(_ + _).toSquareKilometers * factor)
    println("Forest " + "\t" + list.map(g => g.area(Forests)).foldLeft(SquareKilometers(0))(_ + _).toSquareKilometers * factor)
    println("Croplands " + "\t" + list.map(g => g.area(CropLands)).foldLeft(SquareKilometers(0))(_ + _).toSquareKilometers * factor)
    println("Shrubland " + "\t" + list.map(g => g.area(Shrubland)).foldLeft(SquareKilometers(0))(_ + _).toSquareKilometers * factor)
    println("MosaicVegetationCroplands " + "\t" + list.map(g => g.area(MosaicVegetationCroplands)).foldLeft(SquareKilometers(0))(_ + _).toSquareKilometers * factor)
    println("MosaicGrasslandForestShrubland" + "\t" + list.map(g => g.area(MosaicGrasslandForestShrubland)).foldLeft(SquareKilometers(0))(_ + _).toSquareKilometers * factor)
    println("Urban " + "\t" + list.map(g => g.area(UrbanAreas)).foldLeft(SquareKilometers(0))(_ + _).toSquareKilometers * factor)
    println("Flooded + Waters + Ice " + "\t" + list.map(g => g.area(FloodedAreas) + g.area(WaterBodies) + g.area(Ice)).foldLeft(SquareKilometers(0))(_ + _).toSquareKilometers * factor)
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
  def listEROI(g: List[SolarCell], tech: SolarTechnology) = {
    val res = Helper.listValueVSCumulated(g.filter(g => g.potential(tech).value > 0 && g.eroi(tech) >= 1).map(g => (g.eroi(tech), (g.potential(tech) * Hours(365 * 24)).to(Exajoules))))
    (res._1, res._2, tech.name)
  }
  def listEROI(g: List[SolarCell], tech: SolarTechnology, name: String) = {
    val res = Helper.listValueVSCumulated(g.filter(g => g.potential(tech).value > 0 && g.eroi(tech) >= 1).map(g => (g.eroi(tech), (g.potential(tech) * Hours(365 * 24)).to(Exajoules))))
    (res._1, res._2, name)
  }
  def plotEROI(g: List[SolarCell], tech: SolarTechnology) = {
    val res = Helper.listValueVSCumulated(g.filter(g => g.potential(tech).value > 0 && g.eroi(tech) >= 1).map(g => (g.eroi(tech), (g.potential(tech) * Hours(365 * 24)).to(Exajoules))))
    PlotHelper.plotXY(List((res._1, res._2, tech.name)), xLabel = "Potential " + tech.name + "[EJ/year]", yLabel = "EROI")
    //  PlotHelper.cumulativeDensity(List( (g.map(c => (c.pvPotential *Hours(365*24)).to(TerawattHours)),"PV")), yLabel="PV Potential [TWh]")
    //  PlotHelper.cumulativeDensity(g.map(c => (c.cspPotential *Hours(365*24)).to(TerawattHours)))
  }

}
