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
  import Helper._
  import wind_energy.WindFarmEnergyInputs._
  import SolarPower._
  import CSP._

  def main(args: Array[String]): Unit = {
    val grid = _0_5deg
    
    PlotHelper.plotXY(List(listEROI(grid.cells, PVMono),listEROI(grid.cells, PVPoly),listEROI(grid.cells, CSPParabolic)),legend=true, xLabel = "Potential [EJ/year]", yLabel = "EROI")
  
    plotEROI(grid.cells, CSPParabolic)

    val cf = (0 to 40).map(_ / 100.0).toList
    /* PlotHelper.plotXY(List( (cf.map(_*100), 
        cf.map(PVPoly.eroi(_)),"PV Poly 17%"),(cf.map(_*100), 
            cf.map(PVMono.eroi(_)),"PV Mono 24%"),(cf.map(_*100), 
                cf.map(CSPParabolic.eroi(_)),"CSP Parabolic Through")), legend=true, xLabel ="Load Factor [%]", yLabel = "EROI")
  */
    val x = (50 to 400).map(_.toDouble).toList
    PlotHelper.plotXY(List(
      (x.map(_ * 8.76), x.map(i => CSPParabolic.eroi(i)), "Simple"),
      (x.map(_ * 8.76), x.map(i => CSPParabolic.eroi(WattsPerSquareMeter(i), 1.3)), "SM = 1.3"),
      (x.map(_ * 8.76), x.map(i => CSPParabolic.eroi(WattsPerSquareMeter(i), 1.7)), "SM = 1.7"),
      (x.map(_ * 8.76), x.map(i => CSPParabolic.eroi(WattsPerSquareMeter(i), 2)), "SM = 2"),
      (x.map(_ * 8.76), x.map(i => CSPParabolic.eroi(WattsPerSquareMeter(i), 2.3)), "SM = 2.3"),
      (x.map(_ * 8.76), x.map(i => CSPParabolic.eroi(WattsPerSquareMeter(i), 2.7)), "SM = 2.7")),
      legend = true, xLabel = "Mean annual DNI [kWh/m2/y]",
      yLabel = "EROI")

    PlotHelper.plotXY(List(
      (x.map(_ * 8.76), x.map(i => PVMono.eroi(WattsPerSquareMeter(i))), "Mono 24%"),
      (x.map(_ * 8.76), x.map(i => PVPoly.eroi(WattsPerSquareMeter(i))), "Poly 17%")),
      legend = true, xLabel = "Mean annual GHI [kWh/m2/y]",
      yLabel = "EROI")
  }

  def listEROIVSGHI(tech: SolarTechnology, ghi: List[Double], ratedPower: Power) = {
    val prodPerArea = ghi.map(i => i * 365 * 24 * tech.efficiency * tech.performanceRatio)
    val area_17 = ratedPower.toWatts / (1000 * tech.efficiency)
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
  def plotEROI(g: List[SolarCell], tech: SolarTechnology) = {
    val res = Helper.listValueVSCumulated(g.filter(g => g.potential(tech).value > 0 && g.eroi(tech) >= 1).map(g => (g.eroi(tech), (g.potential(tech) * Hours(365 * 24)).to(Exajoules))))
    PlotHelper.plotXY(List((res._1, res._2, tech.name)), xLabel = "Potential " + tech.name + "[EJ/year]", yLabel = "EROI")
    //  PlotHelper.cumulativeDensity(List( (g.map(c => (c.pvPotential *Hours(365*24)).to(TerawattHours)),"PV")), yLabel="PV Potential [TWh]")
    //  PlotHelper.cumulativeDensity(g.map(c => (c.cspPotential *Hours(365*24)).to(TerawattHours)))
  }

}
