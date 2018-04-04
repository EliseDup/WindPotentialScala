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
    println(SlopeGradients.degreesToPercent(Degrees(26.6)))
    println(SlopeGradients.degreesToPercent(Degrees(45)))
    println(SlopeGradients.percentToDegrees(2.1).toDegrees)
    println(SlopeGradients.percentToDegrees(50).toDegrees)
    println(SlopeGradients.percentToDegrees(100).toDegrees)
    
    PlotHelper.plotXY((1 to 100).toList.map(_.toDouble), (1 to 100).toList.map(i => SlopeGradients.percentToDegrees(i.toDouble).toDegrees))
    PlotHelper.plotXY((1 to 45).toList.map(_.toDouble), (1 to 45).toList.map(i => SlopeGradients.degreesToPercent(Degrees(i))))
    
 val grid = _0_5deg
 val classes = SlopeGradients.classes.map(_._2)
 val list = classes.map(c => grid.cells.map(i => i.area.toSquareKilometers * i.slope.slope_leq(c)).sum/1E6)
 PlotHelper.plotXY(classes,list)
   PlotHelper.cumulativeDensity(grid.cells.map(_.slope.total))
    val sm = List(1.3, 1.7, 2, 2.3, 2.7)
    val dni = (2000 to 2800).map(_.toDouble).toList

    val res = sm.map(i => (dni.map(fullLoadHours(_, i) / 8760.0), dni, i.toString))
    PlotHelper.plotXY(res, legend = true)

    val res2 = sm.map(i => -0.0371 * i * i + 0.4171 * i - 0.0744)
    PlotHelper.plotXY(sm, res2)

    val res3 = dni.map(i => 2.5717 * i - 694)
    PlotHelper.plotXY(dni, res3)

    /*    
    val grid = _0_5deg
     PlotHelper.cumulativeDensity(grid.cells.map(_.dni.toWattsPerSquareMeter))
   
    
    val ghi = listValueVSCumulated(grid.cells.map(c => (c.ghi.toWattsPerSquareMeter,c.area.toSquareKilometers/1E6)))
    val sui = listValueVSCumulated(grid.cells.map(c => (c.ghi.toWattsPerSquareMeter,c.suitableArea.toSquareKilometers/1E6)))
       
    plotXY(List( (ghi._1,ghi._2,"Total"),(sui._1,sui._2,"Suitable")), yLabel = "GHI [W/m2]", xLabel = "Cumulated Area [Millions km2]")
    */
    //plotEROI(grid.cells, PVPoly)
    //plotEROI(grid.cells, PVMono)
    //plotEROI(grid.cells, CSPParabolic)

  }

  def printMeanGHI(c: String, grid: SolarGrid) {
    val cells = grid.country(c)
    println(c + "\t" + cells.map(_.ghi.toWattsPerSquareMeter).sum / cells.size)
  }

  def printArea(list: List[SolarCell], factor: Double = 1 / 1E6) {

    println(list.size)
    println("Total " + areaList(list).toSquareKilometers * factor)
   // println("Slope > 45 " + list.map(i => i.area.toSquareKilometers * i.slope.cl8/100).sum * factor)
   // println("Slope > 0.5 " + list.map(i => i.area.toSquareKilometers * i.slope.cl1/100).sum * factor)

    println("Suitable " + list.map(_.suitableArea.toSquareKilometers).sum * factor)
    println("Sparse " + list.map(g => g.area(SparseVegetation) + g.area(Grassland) + g.area(BareAreas)).foldLeft(SquareKilometers(0))(_ + _).toSquareKilometers * factor)
    println("Forest " + list.map(g => g.area(Forests)).foldLeft(SquareKilometers(0))(_ + _).toSquareKilometers * factor)
    println("Croplands " + list.map(g => g.area(CropLands)).foldLeft(SquareKilometers(0))(_ + _).toSquareKilometers * factor)
    println("Shrubland " + list.map(g => g.area(Shrubland)).foldLeft(SquareKilometers(0))(_ + _).toSquareKilometers * factor)
    println("MosaicVegetationCroplands " + list.map(g => g.area(MosaicVegetationCroplands)).foldLeft(SquareKilometers(0))(_ + _).toSquareKilometers * factor)
    println("MosaicGrasslandForestShrubland" + list.map(g => g.area(MosaicGrasslandForestShrubland)).foldLeft(SquareKilometers(0))(_ + _).toSquareKilometers * factor)
    println("Urban " + list.map(g => g.area(UrbanAreas)).foldLeft(SquareKilometers(0))(_ + _).toSquareKilometers * factor)
    println("Flooded + Waters + Ice " + list.map(g => g.area(FloodedAreas) + g.area(WaterBodies) + g.area(Ice)).foldLeft(SquareKilometers(0))(_ + _).toSquareKilometers * factor)
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

  def plotEROI(g: List[SolarCell], tech: SolarTechnology) = {
    val res = Helper.listValueVSCumulated(g.filter(g => g.potential(tech).value > 0 && g.eroi(tech) >= 1).map(g => (g.eroi(tech), (g.potential(tech) * Hours(365 * 24)).to(TerawattHours))))
    PlotHelper.plotXY(List((res._1, res._2, "PV")), xLabel = "Potential [TWh/year]", yLabel = "EROI")
    //  PlotHelper.cumulativeDensity(List( (g.map(c => (c.pvPotential *Hours(365*24)).to(TerawattHours)),"PV")), yLabel="PV Potential [TWh]")
    //  PlotHelper.cumulativeDensity(g.map(c => (c.cspPotential *Hours(365*24)).to(TerawattHours)))
  }

}
