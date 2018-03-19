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
  def main(args: Array[String]): Unit = {

    val grid = _0_1deg
    println(areaList(grid.cells.filter(_.protected_area)))

    val countries = Helper.getLines("../countries_WWS2050", "\t").map(_(0))
    val wp = 239.1751749
    val efficiency = wp / 1000
    countries.map(c => {
      val x = grid.country(c)
      //  println(x(0).ghi.toWattsPerSquareMeter)
      println(c + "\t" + areaList(x).toSquareKilometers + "\t" +
        x.map(_.suitableArea.toSquareKilometers).sum + "\t" +
        x.map(_.installedPV.toMegawatts).sum + "\t" +
        x.map(_.pvPotential.toMegawatts).sum + "\t"
        + x.map(i => i.ghi.toWattsPerSquareMeter * efficiency / wp).sum / x.size)
    })
  }
  def area_min_ghi(grid: SolarGrid, c: String) {
    val country = grid.country(c)
    if (country.size == 0) println(c + "\t" + "---")
    else println(c + "\t" + areaList(country.filter(i => i.ghi.value * 24 >= 4000)).toSquareKilometers
      + "\t" + areaList(country.filter(i => i.ghi.value * 24 >= 5000)).toSquareKilometers
      + "\t" + areaList(country.filter(_.ghi.value * 24 >= 6000)).toSquareKilometers)
  }

  def printMeanGHI(c: String, grid: SolarGrid) {
    val cells = grid.country(c)
    println(c + "\t" + cells.map(_.ghi.toWattsPerSquareMeter).sum / cells.size)
  }
  def printArea(list: List[SolarCell]) {

    println(list.size)
    println("Total " + areaList(list).toSquareKilometers / 1E6)
    println("Slope > 45 " + list.map(i => i.area.toSquareKilometers * i.slope_geq45).sum / 1E6)
    println("Slope > 0.5 " + list.map(i => i.area.toSquareKilometers * i.slope_geq0_5).sum / 1E6)

    println("Suitable " + list.map(_.suitableArea.toSquareKilometers).sum / 1E6)
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
