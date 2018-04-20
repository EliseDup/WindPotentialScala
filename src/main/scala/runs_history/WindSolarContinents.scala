package runs_history

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
import solar_energy.SolarCell
import solar_energy.SolarTechnology
import solar_energy.PVMono
import solar_energy.PVPoly
import solar_energy.CSPParabolic
import solar_energy.SolarGrid

object WindSolarContinents {
  import solar_energy.SolarUtils._
  import DayMonth._
  import PlotHelper._
  import solar_energy.SolarPotential._
  import solar_energy.SolarGrid._
  import Helper._
  import wind_energy.WindFarmEnergyInputs._
  import solar_energy.SolarPower._
  import solar_energy.CSP._

  def main(args: Array[String]): Unit = {
printAreaRepartition(getLines("countries_students").map(_(0)))
  }
  def plotComparisonByContinents {
    // Load solar and wind grid
    val solar = _0_5deg
    val eroi_min = (2 until 40).map(_ * 0.5).toList
    val wind = new WorldGrid("runs_history/wind_2017/results_wind_2017", Degrees(0.75), eroi_min, 34, 47, true, false)
    val continents = countriesByContinent
    println("Missing Countries in Wind Model")
    continents.map(c => c._2.map(i => if (wind.country(i).isEmpty) println(i)))
    println("Missing Countries in Solar Model")
    continents.map(c => c._2.map(i => if (solar.country(i).isEmpty) println(i)))
    plotTotal(solar, wind)
    continents.map(c => plotSolarWind(solar, wind, c))
  }
  def plotSolarWind(solar: SolarGrid, wind: WorldGrid, continent: (String, List[String])) {
    val eroiSolar = listEROISolar(solar.countries(continent._2), PVMono)
    val eroiWind = listEROIWind(wind.countries(continent._2))
    plotXY(List(eroiSolar, eroiWind), legend = true, xLabel = "Potential " + continent._1 + " [EJ/year]", yLabel = "EROI", title = "SolarWind_" + continent._1)
  }
  def plotTotal(solar: SolarGrid, wind: WorldGrid) {
    plotXY(List(listEROISolar(solar.cells, PVMono), listEROISolar(solar.cells, PVPoly), listEROIWind(wind.grids)), legend = true, xLabel = "Potential [EJ/year]", yLabel = "EROI", title = "SolarWind_World")
  }

  def listEROISolar(g: List[SolarCell], tech: SolarTechnology) = {
    val res = Helper.listValueVSCumulated(g.filter(g => g.potential(tech).value > 0 && g.eroi(tech) >= 1).map(g => (g.eroi(tech), (g.potential(tech) * Hours(365 * 24)).to(Exajoules))))
    (res._1, res._2, tech.name)
  }

  def listEROIWind(cells: List[GridCell]) = {
    WindPotential().eroiFunction(cells, 1, true, "Wind")
  }

  // Print area repartition 
  def printAreaRepartition(countries: List[String]) {
    val solar = _0_5deg
    println("Solar")
    printAreaSolar("Total",solar.cells, 1E6, "\t", true)
    countries.map(c => printAreaSolar(c,solar.country(c), 1, "\t", false))
    println("Wind")
    val eroi_min = (2 until 40).map(_ * 0.5).toList
    val wind = new WorldGrid("runs_history/wind_2017/results_wind_2017", Degrees(0.75), eroi_min, 34, 47, true, false)
    printAreaWind("Total",wind.grids.filter(_.onshore), 1E6, "\t", true)
    countries.map(c => printAreaWind(c,wind.country(c), 1, "\t", false))

  }

  def printAreaSolar(name : String, list: List[SolarCell], factor: Double = 1 / 1E6, sep: String, header: Boolean) {
    if (header)
      println("" + "\t" +"Total" + "\t" + "Slope <= 2%" + "\t" + "Slope <= 30%" + "\t" + "Protected"
        + "\t" + "Suitable PV" + "\t" + "Suitable CSP" + "\t" + "Sparse " + "\t" + "Forest" + "\t" + "Croplands" + "\t" +
        "Shrubland" + "\t" + "MosaicVegetationCroplands" + "\t" + "MosaicGrasslandForestShrubland" + "\t" + "Urban" + "\t" + "Flooded + Waters + Ice")
pr(name,sep)
    pr(areaList(list).toSquareKilometers * factor, sep)
    pr(list.map(i => i.area.toSquareKilometers * i.slope.slope_leq(2, true)).sum * factor, sep)
    pr(list.map(i => i.area.toSquareKilometers * i.slope.slope_leq(30, true)).sum * factor, sep)
    pr(list.filter(_.protected_area).map(i => i.area.toSquareKilometers).sum * factor, sep)
    pr(list.map(_.suitableArea(PVMono).toSquareKilometers).sum * factor, sep)
    pr(list.map(_.suitableArea(CSPParabolic).toSquareKilometers).sum * factor, sep)
    pr(list.map(g => g.area(SparseVegetation) + g.area(Grassland) + g.area(BareAreas)).foldLeft(SquareKilometers(0))(_ + _).toSquareKilometers * factor, sep)
    pr(list.map(g => g.area(Forests)).foldLeft(SquareKilometers(0))(_ + _).toSquareKilometers * factor, sep)
    pr(list.map(g => g.area(CropLands)).foldLeft(SquareKilometers(0))(_ + _).toSquareKilometers * factor, sep)
    pr(list.map(g => g.area(Shrubland)).foldLeft(SquareKilometers(0))(_ + _).toSquareKilometers * factor, sep)
    pr(list.map(g => g.area(MosaicVegetationCroplands)).foldLeft(SquareKilometers(0))(_ + _).toSquareKilometers * factor, sep)
    pr(list.map(g => g.area(MosaicGrasslandForestShrubland)).foldLeft(SquareKilometers(0))(_ + _).toSquareKilometers * factor, sep)
    pr(list.map(g => g.area(UrbanAreas)).foldLeft(SquareKilometers(0))(_ + _).toSquareKilometers * factor, sep)
    pr(list.map(g => g.area(FloodedAreas) + g.area(WaterBodies) + g.area(Ice)).foldLeft(SquareKilometers(0))(_ + _).toSquareKilometers * factor, sep)
    println()
  }
  def printAreaWind(name:String,list: List[GridCell], factor: Double = 1 / 1E6, sep: String, header: Boolean) {
    if (header)
      println("" + "\t" +"Total" + "\t" + "Protected"
        + "\t" + "Suitable Wind" + "\t" + "Sparse " + "\t" + "Forest" + "\t" + "Croplands" + "\t" +
        "Shrubland" + "\t" + "MosaicVegetationCroplands" + "\t" + "MosaicGrasslandForestShrubland" + "\t" + "Urban" + "\t" + "Flooded + Waters + Ice")
pr(name,sep)
    pr(area(list).toSquareKilometers * factor, sep)
    pr(list.map(i => i.protectedArea).sum * factor, sep)
    pr(list.map(_.suitableArea(true).toSquareKilometers).sum * factor, sep)
    pr(list.map(g => g.area(SparseVegetation) + g.area(Grassland) + g.area(BareAreas)).foldLeft(SquareKilometers(0))(_ + _).toSquareKilometers * factor, sep)
    pr(list.map(g => g.area(Forests)).foldLeft(SquareKilometers(0))(_ + _).toSquareKilometers * factor, sep)
    pr(list.map(g => g.area(CropLands)).foldLeft(SquareKilometers(0))(_ + _).toSquareKilometers * factor, sep)
    pr(list.map(g => g.area(Shrubland)).foldLeft(SquareKilometers(0))(_ + _).toSquareKilometers * factor, sep)
    pr(list.map(g => g.area(MosaicVegetationCroplands)).foldLeft(SquareKilometers(0))(_ + _).toSquareKilometers * factor, sep)
    pr(list.map(g => g.area(MosaicGrasslandForestShrubland)).foldLeft(SquareKilometers(0))(_ + _).toSquareKilometers * factor, sep)
    pr(list.map(g => g.area(UrbanAreas)).foldLeft(SquareKilometers(0))(_ + _).toSquareKilometers * factor, sep)
    pr(list.map(g => g.area(FloodedAreas) + g.area(WaterBodies) + g.area(Ice)).foldLeft(SquareKilometers(0))(_ + _).toSquareKilometers * factor, sep)
    println()
  }

  def pr(s: String, sep: String) {
    print(s + sep)
  }
  def pr(s: Double, sep: String) {
    print(s + sep)
  }
}