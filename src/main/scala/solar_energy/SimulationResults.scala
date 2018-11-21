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

object SimuationResults {

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
printResultsForPaper(1)
    // plotResultsForPaper

    val g = _0_5deg_total
    g.write("optimal_sm")

    print("-End-")

  }

  def plotBestSM(tech: CSP, sm: List[Double]) {
    val dni = (150 to 450).map(_.toDouble).toList
    val eroi = sm.map(i => (dni, dni.map(j => tech.eroi(WattsPerSquareMeter(j), i)), i.toString))
    //plotXY(eroi, legend = true, xLabel = "DNI [W/m2]", yLabel = "EROI")
    val eff = sm.map(i => (dni, dni.map(j => tech.efficiency(WattsPerSquareMeter(j), i) * 100), i.toString))
    //plotXY(eff, legend = true, xLabel = "DNI [W/m2]", yLabel = "Efficiency [%]")
    val sm_eroi = (dni.map(_ * 8.76), dni.map(j => tech.max_eroi_sm(WattsPerSquareMeter(j))), "Max EROI")
    // val sm_eff = (dni, dni.map(j => tech.max_efficiency_sm(WattsPerSquareMeter(j))), "Max efficiency")
    plotXY(List(sm_eroi), xLabel = "DNI [kWh/m2/year]", yLabel = "Optimal SM " + tech.name)
  }

  def plotResultsForPaper {
    val grid = _0_5deg
    val techs = List(PVPoly, PVMono, CSPParabolic, CSPParabolicStorage12h, CSPTowerStorage12h)

    plotEROI(grid.cells, techs, "potential")
    plotPotentialByTechnology(grid.cells, List(PVPoly, PVMono), "potential_PV")
    plotPotentialByTechnology(grid.cells, List(CSPParabolic, CSPParabolicStorage12h, CSPTowerStorage12h), "potential_CSP")
    plotPotentialByTechnology(grid.cells, techs, "potential_allTech")
    plotPotentialByTechnology(grid.eu28, techs, "potential_eu28")

    plotPotentialByContinents(grid, techs, "potential_by_continent")

    plotPotentialVSArea(grid.cells, techs, "potential_area")
    plotPotentialVSAreaByContinents(grid, List(PVPoly), "potentialByContinentArea_PV")
    plotPotentialVSAreaByContinents(grid, List(CSPParabolicStorage12h), "potentialByContinentArea_CSP")

    // Plot potential for the usual / optimal solar multiple
    
  }
  def printResultsForPaper(eroi_min : Double) {
    val grid = _0_5deg
    val techs = List(PVPoly, PVMono, CSPParabolic, CSPParabolicStorage12h, CSPTowerStorage12h)
    val total = potential(grid.cells,techs,eroi_min).to(Exajoules)
    println("Potential All Tech "+ "\t" +  math.round(total))
    techs.map(t => println("Potential "+t.name + "\t" + math.round(potential(grid.cells,List(t),eroi_min).to(Exajoules))))
    
    
    // Tables by continent
    val countries = countriesByContinent
    println(" & Total & CSP & PV & of Global Potential \\"+ "\\")
    countries.map(c => println(c._1 + " & " + 
        math.round(potential(grid.countries(c._2),techs,eroi_min).to(Exajoules)) + " & " + 
        math.round(potential(grid.countries(c._2),List(PVMono,PVPoly),eroi_min).to(Exajoules)) + " & " + 
        math.round(potential(grid.countries(c._2),List(CSPParabolic,CSPParabolicStorage12h,CSPTowerStorage12h),eroi_min).to(Exajoules)) + " & " +
        math.round(potential(grid.countries(c._2),techs,eroi_min).to(Exajoules)/total*100) +
        " \\" + "\\" ))
     println("textbf{Total}" + "&" + math.round(total) + " & " + math.round(potential(grid.cells,List(PVMono,PVPoly),eroi_min).to(Exajoules))+ "& " + 
         math.round(potential(grid.cells,List(CSPParabolic,CSPParabolicStorage12h,CSPTowerStorage12h),eroi_min).to(Exajoules)) + " & " +" \\" + "\\" )
  }

  def plotPotentialByContinents(grid: SolarGrid, techs: List[SolarTechnology], title: String) {
    val countries = countriesByContinent
    val continents = countries.map(c => listEROI(grid.countries(c._2), techs, c._1))
    plotXY(continents, xLabel = "Net Potential [EJ/year]", yLabel = "EROI", legend = true, title = title)
  }
  def plotPotentialVSAreaByContinents(grid: SolarGrid, techs: List[SolarTechnology], title: String) {
    val countries = countriesByContinent
    val continents = countries.map(c => listEROIVSArea(grid.countries(c._2), techs, c._1))
    plotXY(continents, xLabel = "Cumulated Area [Millions km2]", yLabel = "EROI", legend = true, title = title)
  }
  def plotPotentialByTechnology(grid: List[SolarCell], techs: List[SolarTechnology], title: String) {
    plotXY(techs.map(t => listEROI(grid, t)), xLabel = "Net Potential [EJ/year]", yLabel = "EROI", legend = true, title = title)
  }

  def listEROI(g: List[SolarCell], tech: SolarTechnology) = {
    val res = Helper.listValueVSCumulated(g.filter(g => g.netYearlyProduction(tech).value > 0 && g.eroi(tech) >= 1).map(g => (g.eroi(tech), (g.netYearlyProduction(tech)).to(Exajoules))))
    (res._1, res._2, tech.name)
  }
  def listEROI(g: List[SolarCell], techs: List[SolarTechnology], name: String = "") = {
    val res = Helper.listValueVSCumulated(g.filter(g => g.netYearlyProduction(techs).value > 0 && g.eroi(techs) >= 1).map(g => (g.eroi(techs), (g.netYearlyProduction(techs)).to(Exajoules))))
    (res._1, res._2, name)
  }
  def listEROIVSArea(g: List[SolarCell], tech: SolarTechnology) = {
    val res = Helper.listValueVSCumulated(g.filter(g => g.netYearlyProduction(tech).value > 0 && g.eroi(tech) >= 1).map(g => (g.eroi(tech), (g.suitableArea(tech)).to(SquareKilometers) / 1E6)))
    (res._1, res._2, tech.name)
  }
  def listEROIVSArea(g: List[SolarCell], techs: List[SolarTechnology], name: String = "") = {
    val res = Helper.listValueVSCumulated(g.filter(g => g.netYearlyProduction(techs).value > 0 && g.eroi(techs) >= 1).map(g => (g.eroi(techs), (g.suitableArea(techs)).to(SquareKilometers) / 1E6)))
    (res._1, res._2, name)
  }
  def potential(g: List[SolarCell], techs: List[SolarTechnology], eroi_min: Double): Energy = {
    g.filter(i => i.netYearlyProduction(techs).value > 0 && i.eroi(techs) >= eroi_min).map(_.netYearlyProduction(techs)).foldLeft(Exajoules(0))(_ + _)
  }

  def plotEROI(g: List[SolarCell], tech: SolarTechnology) = plotXY(List(listEROI(g, tech)), xLabel = "Net Potential " + tech.name + "[EJ/year]", yLabel = "EROI", title = "potential_" + tech.name)
  def plotEROI(g: List[SolarCell], techs: List[SolarTechnology], title: String) = plotXY(List(listEROI(g, techs)), xLabel = "Net Potential [EJ/year]", yLabel = "EROI", title = title)
  def plotEROIArea(g: List[SolarCell], tech: SolarTechnology) = plotXY(List(listEROIVSArea(g, tech)), xLabel = "Cumulated Area " + tech.name + "[km2]", yLabel = "EROI", title = "potential_area" + tech.name)
  def plotEROIArea(g: List[SolarCell], techs: List[SolarTechnology], title: String) = plotXY(List(listEROIVSArea(g, techs)), xLabel = "Cumulated Area [Millions km2]", yLabel = "EROI", title = title)

  def listPotentialVSArea(g: List[SolarCell], tech: SolarTechnology) = {
    val res = Helper.listCumulatedVSCumulatedBy(g.filter(g => g.potential(tech).value > 0 && g.eroi(tech) >= 1).map(g => (g.eroi(tech), g.suitableArea(tech).to(SquareKilometers) / 1E6, (g.netYearlyProduction(tech)).to(Exajoules))))
    (res._1, res._2, tech.name)
  }
  def plotPotentialVSArea(g: List[SolarCell], techs: List[SolarTechnology], title: String) = plotXY(techs.map(tech => listPotentialVSArea(g, tech)), legend = true, xLabel = "Cumulated Area [Millions km2]", yLabel = "Cumulated Net Potential [EJ/year]", title = title)

}