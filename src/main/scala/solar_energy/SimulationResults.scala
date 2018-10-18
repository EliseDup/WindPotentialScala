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

    val grid = _0_5deg
   //  plotPotentialByTechnology(grid, List(CSPParabolic, CSPParabolicStorage12h, CSPTowerStorage12h), "potential_CSP")
   val t = CSPParabolicStorage12h
   plotXY(List(listEROI_opt_sm(grid.cells, t,false),
       listEROI_best_CSP(grid.cells, List(t))),  xLabel = "Potential [EJ/year]", yLabel = "EROI", legend = true)
    
    
    print("-End-")

  }

  def plotResultsForPaper {
    val grid = _0_1deg
    val techs = List(PVPoly, PVMono, CSPParabolic, CSPParabolicStorage12h, CSPTowerStorage12h)

    plotEROI(grid.cells, techs, "potential")
    plotPotentialByTechnology(grid, List(PVPoly, PVMono), "potential_PV")
    plotPotentialByTechnology(grid, List(CSPParabolic, CSPParabolicStorage12h, CSPTowerStorage12h), "potential_CSP")
    plotPotentialByContinents(grid, techs, "potential_by_continent")

    plotPotentialVSArea(grid.cells, techs, "potential_area")
    plotPotentialVSAreaByContinents(grid, List(PVPoly), "potentialByContinentArea_PV")
    plotPotentialVSAreaByContinents(grid, List(CSPParabolicStorage12h), "potentialByContinentArea_CSP")

    // Plot potential for the usual / optimal solar multiple
  }

  def plotPotentialByContinents(grid: SolarGrid, techs: List[SolarTechnology], title: String) {
    val countries = countriesByContinent
    val continents = countries.map(c => listEROI(grid.countries(c._2), techs, c._1))
    plotXY(continents, xLabel = "Potential [EJ/year]", yLabel = "EROI", legend = true, title = title)
  }
  def plotPotentialVSAreaByContinents(grid: SolarGrid, techs: List[SolarTechnology], title: String) {
    val countries = countriesByContinent
    val continents = countries.map(c => listEROIVSArea(grid.countries(c._2), techs, c._1))
    plotXY(continents, xLabel = "Cumulated Area [Millions km2]", yLabel = "EROI", legend = true, title = title)
  }
  def plotPotentialByTechnology(grid: SolarGrid, techs: List[SolarTechnology], title: String) {
    plotXY(techs.map(t => listEROI(grid.cells, t)), xLabel = "Potential [EJ/year]", yLabel = "EROI", legend = true, title = title)
  }

  def listEROI(g: List[SolarCell], tech: SolarTechnology) = {
    val res = Helper.listValueVSCumulated(g.filter(g => g.potential(tech).value > 0 && g.eroi(tech) >= 1).map(g => (g.eroi(tech), (g.potential(tech) * Hours(365 * 24)).to(Exajoules))))
    (res._1, res._2, tech.name)
  }
  def listEROI(g: List[SolarCell], techs: List[SolarTechnology], name: String = "") = {
    val res = Helper.listValueVSCumulated(g.filter(g => g.potential(techs).value > 0 && g.eroi(techs) >= 1).map(g => (g.eroi(techs), (g.potential(techs) * Hours(365 * 24)).to(Exajoules))))
    (res._1, res._2, name)
  }
  def listEROIVSArea(g: List[SolarCell], tech: SolarTechnology) = {
    val res = Helper.listValueVSCumulated(g.filter(g => g.potential(tech).value > 0 && g.eroi(tech) >= 1).map(g => (g.eroi(tech), (g.suitableArea(tech)).to(SquareKilometers) / 1E6)))
    (res._1, res._2, tech.name)
  }
  def listEROIVSArea(g: List[SolarCell], techs: List[SolarTechnology], name: String = "") = {
    val res = Helper.listValueVSCumulated(g.filter(g => g.potential(techs).value > 0 && g.eroi(techs) >= 1).map(g => (g.eroi(techs), (g.suitableArea(techs)).to(SquareKilometers) / 1E6)))
    (res._1, res._2, name)
  }
  
   def listEROI_opt_sm(g: List[SolarCell], tech: CSP, opt_sm: Boolean) = {
    val res = Helper.listValueVSCumulated(g.filter(g => tech.yearlyProduction(g.dni, g.panelArea(tech), opt_sm).value > 0 && tech.eroi(g.dni, opt_sm) >= 1).map(g => (tech.eroi(g.dni, opt_sm), (tech.yearlyProduction(g.dni, g.panelArea(tech), opt_sm)).to(Exajoules))))
    (res._1, res._2, if(opt_sm) "Optimal SM" else "Usual SM")
  }
 def listEROI_best_CSP(g: List[SolarCell], techs: List[CSP]) = {
    val res = Helper.listValueVSCumulated(g.filter(g => g.potential(techs).value > 0 && g.eroi(techs) >= 1).map(g => {
      val bestTech = g.bestCSPTechnology(techs)
      (bestTech._1.eroi(g.dni,bestTech._2), bestTech._1.yearlyProduction(g.dni, g.panelArea(bestTech._1), bestTech._2).to(Exajoules))
    }))
    (res._1, res._2, "Best Tech")
  }
  def plotEROI(g: List[SolarCell], tech: SolarTechnology) = plotXY(List(listEROI(g, tech)), xLabel = "Potential " + tech.name + "[EJ/year]", yLabel = "EROI", title = "potential_" + tech.name)
  def plotEROI(g: List[SolarCell], techs: List[SolarTechnology], title: String) = plotXY(List(listEROI(g, techs)), xLabel = "Solar Potential [EJ/year]", yLabel = "EROI", title = title)
  def plotEROIArea(g: List[SolarCell], tech: SolarTechnology) = plotXY(List(listEROIVSArea(g, tech)), xLabel = "Cumulated Area " + tech.name + "[km2]", yLabel = "EROI", title = "potential_area" + tech.name)
  def plotEROIArea(g: List[SolarCell], techs: List[SolarTechnology], title: String) = plotXY(List(listEROIVSArea(g, techs)), xLabel = "Cumulated Area [Millions km2]", yLabel = "EROI", title = title)

  def listPotentialVSArea(g: List[SolarCell], tech: SolarTechnology) = {
    val res = Helper.listCumulatedVSCumulatedBy(g.filter(g => g.potential(tech).value > 0 && g.eroi(tech) >= 1).map(g => (g.eroi(tech), g.suitableArea(tech).to(SquareKilometers) / 1E6, (g.potential(tech) * Hours(365 * 24)).to(Exajoules))))
    (res._1, res._2, tech.name)
  }
  def plotPotentialVSArea(g: List[SolarCell], techs: List[SolarTechnology], title: String) = plotXY(techs.map(tech => listPotentialVSArea(g, tech)), legend = true, xLabel = "Cumulated Area [Millions km2]", yLabel = "Cumulated Potential [EJ/year]", title = title)
  
  
}