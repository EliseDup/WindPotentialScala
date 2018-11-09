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
   
    val dni = (1500 to 3500).map(_.toDouble).toList
    val sm = List(2.5, 3, 3.5, 4)
    plotXY(sm.map(s => (dni, dni.map(d => CSPParabolicStorage12h.eroi(WattsPerSquareMeter(d / 8.76), s)), s.toString)), legend = true, xLabel = "DNI [kWh/m2/year]", yLabel = "EROI")

    val sm2 = (100 to 600).map(_ * 0.01).toList
    val dni2 = List(2000, 2500, 3000)

    plotXY(dni2.map(d => (sm2, sm2.map(s => CSPParabolicStorage12h.eroi(WattsPerSquareMeter(d / 8.76), s)), d.toString)), legend = true, xLabel = "SM", yLabel = "EROI")

    //plotResultsForPaper
    //plotBestSM(CSPTowerStorage12h)

    val grid = _0_5deg_total
    //  grid.writePotential(0)
    grid.writePotential(1)
    /*     grid.writePotential(3)
    grid.writePotential(5)
    grid.writePotential(7)
    grid.writePotential(9)
      val g = grid.cells
    val techs = List(PVPoly, PVMono, CSPParabolic, CSPParabolicStorage12h, CSPTowerStorage12h)
    
    techs.map(t => println(t.name + "\t" + potential(grid.cells, List(t), 1).to(Exajoules)))
    
    val pot = potential(grid.cells, techs, 1)
    val list = Helper.listValueVSCumulated(g.filter(g => g.netYearlyProduction(techs).value > 0 && g.eroi(techs) >= 1).map(g => (g.eroi(techs), (g.netYearlyProduction(techs)).to(Exajoules))))
    val x = (1 to 20).map(_*0.05).toList
    x.map(i => {
      val quantile = list._1.zipWithIndex.filter(j => j._1 >= i*pot.to(Exajoules)).minBy(_._1)
      println(i*100 + " % " + quantile._1 + "\t" + list._2(quantile._2))
    })*/
    print("-End-")

  }

  def plotBestSM(tech: CSP) {
    val dni = (50 to 450).map(_.toDouble).toList
    val eroi = tech.sm_range.map(i => (dni, dni.map(j => tech.eroi(WattsPerSquareMeter(j), i)), i.toString))
    plotXY(eroi, legend = true, xLabel = "DNI [W/m2]", yLabel = "EROI")
    val eff = tech.sm_range.map(i => (dni, dni.map(j => tech.efficiency(WattsPerSquareMeter(j), i) * 100), i.toString))
    plotXY(eff, legend = true, xLabel = "DNI [W/m2]", yLabel = "Efficiency [%]")
    val sm_eroi = (dni, dni.map(j => tech.max_eroi_sm(WattsPerSquareMeter(j))), "Max EROI")
    val sm_eff = (dni, dni.map(j => tech.max_efficiency_sm(WattsPerSquareMeter(j))), "Max efficiency")
    plotXY(List(sm_eroi, sm_eff), legend = true, xLabel = "DNI [W/m2]", yLabel = "Optimal Solar Multiple")
  }

  def plotResultsForPaper {
    val grid = _0_5deg
    val techs = List(PVPoly, PVMono, CSPParabolic, CSPParabolicStorage12h, CSPTowerStorage12h)

    plotEROI(grid.cells, techs, "potential")
    plotPotentialByTechnology(grid, List(PVPoly, PVMono), "potential_PV")
    plotPotentialByTechnology(grid, List(CSPParabolic, CSPParabolicStorage12h, CSPTowerStorage12h), "potential_CSP")
    plotPotentialByTechnology(grid, techs, "potential_allTech")

    plotPotentialByContinents(grid, techs, "potential_by_continent")

    plotPotentialVSArea(grid.cells, techs, "potential_area")
    plotPotentialVSAreaByContinents(grid, List(PVPoly), "potentialByContinentArea_PV")
    plotPotentialVSAreaByContinents(grid, List(CSPParabolicStorage12h), "potentialByContinentArea_CSP")

    // Plot potential for the usual / optimal solar multiple
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
  def plotPotentialByTechnology(grid: SolarGrid, techs: List[SolarTechnology], title: String) {
    plotXY(techs.map(t => listEROI(grid.cells, t)), xLabel = "Net Potential [EJ/year]", yLabel = "EROI", legend = true, title = title)
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