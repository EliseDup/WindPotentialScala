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
    val techs = List(PVPoly, PVMono, CSPParabolic, CSPParabolicStorage12h, CSPTowerStorage12h)
    plotEROI(grid.cells, techs)

    val countries = countriesByContinent
    val continents = countries.map(c => listEROI(grid.countries(c._2), techs, c._1))
    plotXY(continents, xLabel = "Potential [EJ/year]", yLabel = "EROI", legend = true)
   
    //plotXY(List(listEROI(grid.cells, PVPoly), listEROI(grid.cells, PVMono), listEROI(grid.cells, CSPParabolic), listEROI(grid.cells, CSPParabolicStorage12h), listEROI(grid.cells, CSPTowerStorage12h)), xLabel = "Potential [EJ/year]", yLabel = "EROI", legend = true)
    //plotXY(List(listEROI(grid.cells, CSPParabolic), listEROI(grid.cells, CSPParabolicStorage12h)), xLabel = "Potential [EJ/year]", yLabel = "EROI", legend = true)
  }

  def listEROI(g: List[SolarCell], tech: SolarTechnology) = {
    val res = Helper.listValueVSCumulated(g.filter(g => g.potential(tech).value > 0 && g.eroi(tech) >= 1).map(g => (g.eroi(tech), (g.potential(tech) * Hours(365 * 24)).to(Exajoules))))
    (res._1, res._2, tech.name)
  }
  def listEROI(g: List[SolarCell], techs: List[SolarTechnology], name: String = "") = {
    val res = Helper.listValueVSCumulated(g.filter(g => g.potential(techs).value > 0 && g.eroi(techs) >= 1).map(g => (g.eroi(techs), (g.potential(techs) * Hours(365 * 24)).to(Exajoules))))
    (res._1, res._2, name)
  }
  def plotEROI(g: List[SolarCell], tech: SolarTechnology) = plotXY(List(listEROI(g, tech)), xLabel = "Potential " + tech.name + "[EJ/year]", yLabel = "EROI")
  def plotEROI(g: List[SolarCell], techs: List[SolarTechnology]) = plotXY(List(listEROI(g, techs)), xLabel = "Solar Potential [EJ/year]", yLabel = "EROI")
}