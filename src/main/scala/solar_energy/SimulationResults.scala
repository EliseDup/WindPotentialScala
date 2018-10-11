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
    val dnis = (500 to 3000).map(i => WattsPerSquareMeter(i/8.76)).toList
    
     val dni = WattsPerSquareMeter(2800 / 8.76)
   
    println(PVMono.lifeTimeEfficiency(dni))
    println(PVPoly.lifeTimeEfficiency(dni))
    
    val csp = List(CSPParabolic, CSPParabolicStorage12h, CSPTowerStorage12h)
    val area = SquareKilometers(1)
   // val power = Megawatts(1)
    csp.map(x => {
      println(x.name)
      val power = x.designPointIrradiance * x.designEfficiency * area / x.solar_multiple
      println(power.toMegawatts)
      println(x.apertureArea(power))
      println(x.lifeTimeEfficiency(dni))
      println(x.yearlyProduction(dni, power).to(GigawattHours)*30)
      println(x.ee.embodiedEnergyArea(power, x.yearlyProduction(dni, power), x.apertureArea(power)).to(GigawattHours))
      println(x.yearlyProduction(dni, power)/(power*Hours(365*24)))
      println(x.eroi(dni))
    })

    val grid = _0_5deg
   val SP = grid.country("Spain")
   println(SP.map(_.ghi.toWattsPerSquareMeter).sum / SP.size + "\t" + SP.map(_.dni.toWattsPerSquareMeter).sum / SP.size)
    val techs = List(PVPoly, PVMono, CSPParabolic, CSPParabolicStorage12h, CSPTowerStorage12h)
    // grid.write("bat")
    // plotEROI(grid.cells, techs)

    val countries = countriesByContinent
    val continents = countries.map(c => listEROI(grid.countries(c._2), techs, c._1))
    // plotXY(continents, xLabel = "Potential [EJ/year]", yLabel = "EROI", legend = true)

    // plotXY(techs.map(t => listEROI(grid.cells, t)), xLabel = "Potential [EJ/year]", yLabel = "EROI", legend = true)
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