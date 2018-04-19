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
  // Load S
    val solar = _0_5deg

    val eroi_min = (2 until 40).map(_ * 0.5).toList
    val wind = new WorldGrid("runs_history/wind_2017/results_wind_2017", Degrees(0.75), eroi_min, 34, 47, true, false)

    val continents = countriesByContinent
    println("Missing Countries in Wind Model")
    continents.map(c => c._2.map(i => if(wind.country(i).isEmpty) println(i)))
    println("Missing Countries in Solar Model")
    continents.map(c => c._2.map(i => if(solar.country(i).isEmpty) println(i)))
   plotTotal
    // continents.map(c => plotSolarWind(c))
    def plotSolarWind(continent: (String, List[String])) {
      val eroiSolar = listEROISolar(solar.countries(continent._2), PVMono)
      val eroiWind = listEROIWind(wind.countries(continent._2))
      plotXY(List(eroiSolar,eroiWind), legend=true, xLabel = "Potential "+continent._1 +" [EJ/year]", yLabel = "EROI", title="SolarWind_"+continent._1)
    }
    def plotTotal {
      plotXY(List(listEROISolar(solar.cells,PVMono),listEROIWind(wind.grids)),xLabel = "Potential [EJ/year]", yLabel = "EROI", title="SolarWind_World")
    }

  }

  def listEROISolar(g: List[SolarCell], tech: SolarTechnology) = {
    val res = Helper.listValueVSCumulated(g.filter(g => g.potential(tech).value > 0 && g.eroi(tech) >= 1).map(g => (g.eroi(tech), (g.potential(tech) * Hours(365 * 24)).to(Exajoules))))
    (res._1, res._2, "Solar")
  }

  def listEROIWind(cells: List[GridCell]) = {
    WindPotential().eroiFunction(cells, 1, true, "Wind")
  }

}