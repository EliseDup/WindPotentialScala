package runs_history

import utils._
import wind_solar._
import java.io._
import squants.energy._
import squants.time._
import wind_energy.OnshoreWindTechnology
import wind_energy.OffshoreWindTechnology
import solar_energy.PVMono
import solar_energy.CSPTowerStorage12h

object Locomotion {
  import Helper._
  import PlotHelper._

  def main(args: Array[String]): Unit = { 
   
    val grid = Grid()
    
    val tech = List(OnshoreWindTechnology, OffshoreWindTechnology, PVMono, CSPTowerStorage12h)
    tech.map(t => printResults(grid.cells, t, t.name+"_world"))
    tech.map(t => printResults(grid.country("Spain"), t, t.name+"_Spain"))
    tech.map(t => printResults(grid.eu28, t, t.name+"_eu28"))
    
  }

  def printResults(grid: List[Cell], tech: RenewableTechnology, output_file: String = "") {
    val out_stream = new java.io.PrintStream(new java.io.FileOutputStream(if (output_file.isEmpty()) tech.name else output_file))
    out_stream.print("CF" + "\t" + "TWe" + "\t" + "TotalArea[km^2]" + "\t" + "SuitableArea[km^2]" + "\t" + "InstalledCapacity[MWi]" + "\n")

    val sorted = grid.filter(c => tech.suitabilityFactor(c) > 0).filter(c => tech.potential(c, 1).value>0).sortBy(c => tech.capacityFactor(c, 1)).reverse
    sorted.map(c =>
      out_stream.print(tech.capacityFactor(c, 1) + "\t" + tech.potential(c, 1).to(Terawatts) + "\t" + c.area.toSquareKilometers
        + "\t" + c.area.toSquareKilometers * tech.suitabilityFactor(c) + "\t" + tech.ratedPower(c, 1).to(Megawatts) + "\n"))
  }

}