package runs_history

import utils._
import wind_solar._
import java.io._
import squants.energy._
import squants.time._
import wind_energy._
import solar_energy._

object Locomotion {
  import Helper._
  import PlotHelper._
  import SolarGrid._
  def main(args: Array[String]): Unit = {

    val grid = _0_5deg.cells
    val techs = List(PVMono, PVPoly, CSPTowerStorage12h, CSPParabolicStorage12h, CSPParabolic)
     
    techs.map(t =>  {
      val res = SolarPotential_03_2019.listGrossGEER(grid, t)
      val out_stream = new PrintStream(new java.io.FileOutputStream(res._3+"_grossGEER"))
      (0 until res._1.size).map(i => out_stream.print(res._1(i) + "\t" +res._2(i) + "\n"))
      out_stream.close()
    })
     techs.map(t =>  {
      val res = SolarPotential_03_2019.listGEER(grid, t)
      val out_stream = new PrintStream(new java.io.FileOutputStream(res._3+"_netGEER"))
      (0 until res._1.size).map(i => out_stream.print(res._1(i) + "\t" +res._2(i) + "\n"))
      out_stream.close()
    })
  // plotPotentialByTechnology(grid.cells, techs, "potential_allTech")

    //println( (Terawatts(1.47)*Hours(365*25)).to(Exajoules))
    /*   val grid = Grid()

    val tech = List(OnshoreWindTechnology, OffshoreWindTechnology, PVMono, PVPoly, CSPTowerStorage12h)
    tech.map(t => printResults(grid.cells, t, t.name + "_world"))
    tech.map(t => printResults(grid.country("Spain"), t, t.name + "_Spain"))
    tech.map(t => printResults(grid.eu28, t, t.name + "_eu28"))
*/
    /*    RooftopPVPotential.printCF_Potential(Some(List("Spain")), List(PVMono,PVPoly), "Spain")
    RooftopPVPotential.printCF_Potential(Some(Grid().eu28countries), List(PVMono,PVPoly), "EU28")
    RooftopPVPotential.printCF_Potential(None, List(PVMono,PVPoly), "World")
*/
  }

  def printResults(grid: List[Cell], tech: RenewableTechnology, output_file: String = "") {
    val out_stream = new java.io.PrintStream(new java.io.FileOutputStream(if (output_file.isEmpty()) tech.name else output_file))
    out_stream.print("Country" + "\t" + "CF" + "\t" + "TWe" + "\t" + "TotalArea[km^2]" + "\t" + "SuitableArea[km^2]" + "\t" + "InstalledCapacity[MWi]" + "\n")

    val sorted = grid.filter(c => tech.suitabilityFactor(c) > 0).filter(c => tech.potential(c, 1).value > 0).sortBy(c => tech.capacityFactor(c, 1)).reverse
    sorted.map(c =>
      out_stream.print(c.country.replaceAll("\\s", "") + "\t" + tech.capacityFactor(c, 1) + "\t" + tech.potential(c, 1).to(Terawatts) + "\t" + c.area.toSquareKilometers
        + "\t" + c.area.toSquareKilometers * tech.suitabilityFactor(c) + "\t" + tech.ratedPower(c, 1).to(Megawatts) + "\n"))
  }

}