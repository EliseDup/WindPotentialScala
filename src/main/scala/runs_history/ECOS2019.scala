package runs_history

import squants.energy._
import squants.time.Hours
import squants.space._
import squants.radio._
import utils._
import grid._
import scala.io.Source
import java.io.PrintStream
import wind_solar._
import wind_energy._
import solar_energy._

object ECOS2019 {
  import PlotHelper._

  def main(args: Array[String]): Unit = {
    // ECOSPaperResults
    val grid = Grid.eu()
    val cells = grid.cells
    plotResults(grid)
    printPotentialTable(grid, List(2, 4, 6, 8, 10, 12).map(_.toDouble))
  }

  // Results for the paper of ECOS 2019 conference - 15/02/19
  def ECOSPaperResults {
    var t = System.currentTimeMillis()
    val grid = Grid.eu()
    val cells = Grid.eu().eu28
    println("Grid loaded in " + (System.currentTimeMillis() - t) / 1000.0 + " seconds")

    plotResults(grid)
    printResultsPerCountry(grid, 1)
    printPotentialTable(grid, List(2, 4, 6, 8, 10, 12).map(_.toDouble))
    List(4, 6, 9, 12).map(e => grid.writeWindPotential(e, grid.eu28))
    val techs = List(OnshoreWindTechnology, OffshoreWindTechnology, PVMono, PVPoly, CSPParabolic, CSPParabolicStorage12h, CSPTowerStorage12h)
    techs.map(t => grid.writePotential(t, 1, grid.eu28))
    techs.map(t => grid.writePotential(t, List(4, 6, 9, 12).map(_.toDouble), grid.eu28))

    grid.writeTechnology(List(4, 6, 9, 12).map(_.toDouble), grid.eu28)

    println("-- End :) --")
  }

  def solarPotential(techs: List[SolarTechnology], grid: Grid, name: String) = {
    val res = grid.eroi_netpotential(grid.eu28, techs, 1)
    (res._1, res._2, name)
  }

  def printPotentialTable(grid: Grid, eroi: List[Double]) {
    val techs = List(OnshoreWindTechnology, OffshoreWindTechnology, PVMono, PVPoly, CSPParabolic, CSPParabolicStorage12h, CSPTowerStorage12h)
    val solartechs = List(PVMono, PVPoly, CSPParabolic, CSPParabolicStorage12h, CSPTowerStorage12h)
    val techs_sum = List(List(OnshoreWindTechnology), List(OffshoreWindTechnology), List(PVMono, PVPoly, CSPParabolic, CSPParabolicStorage12h, CSPTowerStorage12h))
    techs.map(t => print("\t" + t.name))
    print("\t" + "Solar" + "\t" + "Total")
    print("\n")
    eroi.map(e => {
      print(e)
      techs.map(t => print("\t" + grid.netpotential(grid.eu28, t, e)))
      print("\t" + grid.netpotential(grid.eu28, solartechs, e) + "\t" + grid.sum_netpotential(grid.eu28, techs_sum, e) + "\n")
    })
  }

  def printResultsPerCountry(grid: Grid, eroi: Double) {
    val techs = List(OnshoreWindTechnology, OffshoreWindTechnology, PVMono, PVPoly, CSPParabolic, CSPParabolicStorage12h, CSPTowerStorage12h)
    print("Country " + "\t")
    techs.map(t => print(t.name + "\t" + "" + "\t"))
    println()
    for (c <- grid.eu28countries) {
      val cells = grid.country(c)
      print(c + "\t" + cells.filter(_.onshore).map(_.area.toSquareKilometers).sum / 1000 + "\t" +
        cells.filter(_.offshore).map(_.area.toSquareKilometers).sum / 1000 + "\t")
      techs.map(tech => print(cells.map(c => c.area.toSquareKilometers * tech.suitabilityFactor(c)).sum / 1000 + "\t" +
        grid.netpotential(cells, tech, eroi) + "\t"))
      print(grid.netpotential(cells, List(PVMono, CSPTowerStorage12h), 1))
      println()
    }
  }
  def plotResults(grid: Grid) {

    val techs = List(OnshoreWindTechnology, OffshoreWindTechnology, CSPParabolic, CSPParabolicStorage12h, CSPTowerStorage12h)
    val eroi_min = (2 until 40).map(_ * 0.5).toList
    val onshorepotential = (eroi_min.map(e => grid.netpotential(grid.eu28, OnshoreWindTechnology, e)), eroi_min, OnshoreWindTechnology.name)
    val offshorepotential = (eroi_min.map(e => grid.netpotential(grid.eu28, OffshoreWindTechnology, e)), eroi_min, OffshoreWindTechnology.name)
    val windPotential = (eroi_min.map(e => grid.sum_netpotential(grid.eu28, List(List(OnshoreWindTechnology), List(OffshoreWindTechnology)), e)), eroi_min, "Wind")

    val solarPot = {
      val res = grid.eroi_netpotential(grid.eu28, List(PVMono, PVPoly, CSPParabolic, CSPParabolicStorage12h, CSPTowerStorage12h), 1)
      (res._1, res._2, "Solar")
    }
    val listSolar = List(PVMono, PVPoly, CSPParabolic, CSPParabolicStorage12h, CSPTowerStorage12h).map(t => solarPotential(List(t), grid, t.name))

    plotXY(List(windPotential, onshorepotential, offshorepotential), legend = true, yLabel = "EROI", xLabel = "Wind Net Potential [EJ/year]", title = "windPotential")
    plotXY(listSolar, legend = true, yLabel = "EROI", xLabel = "Solar Net Potential [EJ/year]", title = "solarPotential")
    plotXY(List(windPotential, solarPot), legend = true, yLabel = "EROI", xLabel = "Net Potential [EJ/year]", title = "wind_solarPotential")
    plotXY(List((eroi_min.map(e => grid.sum_netpotential(grid.eu28, List(List(OnshoreWindTechnology), List(OffshoreWindTechnology), List(PVMono, PVPoly, CSPParabolic, CSPParabolicStorage12h, CSPTowerStorage12h)), e)), eroi_min, "")), yLabel = "EROI", xLabel = "Net Potential [EJ/year]", title = "totalPotential")

  }

}