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
  import Helper._
  def main(args: Array[String]): Unit = {
    var t = System.currentTimeMillis()
    val grid = Grid.eu()
    val cells = grid.eu28
    println("Grid loaded in " + (System.currentTimeMillis() - t) / 1000.0 + " seconds")
    //plotGrossResults(grid,cells)

    ECOSPaperResults
    //val eroi = List(7.5,8.5,9.5).map(_.toDouble)
    //grid.writeTechnology(eroi.map(_.toDouble), cells)
  }

  // Results for the paper of ECOS 2019 conference - 15/02/19
  def ECOSPaperResults {
    var t = System.currentTimeMillis()
    val grid = Grid.eu()
    val cells = grid.eu28
    println("Grid loaded in " + (System.currentTimeMillis() - t) / 1000.0 + " seconds")

    //  plotResults(grid, cells)
    //  printResultsPerCountry(grid, 1)
    printPotentialTable(grid, cells, List(5, 12).map(_.toDouble))
    printPotentialTable(grid, cells, List(2, 4, 6, 8, 10, 12).map(_.toDouble))
    plotGrossResults(grid, cells)

    List(4, 6, 9, 12).map(e => grid.writeWindPotential(e, cells))
    val techs = List(OnshoreWindTechnology, OffshoreWindTechnology, PVMono, PVPoly, CSPParabolic, CSPParabolicStorage12h, CSPTowerStorage12h)
    techs.map(t => grid.writePotential(t, List(1.0), cells))
    techs.map(t => grid.writePotential(t, List(4, 6, 9, 12).map(_.toDouble), cells))

    grid.writeTechnology(List(4, 6, 9, 12).map(_.toDouble), cells)

    println("-- End :) --")
  }

  def solarPotential(techs: List[SolarTechnology], grid: Grid, cells: List[Cell], name: String) = {
    val res = grid.eroi_netpotential(cells, techs, 1)
    (res._1, res._2, name)
  }
  def solarGrossPotential(techs: List[SolarTechnology], grid: Grid, cells: List[Cell], name: String) = {
    val res = grid.eroi_potential(cells, techs, 1)
    (res._1, res._2, name)
  }
  def solarGrossPotential_pou(techs: List[SolarTechnology], grid: Grid, cells: List[Cell], name: String, distr_losses: Double) = {
    val res = grid.eroi_pou_potential(cells, techs, 1, distr_losses)
    (res._1, res._2, name)
  }
  def printPotentialTable(grid: Grid, cells: List[Cell], eroi: List[Double]) {
    val techs = List(OnshoreWindTechnology, OffshoreWindTechnology, PVMono, PVPoly, CSPParabolic, CSPParabolicStorage12h, CSPTowerStorage12h)
    val solartechs = List(PVMono, PVPoly, CSPParabolic, CSPParabolicStorage12h, CSPTowerStorage12h)
    val techs_sum = List(List(OnshoreWindTechnology), List(OffshoreWindTechnology), List(PVMono, PVPoly, CSPParabolic, CSPParabolicStorage12h, CSPTowerStorage12h))
    techs.map(t => print("\t" + t.name))
    print("\t" + "Solar" + "\t" + "Total")
    print("\n")
    eroi.map(e => {
      print(e)
      techs.map(t => print("\t" + grid.potential(cells, t, e)))
      print("\t" + grid.potential(cells, solartechs, e) + "\t" + grid.sum_potential(cells, techs_sum, e) + "\n")
    })
  }

  def printResultsPerCountry(grid: Grid, eroi: Double) {
    val techs = List(OnshoreWindTechnology, OffshoreWindTechnology, PVMono, PVPoly, CSPParabolic, CSPParabolicStorage12h, CSPTowerStorage12h)
    print(" " + "\t" + " " + "\t" + " " + "\t" + " " + "\t")
    techs.map(t => print(t.name + "\t" + " " + "\t" + " " + "\t" + " " + "\t" + " " + "\t"))
    println()
    print("Country" + "\t" + "Onshore_area" + "\t" + "Offshore_area" + "\t")
    techs.map(t => print("Suitable_area" + "\t" + "Potential[EJ/year]" + "\t" + "Installed_Capacity[GW]" + "\t"))
    println()
    for (c <- grid.eu28countries) {
      val cells = grid.country(c)
      print(c + "\t" + cells.filter(_.onshore).map(_.area.toSquareKilometers).sum / 1000 + "\t" +
        cells.filter(_.offshore).map(_.area.toSquareKilometers).sum / 1000 + "\t")
      techs.map(tech => print(cells.map(c => c.area.toSquareKilometers * tech.suitabilityFactor(c)).sum / 1000 + "\t" +
        grid.potential(cells, tech, eroi) + "\t" + grid.installedCapacity(cells, tech, eroi) + "\t"))
      print(grid.potential(cells, List(PVMono, CSPTowerStorage12h), 1))
      println()
    }
  }
  def plotResults(grid: Grid, cells: List[Cell]) {

    val techs = List(OnshoreWindTechnology, OffshoreWindTechnology, CSPParabolic, CSPParabolicStorage12h, CSPTowerStorage12h)
    val eroi_min = (2 until 40).map(_ * 0.5).toList
    val onshorepotential = (eroi_min.map(e => grid.netpotential(cells, OnshoreWindTechnology, e)), eroi_min, OnshoreWindTechnology.name)
    val offshorepotential = (eroi_min.map(e => grid.netpotential(cells, OffshoreWindTechnology, e)), eroi_min, OffshoreWindTechnology.name)
    val windPotential = (eroi_min.map(e => grid.sum_netpotential(cells, List(List(OnshoreWindTechnology), List(OffshoreWindTechnology)), e)), eroi_min, "Wind")

    val solarPot = {
      val res = grid.eroi_netpotential(cells, List(PVMono, PVPoly, CSPParabolic, CSPParabolicStorage12h, CSPTowerStorage12h), 1)
      (res._1, res._2, "Solar")
    }
    val listSolar = List(PVMono, PVPoly, CSPParabolic, CSPParabolicStorage12h, CSPTowerStorage12h).map(t => solarPotential(List(t), grid, cells, t.name))

    plotXY(List(windPotential, onshorepotential, offshorepotential), legend = true, yLabel = "EROI", xLabel = "Wind Net Potential [EJ/year]", title = "windPotential")
    plotXY(listSolar, legend = true, yLabel = "EROI", xLabel = "Solar Net Potential [EJ/year]", title = "solarPotential")
    plotXY(List(windPotential, solarPot), legend = true, yLabel = "EROI", xLabel = "Net Potential [EJ/year]", title = "wind_solarPotential")
    plotXY(List((eroi_min.map(e => grid.sum_netpotential(cells, List(List(OnshoreWindTechnology), List(OffshoreWindTechnology), List(PVMono, PVPoly, CSPParabolic, CSPParabolicStorage12h, CSPTowerStorage12h)), e)), eroi_min, "")), yLabel = "EROI", xLabel = "Net Potential [EJ/year]", title = "totalPotential")

  }
  def plotGrossResults(grid: Grid, cells: List[Cell]) {
    val eroi_min = (2 until 40).map(_ * 0.5).toList
    val windPotential = (eroi_min.map(e => grid.sum_potential(cells, List(List(OnshoreWindTechnology), List(OffshoreWindTechnology)), e)), eroi_min, "Wind")
    val solarPot = {
      val res = grid.eroi_potential(cells, List(PVMono, PVPoly, CSPParabolic, CSPParabolicStorage12h, CSPTowerStorage12h), 1)
      (res._1, res._2, "Solar")
    }
    val total = (eroi_min.map(e => grid.sum_potential(cells, List(List(OnshoreWindTechnology), List(OffshoreWindTechnology), List(PVMono, PVPoly, CSPParabolic, CSPParabolicStorage12h, CSPTowerStorage12h)), e)), eroi_min, "Total")
    plotXY(List(windPotential, solarPot, total), legend = true, yLabel = "EROI", xLabel = "Potential [EJ/year]", title = "wind_solar_tot_GrossPotential")

    val techs = List(OnshoreWindTechnology, OffshoreWindTechnology, CSPParabolic, CSPParabolicStorage12h, CSPTowerStorage12h)
    val onshorepotential = (eroi_min.map(e => grid.potential(cells, OnshoreWindTechnology, e)), eroi_min, OnshoreWindTechnology.name)
    val offshorepotential = (eroi_min.map(e => grid.potential(cells, OffshoreWindTechnology, e)), eroi_min, OffshoreWindTechnology.name)
    val listSolar = List(PVMono, PVPoly, CSPParabolic, CSPParabolicStorage12h, CSPTowerStorage12h).map(t => solarGrossPotential(List(t), grid, cells, t.name))

    plotXY(List(windPotential, onshorepotential, offshorepotential), legend = true, yLabel = "EROI", xLabel = "Potential [EJ/year]", title = "windGrossPotential")
    plotXY(listSolar, legend = true, yLabel = "EROI", xLabel = "Potential [EJ/year]", title = "solarGrossPotential")
    plotXY(List(windPotential, solarPot), legend = true, yLabel = "EROI", xLabel = "Potential [EJ/year]", title = "wind_solarGrossPotential")
    plotXY(List(total), yLabel = "EROI", xLabel = "Potential [EJ/year]", title = "totalGrossPotential")

  }

  def plotGrossResults_pou(grid: Grid, cells: List[Cell], distr_losses: Double) {
    // val eroi_min = (2 until 40).map(_ * 0.5).toList
    val windPotential = {
      val res = grid.eroi_pou_potential(cells, List(OnshoreWindTechnology, OffshoreWindTechnology), 1, distr_losses)
      (res._1, res._2, "Wind")
    }
    val solarPot = {
      val res = grid.eroi_pou_potential(cells, List(PVMono, PVPoly), 1, distr_losses)
      (res._1, res._2, "Solar")
    }
    // val total = grid.sum_potential_pou(cells, List(List(OnshoreWindTechnology), List(OffshoreWindTechnology), List(PVMono, PVPoly, CSPParabolic, CSPParabolicStorage12h, CSPTowerStorage12h)), 1, distr_losses)
    //plotXY(List(windPotential, solarPot, total), legend = true, yLabel = "EROIpou", xLabel = "Potential [EJ/year]", title = "wind_solar_tot_GrossPotentialpou")

    val techs = List(OnshoreWindTechnology, OffshoreWindTechnology, CSPParabolic, CSPParabolicStorage12h, CSPTowerStorage12h)
    //   val onshorepotential = (eroi_min.map(e => grid.potential_pou(cells, OnshoreWindTechnology, e, distr_losses)), eroi_min, OnshoreWindTechnology.name)
    //   val offshorepotential = (eroi_min.map(e => grid.potential_pou(cells, OffshoreWindTechnology, e, distr_losses)), eroi_min, OffshoreWindTechnology.name)
    val listSolar = List(PVMono, PVPoly, CSPParabolic, CSPParabolicStorage12h, CSPTowerStorage12h).map(t => solarGrossPotential_pou(List(t), grid, cells, t.name, distr_losses))

    // plotXY(List(windPotential, onshorepotential, offshorepotential), legend = true, yLabel = "EROIpou", xLabel = "Potential [EJ/year]", title = "windGrossPotentialpou")
    plotXY(listSolar, legend = true, yLabel = "EROIpou", xLabel = "Potential [EJ/year]", title = "solarGrossPotentialpou")
    plotXY(List(windPotential, solarPot), legend = true, yLabel = "EROIpou", xLabel = "Potential [EJ/year]", title = "wind_solarGrossPotentialpou")
    // plotXY(List(total), yLabel = "EROIpou", xLabel = "Potential [EJ/year]", title = "totalGrossPotentialpou")
  }
}