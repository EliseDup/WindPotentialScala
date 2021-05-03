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

object Thesis_Results {
  import PlotHelper._
  import Helper._
  // Plot:
  // EROIstd vs Electricity Produced
  // EROIpou vs Electricity Delivered
  // EROI pou, external vs Electricity Delivered
  // + Impact of technological progress 
  val distr_losses = 9.5 / 100 / (1 + 9.5 / 100)
  val solar_tech = List(PVMono)
  val wind_tech = List(OnshoreWindTechnology, OffshoreWindTechnology)
  val all_tech = List(OnshoreWindTechnology, OffshoreWindTechnology, PVMono)

  def main(args: Array[String]): Unit = {
    
    val eu = Grid.eu()
    val grid = Grid()
    val cells_red = grid.cells.filter(c => (OnshoreWindTechnology.suitabilityFactor(c) + OffshoreWindTechnology.suitabilityFactor(c) + PVMono.suitabilityFactor(c)) > 0)
    val eu_red = eu.eu28.filter(c => (OnshoreWindTechnology.suitabilityFactor(c) + OffshoreWindTechnology.suitabilityFactor(c) + PVMono.suitabilityFactor(c)) > 0)
    
    List(OnshoreWindTechnology, OffshoreWindTechnology, PVMono,PVPoly).map(t => {
      println("EROI std "+ "\t" +t.name + "\t" + eroi_std_e_produced(List(t), eu, eu_red,"")._2.max)
      println("EROI pou "+ "\t"+ t.name + "\t" + eroi_pou_e_delivered(List(t), eu, eu_red,distr_losses,"")._2.max)
      
    })
    
    
  /*  eroiStdCurves(grid, cells_red)
    eroiPouCurves(grid, cells_red)
    
    eroiStdCurvesDynamic(grid, cells_red, List(1.0, 0.8, 0.5))
    eroiPouCurvesDynamic(grid, cells_red, List(1.0, 0.8, 0.5))
*/
    /* printPotentialTable(grid, eu_red, List(1, 2, 4, 6, 8, 10, 12).map(_.toDouble))
    printPotentialPouTable(grid, eu_red, List(1, 2, 4, 6, 8, 10, 12).map(_.toDouble))

    RooftopPVPotential.printEProduced(RooftopPVPotential.eu28)
    RooftopPVPotential.printEDelivered(RooftopPVPotential.eu28, distr_losses)
*/
    val eroi = List(4, 6, 8, 10).map(_.toDouble)
    /*      grid.writeTechnologyPVWind(eroi)
      grid.writePotential(OnshoreWindTechnology, List(1.0))
      grid.writePotential(OffshoreWindTechnology, List(1.0))
      grid.writePotential(PVMono, List(1.0))
      grid.writePotential(PVPoly, List(1.0))
      
  //    val eu= Grid.eu()
      eu.writeTechnologyPVWind(eroi,eu.eu28,"_eu")
      
      eu.writePotential(OnshoreWindTechnology, List(1.0),eu.eu28,"_eu")
      eu.writePotential(OffshoreWindTechnology, List(1.0),eu.eu28,"_eu")
      eu.writePotential(PVMono, List(1.0),eu.eu28,"_eu")
      eu.writePotential(PVPoly, List(1.0),eu.eu28,"_eu")
  */

    //eu.writeWindPotential(1.0, eu.eu28,"_eu")
    //    eu.writePotential(PVMono, 1.0,eu.eu28,"_eu")
    // eroiCurve
  }

  def eroiCurves {

    var t = System.currentTimeMillis()
    val grid = Grid()
    val cells = grid.cells.filter(c => (OnshoreWindTechnology.suitabilityFactor(c) + OffshoreWindTechnology.suitabilityFactor(c) + PVMono.suitabilityFactor(c)) > 0)
    println(grid.cells.size + "\t" + cells.size)
    println("Grid loaded in " + (System.currentTimeMillis() - t) / 1000.0 + " seconds")

    //    grid.writeTechnology(List(4, 6, 9, 12).map(_.toDouble), cells)
    //printPotentialTable(grid,cells)
    eroiCurves(grid, cells)
    // eroiStdCurves(grid, cells)
    // eroiPouCurves(grid, cells)
    // eroiPouExtCurves(grid, cells)

    val grid_eu = Grid.eu()
    val cells_eu = grid_eu.eu28.filter(c => (OnshoreWindTechnology.suitabilityFactor(c) + OffshoreWindTechnology.suitabilityFactor(c) + PVMono.suitabilityFactor(c)) > 0)
    println(grid_eu.cells.size + "\t" + cells_eu.size)
    println("Grid loaded in " + (System.currentTimeMillis() - t) / 1000.0 + " seconds")

    //    grid.writeTechnology(List(4, 6, 9, 12).map(_.toDouble), cells)
    //printPotentialTable(grid,cells)
    eroiCurves(grid_eu, cells_eu, "_eu")
    //  eroiStdCurves(grid_eu, cells_eu, "_eu")
    //  eroiPouCurves(grid_eu, cells_eu, "_eu")
    //  eroiPouExtCurves(grid_eu, cells_eu, "_eu")

  }
  def printPotentialTable(grid: Grid, cells: List[Cell], eroi: List[Double] = List(2, 4, 6, 8, 10, 12)) {
    val techs = List(OnshoreWindTechnology, OffshoreWindTechnology, PVMono, PVPoly) //, CSPParabolic, CSPParabolicStorage12h, CSPTowerStorage12h)
    val solartechs = List(PVMono, PVPoly) //, CSPParabolic, CSPParabolicStorage12h, CSPTowerStorage12h)
    val techs_sum = List(List(OnshoreWindTechnology), List(OffshoreWindTechnology), List(PVMono))
    techs.map(t => print("\t" + t.name))
    print("\t" + "Solar" + "\t" + "Total, produced" + "\n")
    print("\n")
    eroi.map(e => {
      print(e)
      techs.map(t => print("\t" + round(grid.gross_potential(cells, t, e), 1) + "\t"))
      print("\t" + round(grid.gross_potential(cells, solartechs, e), 1) + "\t" + round(grid.sum_gross_potential(cells, techs_sum, e), 1) + "\n")
    })
  }
  def printPotentialPouTable(grid: Grid, cells: List[Cell], eroi: List[Double] = List(2, 4, 6, 8, 10, 12)) {
    val techs = List(OnshoreWindTechnology, OffshoreWindTechnology, PVMono, PVPoly) //, CSPParabolic, CSPParabolicStorage12h, CSPTowerStorage12h)
    val solartechs = List(PVMono, PVPoly) //, CSPParabolic, CSPParabolicStorage12h, CSPTowerStorage12h)
    val techs_sum = List(List(OnshoreWindTechnology), List(OffshoreWindTechnology), List(PVMono))
    techs.map(t => print("\t" + t.name))
    print("\t" + "Solar" + "\t" + "Total, delivered" + "\n")
    print("\n")
    eroi.map(e => {
      print(e)
      techs.map(t => print("\t" + round(grid.potential_pou(cells, t, e, distr_losses), 1) + "\t"))
      print("\t" + round(grid.potential_pou(cells, solartechs, e, distr_losses), 1) + "\t" + round(grid.sum_potential_pou(cells, techs_sum, e, distr_losses), 1) + "\n")
    })
  }
  def eroiStdCurvesDynamic(grid: Grid, cells: List[Cell], improvment: List[Double], region_name: String = "") {
    val eroi_improvment = improvment.map(i => eroi_std_e_produced_dynamic(all_tech, grid, cells, i, (100 - 100 * i).toInt + "%"))
    plotXY(eroi_improvment, legend = true, xLabel = "Electricity Produced [EJ/y]", yLabel = "EROI std", title = "eroi_std_total_dyn" + region_name)
  }

  def eroiStdCurves(grid: Grid, cells: List[Cell], region_name: String = "") {
    val eroi_std_wind = eroi_std_e_produced(wind_tech, grid, cells, "Wind")
    val eroi_std_solar = eroi_std_e_produced(solar_tech, grid, cells, "Solar")
    val eroi_std_total = eroi_std_e_produced(all_tech, grid, cells, "Total")
    println("Max EROI STD wind : " + "\t" + eroi_std_wind._2.max +"\t" + ", solar : " + eroi_std_solar._2.max)
    
    plotXY(List(eroi_std_total, eroi_std_wind, eroi_std_solar), legend = true, xLabel = "Electricity Produced [EJ/y]", yLabel = "EROI std", title = "eroi_std_total" + region_name)
  }
  def eroiPouCurves(grid: Grid, cells: List[Cell], region_name: String = "") {
    val eroi_pou_wind = eroi_pou_e_delivered(wind_tech, grid, cells, distr_losses, "Wind")
    val eroi_pou_solar = eroi_pou_e_delivered(solar_tech, grid, cells, distr_losses, "Solar")
    val eroi_pou_total = eroi_pou_e_delivered(all_tech, grid, cells, distr_losses, "Total")
    println("Max EROI POU wind : " + "\t" + eroi_pou_wind._2.max +"\t" + ", solar : " + eroi_pou_solar._2.max)
    plotXY(List(eroi_pou_total, eroi_pou_wind, eroi_pou_solar), legend = true, xLabel = "Electricity Delivered [EJ/y]", yLabel = "EROI pou", title = "eroi_pou_total" + region_name)
  }
  def eroiPouCurvesDynamic(grid: Grid, cells: List[Cell], improvment: List[Double], region_name: String = "") {
    val eroi_improvment = improvment.map(i => eroi_pou_e_delivered_dynamic(all_tech, grid, cells, i, (100 - 100 * i).toInt + "%"))
    plotXY(eroi_improvment, legend = true, xLabel = "Electricity Delivered [EJ/y]", yLabel = "EROI pou", title = "eroi_pou_total_dyn" + region_name)
  }
  def eroiPouExtCurves(grid: Grid, cells: List[Cell], region_name: String = "") {
    val eroi_pou_ext_wind = eroi_pou_ext_e_delivered(wind_tech, grid, cells, distr_losses, "Wind")
    val eroi_pou_ext_solar = eroi_pou_ext_e_delivered(solar_tech, grid, cells, distr_losses, "Solar")
    val eroi_pou_ext_total = eroi_pou_ext_e_delivered(all_tech, grid, cells, distr_losses, "Total")
    plotXY(List(eroi_pou_ext_total, eroi_pou_ext_wind, eroi_pou_ext_solar), legend = true, xLabel = "Electricity Delivered [EJ/y]", yLabel = "EROI pou, external", title = "eroi_pou_ext_total" + region_name)
  }
  def eroiCurves(grid: Grid, cells: List[Cell], region_name: String = "", detailed: Boolean = false) {
    val eroi_std_total_delivered = eroi_std_e_delivered(all_tech, grid, cells, distr_losses, "Total")
    val eroi_pou_total = eroi_pou_e_delivered(all_tech, grid, cells, distr_losses, "Total")
    val eroi_pou_ext_total = eroi_pou_ext_e_delivered(all_tech, grid, cells, distr_losses, "Total")

    plotXY(List((eroi_std_total_delivered._1, eroi_std_total_delivered._2, "EROI std"), (eroi_pou_ext_total._1, eroi_pou_ext_total._2, "EROI pou, external"), (eroi_pou_total._1, eroi_pou_total._2, "EROI pou")), legend = true, xLabel = "Electricity Delivered [EJ/y]", yLabel = "EROI", title = "erois" + region_name)

    if (detailed) {
      val eroi_std_wind = eroi_std_e_produced(wind_tech, grid, cells, "Wind")
      val eroi_std_solar = solar_eroi_std_eproduced(solar_tech, grid, cells, "Solar")

      val eroi_std_wind_onshore = eroi_std_e_produced(List(OnshoreWindTechnology), grid, cells, "Onshore")
      val eroi_std_wind_offshore = eroi_std_e_produced(List(OffshoreWindTechnology), grid, cells, "Offshore")
      val eroi_std_pv = eroi_std_e_produced(List(PVMono), grid, cells, "PV")
      val eroi_std_csp = eroi_std_e_produced(List(CSPParabolic), grid, cells, "CSP")
      val eroi_pou_wind = eroi_pou_e_delivered(wind_tech, grid, cells, distr_losses, "Wind")
      val eroi_pou_solar = solar_eroi_pou_edelivered(solar_tech, grid, cells, distr_losses, "Solar")
      val eroi_pou_wind_onshore = eroi_pou_e_delivered(List(OnshoreWindTechnology), grid, cells, distr_losses, "Onshore")
      val eroi_pou_wind_offshore = eroi_pou_e_delivered(List(OffshoreWindTechnology), grid, cells, distr_losses, "Offshore")
      val eroi_pou_pv = eroi_pou_e_delivered(List(PVMono), grid, cells, distr_losses, "PV")
      val eroi_pou_csp = eroi_pou_e_delivered(List(CSPParabolic), grid, cells, distr_losses, "CSP")
      val eroi_pou_ext_wind = eroi_pou_ext_e_delivered(wind_tech, grid, cells, distr_losses, "Wind")
      val eroi_pou_ext_solar = solar_eroi_pou_ext_edelivered(solar_tech, grid, cells, distr_losses, "Solar")
      val eroi_pou_ext_wind_onshore = eroi_pou_ext_e_delivered(List(OnshoreWindTechnology), grid, cells, distr_losses, "Onshore")
      val eroi_pou_ext_wind_offshore = eroi_pou_ext_e_delivered(List(OffshoreWindTechnology), grid, cells, distr_losses, "Offshore")
      val eroi_pou_ext_pv = eroi_pou_ext_e_delivered(List(PVMono), grid, cells, distr_losses, "PV")
      val eroi_pou_ext_csp = eroi_pou_ext_e_delivered(List(CSPParabolic), grid, cells, distr_losses, "CSP")

      plotXY(List(eroi_std_wind, eroi_std_wind_onshore, eroi_std_wind_offshore), legend = true, xLabel = "Electricity Produced [EJ/y]", yLabel = "EROI std", title = "eroi_std_wind" + region_name)
      plotXY(List(eroi_pou_wind, eroi_pou_wind_onshore, eroi_pou_wind_offshore), legend = true, xLabel = "Electricity Delivered [EJ/y]", yLabel = "EROI pou", title = "eroi_pou_wind" + region_name)
      plotXY(List(eroi_pou_ext_wind, eroi_pou_ext_wind_onshore, eroi_pou_ext_wind_offshore), legend = true, xLabel = "Electricity Delivered [EJ/y]", yLabel = "EROI pou ext", title = "eroi_pou_ext_wind" + region_name)

      plotXY(List(eroi_std_solar, eroi_std_pv, eroi_std_csp), legend = true, xLabel = "Electricity Produced [EJ/y]", yLabel = "EROI std", title = "eroi_std_solar" + region_name)
      plotXY(List(eroi_pou_solar, eroi_pou_pv, eroi_pou_csp), legend = true, xLabel = "Electricity Delivered [EJ/y]", yLabel = "EROI pou", title = "eroi_pou_solar" + region_name)
      plotXY(List(eroi_pou_ext_solar, eroi_pou_ext_pv, eroi_pou_ext_csp), legend = true, xLabel = "Electricity Delivered [EJ/y]", yLabel = "EROI pou ext", title = "eroi_pou_ext_solar" + region_name)

    }
  }
  def tpes_gdp {
    val data = getLines("../model_data/TPES_GDP")
    val y = data.map(_(0).toDouble)
    val tpes = data.map(_(1).toDouble)
    val gdp = data.map(_(2).toDouble / 1000)
    val intensity = (0 until y.size).toList.map(i => tpes(i) / gdp(i))
    plotXY(List((tpes, gdp, "")), xLabel = "Primary Energy Consumption [EJ]", yLabel = "GDP [TUS$2010]", title = "tpc_gdp")
    plotXY(List((y, intensity, "")), yLabel = "Energy Intensity of GDP [MJ/US$2010]", title = "energy_intensity", int_x_axis = true)
  }

  def plotDistLosses {
    val lines = getLines("../model_data/data_distribution_losses", "\t").map(i => (i(0).toInt, MegaTonOilEquivalent(i(1).toDouble), MegaTonOilEquivalent(i(2).toDouble)))
    val year = lines.map(_._1)
    val elec_cons = lines.map(_._2)
    val elec_losses = lines.map(_._3)
    val ratio = (0 until elec_cons.size).toList.map(i => (elec_losses(i) / elec_cons(i)) * 100)
    plotXY(List((year.map(_.toDouble), ratio, ""), (year.map(_.toDouble), year.map(i => ratio.sum / ratio.size), "a")), yLabel = "Losses / Electricity Consumption [%]", int_x_axis = true)
    val sub_list = (0 until 10).map(i => year.size - 10 + i).toList
    val sub_y = sub_list.map(i => year(i).toDouble)
    val sub_ratio = sub_list.map(i => ratio(i).toDouble)
    plotXY(List((sub_y, sub_ratio, ""), (sub_y, sub_list.map(i => sub_ratio.sum / sub_ratio.size), "a")), yLabel = "Losses / Electricity Consumption [%]", int_x_axis = true)
  }

  def eroi_pou_e_delivered(techs: List[RenewableTechnology], grid: Grid, cells: List[Cell], distr_losses: Double, name: String) = {
    val res = grid.eroi_pou_sum_potential(cells, techs, 1, distr_losses)
    (res._1, res._2, name)
  }
  def eroi_pou_ext_e_delivered(techs: List[RenewableTechnology], grid: Grid, cells: List[Cell], distr_losses: Double, name: String) = {
    val res = grid.eroi_pou_external_sum_potential(cells, techs, 1, distr_losses)
    (res._1, res._2, name)
  }
  def eroi_std_e_produced(techs: List[RenewableTechnology], grid: Grid, cells: List[Cell], name: String) = {
    val res = grid.eroi_std_sum_potential(cells, techs, 1)
    (res._1, res._2, name)
  }
  def eroi_std_e_produced_dynamic(techs: List[RenewableTechnology], grid: Grid, cells: List[Cell], improvment: Double, name: String) = {
    val res = grid.eroi_std_dynamic_sum_potential(cells, techs, 1, improvment)
    (res._1, res._2, name)
  }

  def eroi_std_e_delivered(techs: List[RenewableTechnology], grid: Grid, cells: List[Cell], distr_losses: Double, name: String) = {
    val res = grid.eroi_std_sum_potential_delivered(cells, techs, 1, distr_losses)
    (res._1, res._2, name)
  }
  def solar_eroi_std_eproduced(techs: List[SolarTechnology], grid: Grid, cells: List[Cell], name: String) = {
    val res = grid.eroi_potential(cells, techs, 1)
    (res._1, res._2, name)
  }
  def solar_eroi_pou_edelivered(techs: List[SolarTechnology], grid: Grid, cells: List[Cell], distr_losses: Double, name: String) = {
    val res = grid.eroi_pou_potential(cells, techs, 1, distr_losses)
    (res._1, res._2, name)
  }
  def eroi_pou_e_delivered_dynamic(techs: List[RenewableTechnology], grid: Grid, cells: List[Cell], improvment: Double, name: String) = {
    val res = grid.eroi_pou_dynamic_sum_potential(cells, techs, 1, distr_losses, improvment)
    (res._1, res._2, name)
  }
  def solar_eroi_pou_ext_edelivered(techs: List[SolarTechnology], grid: Grid, cells: List[Cell], distr_losses: Double, name: String) = {
    val res = grid.eroi_pou_ext_potential(cells, techs, 1, distr_losses)
    (res._1, res._2, name)
  }

  def recalculateSolarResults {
    /*SolarPotential_03_2019.printResultsPV(PVPoly, Gigawatts(1), WattsPerSquareMeter(207))
    SolarPotential_03_2019.printResultsPV(PVMono, Gigawatts(1), WattsPerSquareMeter(207))
    SolarPotential_03_2019.printResultsCSP(CSPParabolic, 1.3, Gigawatts(1), WattsPerSquareMeter(237))
    SolarPotential_03_2019.printResultsCSP(CSPParabolic, 1.9, Gigawatts(1), WattsPerSquareMeter(237))
    SolarPotential_03_2019.printResultsCSP(CSPParabolicStorage12h, 2.7, Gigawatts(1), WattsPerSquareMeter(237))
    SolarPotential_03_2019.printResultsCSP(CSPParabolicStorage12h, 4, Gigawatts(1), WattsPerSquareMeter(237))
    SolarPotential_03_2019.printResultsCSP(CSPTowerStorage12h,  2.7, Gigawatts(1), WattsPerSquareMeter(237))
    SolarPotential_03_2019.printResultsCSP(CSPTowerStorage12h, 4, Gigawatts(1), WattsPerSquareMeter(237))
    */
    // SolarPotential_03_2019.plotGrossResultsForPaper

    SolarPotential_03_2019.printGrossResultsForPaper(1)
    /*val grid = SolarGrid._0_5deg //.cells
  grid.writeTechnology(5,List(),PVMono,CSPTowerStorage12h)
  grid.writeTechnology(7,List(),PVMono,CSPTowerStorage12h)
  grid.writeTechnology(9,List(),PVMono,CSPTowerStorage12h)*/

    cspCorrectedCF(CSPParabolic, 0.6)
    cspCorrectedCF(CSPParabolicStorage12h, 0.6)
    cspCorrectedCF(CSPTowerStorage12h, 0.6)

  }

  def cspCorrectedCF(tech: CSP, factor: Double) {
    val grid = SolarGrid._0_5deg

    val theoreticalCF = SolarPotential_03_2019.listGrossEROI(grid.cells, List(tech), "Theoretical CF")
    val realCF = {
      val res = Helper.listValueVSCumulated(grid.cells.filter(g => g.grossYearlyProduction(List(tech)).value > 0 && tech.corrected_eroi(g.dni, factor) >= 1).map(g => (tech.corrected_eroi(g.dni, factor), (g.grossYearlyProduction(List(tech))).to(Exajoules) * 0.6)))
      (res._1, res._2, "Corrected CF")
    }
    plotXY(List(theoreticalCF, realCF), xLabel = "Electricity Produced [EJ/year]", yLabel = "EROI std", legend = true, title = tech.name + "_cfs")

    List(1, 5, 7.5, 9).map(e => println(e + "\t" + potentialCorrected(grid.cells, tech, 1.0, e) + "\t" + potentialCorrected(grid.cells, tech, factor, e) + "\n"))

  }
  def potentialCorrected(cells: List[SolarCell], tech: CSP, factor: Double, eroi_min: Double) = {
    cells.filter(i => i.grossYearlyProduction(tech).value > 0 && tech.corrected_eroi(i.dni, factor) >= eroi_min).map(_.grossYearlyProduction(tech) * factor).foldLeft(Exajoules(0))(_ + _)
  }

}