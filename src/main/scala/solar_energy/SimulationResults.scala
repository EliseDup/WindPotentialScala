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
import wind_energy.OnshoreWindTechnology
import wind_energy.OffshoreWindTechnology

object SimuationResults {

  import SolarUtils._
  import DayMonth._
  import PlotHelper._
  import SolarPotential._
  import SolarGrid._
  import Helper._
  import wind_energy.WindFarmEnergyInputs._
  import SolarPower._

  def main(args: Array[String]): Unit = {
    val grid = _0_5deg
    
    //logEROICurves(List(List(PVMono, PVPoly), List(CSPParabolic, CSPParabolicStorage12h, CSPTowerStorage12h)), "pv_csp_eroi")
    printResultsForPaper(5)
    plotResultsForPaper
    //val grid = _0_5deg
    //(1 to 20).map(i => println(i / 2.0 + "\t" + potential(grid.cells, List(PVPoly, CSPParabolic, CSPParabolicStorage12h, CSPTowerStorage12h), i / 2.0).to(Exajoules)))
    print("-End-")

  }

  def logEROICurves(techs: List[List[SolarTechnology]], fileName: String) {
    val grid = _0_5deg.cells.filter(_.potential(techs.flatten).value > 0)
    val log = new java.io.PrintStream(new java.io.FileOutputStream(fileName))

    grid.map(c => {
      techs.map(t => {
        val tech = c.bestTechnology(t)
        val output = c.potential(tech) * Hours(365 * 24)
        val installed = c.installedCapacity(tech)
        val inputs = tech.embodiedEnergy(installed, Joules(0), SquareKilometers(1))
        val oe = tech.ee.O_M_output.toGigajoules * output.to(Gigajoules) * tech.lifeTime
        log.print(c.eroi(tech) + "\t" + output.to(Petajoules) + "\t" + installed.toMegawatts + "\t" + inputs.to(Petajoules) + "\t" + oe / 1E6 + "\t" + c.suitableArea(tech).toSquareKilometers +
          "\t" + !tech.directOnly + "\t")
      })
      // MAX
      val tech = c.bestTechnology(techs.flatten)
      val output = c.potential(tech) * Hours(365 * 24)
      val installed = c.installedCapacity(tech)
      val inputs = tech.embodiedEnergy(installed, Joules(0), SquareKilometers(1))
      val oe = tech.ee.O_M_output.toGigajoules * output.to(Gigajoules) * tech.lifeTime
      log.print(c.eroi(tech) + "\t" + output.to(Petajoules) + "\t" + installed.toMegawatts + "\t" + inputs.to(Petajoules) + "\t" + oe / 1E6 + "\t" + c.suitableArea(tech).toSquareKilometers +
        "\t" + !tech.directOnly + "\t")

      log.print("\n")

    })
  }

  def plotBestSM(tech: CSP, sm: List[Double] = List()) {
    val dni = (2300 to 4500).map(_.toDouble / 10).toList
    // val eroi = sm.map(i => (dni, dni.map(j => tech.eroi(WattsPerSquareMeter(j), i)), i.toString))
    //plotXY(eroi, xLabel = "DNI [W/m2]", yLabel = "EROI", legend = true)
    val eff = sm.map(i => (dni, dni.map(j => tech.efficiency(WattsPerSquareMeter(j), i) * 100), i.toString))
    //  plotXY(eff, xLabel = "DNI [W/m2]", yLabel = "Efficiency [%]", legend = true)
    val sm_eroi = (dni, dni.map(j => tech.max_eroi_sm(WattsPerSquareMeter(j))), "Max EROI")
    val sm_net_e = (dni, dni.map(j => tech.max_net_energy_sm(WattsPerSquareMeter(j), SquareKilometers(100))), "Max Net Energy")
    plotXY(List(sm_eroi, sm_net_e), xLabel = "DNI [W/m2]", yLabel = "Optimal SM " + tech.name, legend = true, title = "sm_" + tech.name)
  }

  def plotResultsForPaper {

    val grid = _0_5deg
    val techs = List(PVMono, PVPoly, CSPTowerStorage12h, CSPParabolicStorage12h, CSPParabolic)

    plotEROI(grid.cells, techs, "potential")
    plotPotentialByTechnology(grid.cells, List(PVMono, PVPoly), "potential_PV")
    plotPotentialByTechnology(grid.cells, List(CSPTowerStorage12h, CSPParabolicStorage12h, CSPParabolic), "potential_CSP")
    plotPotentialByTechnology(grid.cells, techs, "potential_allTech")
    plotPotentialByTechnology(grid.eu28, techs, "potential_eu28")

    plotPotentialByContinents(grid, techs, "potential_by_continent")

    //plotPotentialVSArea(grid.cells, techs, "potential_area")
    //plotPotentialVSAreaByContinents(grid, List(PVMono), "potentialByContinentArea_PV")
    //plotPotentialVSAreaByContinents(grid, List(CSPParabolicStorage12h), "potentialByContinentArea_CSP")

    // Plot potential for the usual / optimal solar multiple
    plotXY(List(listEROI(grid.cells, CSPParabolic, 1.3), listEROI(grid.cells, CSPParabolic)), xLabel = "Net Potential [EJ/year]", yLabel = "EROI", legend = true, title = "ptpp_sm")
    plotXY(List(listEROI(grid.cells, CSPParabolicStorage12h, 2.7), listEROI(grid.cells, CSPParabolicStorage12h)), xLabel = "Net Potential [EJ/year]", yLabel = "EROI", legend = true, title = "ptpp12h_sm")
    plotXY(List(listEROI(grid.cells, CSPTowerStorage12h, 2.7), listEROI(grid.cells, CSPTowerStorage12h)), xLabel = "Net Potential [EJ/year]", yLabel = "EROI", legend = true, title = "stpp12h_sm")
  }

  def printResultsForPaper(eroi_min: Double) {
    val grid = _0_5deg
    val techs = List(PVMono, PVPoly, CSPParabolic, CSPParabolicStorage12h, CSPTowerStorage12h)
    val total = netPotential(grid.cells, techs, eroi_min).to(Exajoules)
    val totalPV = netPotential(grid.cells, List(PVPoly, PVMono), eroi_min).to(Exajoules)
    val totalCSP = netPotential(grid.cells, List(CSPParabolic, CSPParabolicStorage12h, CSPTowerStorage12h), eroi_min).to(Exajoules)

    println("Potential All Tech " + "\t" + math.round(total))
    techs.map(t => println("Net Potential " + t.name + "\t" + math.round(netPotential(grid.cells, List(t), eroi_min).to(Exajoules))))
    techs.map(t => println("Max EROI " + t.name + "\t" + grid.cells.filter(_.potential(t).value > 0).map(i => i.eroi(t)).max))

    // Tables by continent
    val countries = countriesByContinent
    countries.map(c => println(c._1 + " & " +
      math.round(netPotential(grid.countries(c._2), techs, eroi_min).to(Exajoules)) + " & " +
      math.round(netPotential(grid.countries(c._2), techs, eroi_min).to(Exajoules) / total * 100) + " & " +
      math.round(netPotential(grid.countries(c._2), List(PVMono, PVPoly), eroi_min).to(Exajoules)) + " & " +
      math.round(netPotential(grid.countries(c._2), List(PVMono, PVPoly), eroi_min).to(Exajoules) / totalPV * 100) + " & " +
      math.round(netPotential(grid.countries(c._2), List(CSPParabolic, CSPParabolicStorage12h, CSPTowerStorage12h), eroi_min).to(Exajoules)) + " & " +
      math.round(netPotential(grid.countries(c._2), List(CSPParabolic, CSPParabolicStorage12h, CSPTowerStorage12h), eroi_min).to(Exajoules) / totalCSP * 100) +

      " \\" + "\\"))

    println("textbf{Total}" + "&" + math.round(total) + " & " + math.round(totalPV) + "& " + math.round(totalCSP) + " & " + " \\" + "\\")

    // EU28 potential
    val eu = grid.eu28
    techs.map(t => println("Potential EU28 " + t.name + "\t" + math.round(netPotential(eu, List(t), eroi_min).to(Exajoules))))
    techs.map(t => println("Max EROI EU28 " + t.name + "\t" + eu.filter(_.potential(t).value > 0).map(i => i.eroi(t)).max))
    // techs.map(t => println("Max CF " + t.name + " \t" + grid.cells.map(i => t.capacityFactor(if (t.directOnly) i.dni else i.ghi, i.panelArea(t)) * 100).max))

    val eroi = List(1, 5, 7.5, 9)
    techs.map({ tech =>
      print(tech.name + "\t")
      eroi.map(e => print(math.round(netPotential(grid.cells, List(tech), e).to(Exajoules)) + "\t"))
      println()
    })
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

  def plotPotentialByTechnology(grid: List[SolarCell], techs: List[SolarTechnology], title: String) {
    plotXY(techs.map(t => listEROI(grid, t)), xLabel = "Net Potential [EJ/year]", yLabel = "EROI", legend = true, title = title)
  }

  def listEROI(g: List[SolarCell], tech: SolarTechnology) = {
    val res = Helper.listValueVSCumulated(g.filter(g => g.netYearlyProduction(tech).value > 0 && g.eroi(tech) >= 1).map(g => (g.eroi(tech), (g.netYearlyProduction(tech)).to(Exajoules))))
    (res._1, res._2, tech.name)
  }
  def listEROI(g: List[SolarCell], tech: CSP, sm: Double) = {
    val res = Helper.listValueVSCumulated(g.filter(g => g.netYearlyProduction(tech, sm).value > 0 && tech.eroi(g.dni, sm) >= 1).map(g => (tech.eroi(g.dni, sm), (g.netYearlyProduction(tech, sm)).to(Exajoules))))
    (res._1, res._2, tech.name + ", sm = " + sm)
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
  def netPotential(g: List[SolarCell], techs: List[SolarTechnology], eroi_min: Double): Energy = {
    g.filter(i => i.netYearlyProduction(techs).value > 0 && i.eroi(techs) >= eroi_min).map(_.netYearlyProduction(techs)).foldLeft(Exajoules(0))(_ + _)
  }
  def netPotential(g: List[SolarCell], tech: CSP, eroi_min: Double, sm: Double): Energy = {
    g.filter(i => i.netYearlyProduction(tech, sm).value > 0 && i.eroi(tech) >= eroi_min).map(_.netYearlyProduction(tech, sm)).foldLeft(Exajoules(0))(_ + _)
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

  def printResultsCSP(tech: CSP, sm: Double, power: Power, dni: Irradiance) {
    print(tech.name + "\t")
    print(math.round((tech.panelArea(power, sm) * tech.occupationRatio).toSquareKilometers) + "\t")
    print(math.round(tech.lifeTimeEfficiency(dni, sm) * 100 * 100) / 100.0 + "\t")
    print(math.round(tech.potential(dni, tech.panelArea(power, sm), sm) / power * 100 * 100) / 100.0 + "\t")
    print(math.round((tech.potential(dni, tech.panelArea(power, sm), sm) * Hours(365 * 24) * tech.ee.lifeTime).to(Petajoules) * 100) / 100.0 + "\t")
    print(math.round(tech.ee.embodiedEnergyArea(power, tech.potential(dni, tech.panelArea(power, sm), sm) * Hours(365 * 24), tech.panelArea(power, sm)).to(Petajoules) * 100) / 100.0 + "\t")
    print(math.round(tech.ee.embodiedEnergyArea(power, Joules(0), tech.panelArea(power, sm)).to(Petajoules) * 100) / 100.0 + "\t")
    println(math.round(tech.eroi(dni, sm) * 100) / 100.0)
  }

  def printResultsPV(tech: SolarTechnology, power: Power, ghi: Irradiance) {
    print(tech.name + "\t" + tech.max_eroi_sm(ghi) + "\t")
    print(math.round((tech.panelArea(power, ghi) * tech.occupationRatio).toSquareKilometers) + "\t")
    print(math.round(tech.lifeTimeEfficiency(ghi) * 100 * 100) / 100.0 + "\t")
    print(math.round(tech.potential(ghi, tech.panelArea(power, ghi)) / power * 100 * 100) / 100.0 + "\t")
    print(math.round((tech.potential(ghi, tech.panelArea(power, ghi)) * Hours(365 * 24) * tech.ee.lifeTime).to(Petajoules) * 100) / 100.0 + "\t")
    print(math.round(tech.ee.embodiedEnergyArea(power, tech.potential(ghi, tech.panelArea(power, ghi)) * Hours(365 * 24), tech.panelArea(power, ghi)).to(Petajoules) * 100) / 100.0 + "\t")
    print(math.round(tech.ee.embodiedEnergyArea(power, Joules(0), tech.panelArea(power, ghi)).to(Petajoules) * 100) / 100.0 + "\t")
    println(math.round(tech.eroi(ghi) * 100) / 100.0)
  }
}