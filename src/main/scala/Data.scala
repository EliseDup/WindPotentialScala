

import utils.Helper
import java.io.PrintStream
import utils.PlotHelper
import squants.space.Degrees
import grid.WorldGrid
import wind_energy.CapacityFactorCalculation
import wind_energy.WindPotential
import squants.space.SquareKilometers
import squants.radio.WattsPerSquareMeter
import utils.Exajoules
import squants.energy._
import wind_energy.WindPowerTransmission
import grid.GlobCoverClasses
import java.io.FileOutputStream
import squants.radio.Irradiance
import utils._
import utils.TerawattHours
import utils.Terawatts
import squants.time.Hours
import squants.space.Area
import grid.GridCell
import squants.motion.Velocity
import squants.motion.MetersPerSecond
import utils._
import squants.space.Meters
import org.apache.commons.math3.special.Gamma
import wind_energy.GustavsonWakeEffect
import wind_energy.WindProfile
import wind_energy.WindFarmEnergyInputs
import utils.PetawattHours
import co.theasi.plotly._
import utils.PlotlyHelper
import wind_energy.NormalizedPowerCurve
import squants.space.Kilometers

object Data {

  def main(args: Array[String]): Unit = {
    
    println(MegaTonOilEquivalent(9425).to(Exajoules))
    println("Start")

    val eroi = (2 to 40).map(_ * 0.5.toDouble).toList
    val t0 = System.currentTimeMillis
    val world = new WorldGrid("../model_data/23_05_17_dissipation", Degrees(0.75))
    println("Load Grid in " + (System.currentTimeMillis() - t0) / 1000.0 + " seconds")
    val c = world.grids.filter(_.EEZ)
    val total = (eroi.map(e => WindPotential.potential(e, true, c).to(Exajoules)),eroi, "Total")
    val on = (eroi.map(e => WindPotential.potential(e, true, c.filter(_.onshore)).to(Exajoules)),eroi, "Onshore")
    val off = (eroi.map(e => WindPotential.potential(e, true, c.filter(_.offshore)).to(Exajoules)),eroi, "Offshore")
    PlotHelper.plotXY(List(total,on,off), xLabel = "Energie Brute Produite [EJ/an]", yLabel = "TRE", tick = (true, 100, 4), legend = true)
 
  }
  
  def oneCellTest(cf: Double) {
    val area = SquareKilometers(1000)
    val ratedPower = Megawatts(2)
    val d = Meters(80)
    // val cf = 0.25
    println(WindPotential.onshoreEnergyInputs(Megawatts(1), Megawatts(1) * cf * 0.8 * Hours(365 * 24), Kilometers(0)).toGigajoules)

    def spacing(installedCap: Irradiance) = Math.sqrt((ratedPower) / (d * d * installedCap))

    def powerDensity(installedCap: Irradiance) = {
      val n = spacing(installedCap); val nT = (installedCap * area) / ratedPower
      cf * installedCap * GustavsonWakeEffect.arrayEfficiency(nT, Math.PI / (4 * n * n), false)
    }
    def power(installedCap: Irradiance) = {
      val n = spacing(installedCap); val nT = (installedCap * area) / ratedPower
      area * cf * installedCap * GustavsonWakeEffect.arrayEfficiency(nT, Math.PI / (4 * n * n), false)
    }
    def netEnergy(installedCap: Irradiance) = power(installedCap) * Hours(365 * 24) - (WindPotential.onshoreEnergyInputs(installedCap * area, power(installedCap) * Hours(365 * 24), Kilometers(0)) / 25.0)
    def eroiTot(installedCap: Irradiance) = {
      power(installedCap) * Hours(365 * 24) / (WindPotential.onshoreEnergyInputs(installedCap * area, power(installedCap) * Hours(365 * 24), Kilometers(0)) / 25.0)
    }

    val cd = (5 to 150).map(c => WattsPerSquareMeter(c * 0.1)).toList
    PlotHelper.plotXY(List((cd.map(_.toWattsPerSquareMeter), cd.map(powerDensity(_).to(WattsPerSquareMeter)), "")), xLabel = "Installed Capacity Density [Wi/m2]", yLabel = "Net Energy [PJ/year]")
    PlotHelper.dualAxisPlot(
      cd.map(_.toWattsPerSquareMeter), cd.map(netEnergy(_).to(Petajoules)), cd.map(eroiTot(_)), "Installed Capacity Density [Wi/m2]", "Net Energy [PJ/year]", "EROI")

  }
  def printNetOnshoreOffshore(grids: List[GridCell], eroi: List[Double] = List(5, 8, 10, 12)) {
    eroi.map(e => print(Math.round(WindPotential.netPotential(e, true, grids).to(Exajoules)) + ","))
    println()
    eroi.map(e => print(Math.round(WindPotential.netPotential(e, true, grids.filter(_.onshore)).to(Exajoules)) + ","))
    println()
    eroi.map(e => print(Math.round(WindPotential.netPotential(e, true, grids.filter(_.offshore)).to(Exajoules)) + ","))
    println()

  }
  def plotEnveloppe(grids: List[GridCell], eroi: List[Double], tick: (Boolean, Double, Double) = (true, 100, 2)) {
    val total = eroiFunctionNet("Net Energy Maximization", grids, 1, true)
    PlotHelper.plotXY(List(eroiFunctionNet("EROImin = 1", grids, 1),
      eroiFunctionNet("EROImin = 8", grids, 8),
      eroiFunctionNet("EROImin = 5", grids, 5),
      eroiFunctionNet("EROImin = 10", grids, 10),
      eroiFunctionNet("EROImin = 12", grids, 12),
      eroiFunctionNet("EROImin = 14", grids, 14),
      (eroi.map(e => WindPotential.netPotential(e, true, grids).to(Exajoules)), eroi, "Enveloppe")), "enveloppe",
      xLabel = "Net Wind Potential [EJ/year]", yLabel = "EROImin", tick = tick)
    PlotHelper.plotXY(List(eroiFunctionNet("EROImin = 1", grids, 1),
      eroiFunctionNet("EROImin = 8", grids, 8),
      eroiFunctionNet("EROImin = 5", grids, 5),
      eroiFunctionNet("EROImin = 10", grids, 10),
      eroiFunctionNet("EROImin = 12", grids, 12),
      eroiFunctionNet("EROImin = 14", grids, 14)),
      xLabel = "Net Annual  Production [EJ/year]", yLabel = "EROI", tick = tick)
    PlotHelper.plotXY(List(eroiFunctionNet("EROImin = 1", grids, 1), eroiFunctionNet("EROImin = 8", grids, 8)), "eroiFunction_8",
      xLabel = "Net Annual  Production [EJ/year]", yLabel = "EROI", tick = tick)
    PlotHelper.plotXY(List(eroiFunctionNet("EROImin = 1", grids, 1), eroiFunctionNet("EROImin = 8", grids, 8), eroiFunctionNet("EROImin = 5", grids, 5), eroiFunctionNet("EROImin = 12", grids, 12)), "eroiFunction_5_8_12",
      xLabel = "Net Annual  Production [EJ/year]", yLabel = "EROI", tick = tick)
    PlotHelper.plotXY(List(eroiFunctionNet("EROImin = 1", grids, 1)), "eroiFunction",
      xLabel = "Net Annual Production [EJ/year]", yLabel = "EROI", tick = tick)
  }
  def plotPotential(world: List[GridCell], eroi: List[Double], tick: Double, topDown: Boolean) {
    val offshore = world.filter(_.offshoreEEZ).filter(_.waterDepth.toMeters <= 1000); val onshore = world.filter(_.onshore); val grids = offshore ++ onshore
    val total = (eroi.map(e => WindPotential.potential(e, true, grids).to(Exajoules)), eroi, "Total")

    PlotHelper.plotXY(List(
      total,
      (eroi.map(e => WindPotential.potential(e, true, onshore).to(Exajoules)), eroi, "Onshore"),
      (eroi.map(e => WindPotential.potential(e, true, offshore).to(Exajoules)), eroi, "Offshore")), title = "potential_bw", legend = true,
      xLabel = "Maximum Global Potential [EJ/year]", yLabel = "EROImin", tick = (true, tick, 2.5))

    val fixed = List(2, 4, 9)
    val listFixed = fixed.map(cd => (eroi.map(e => WindPotential.potentialFixedDensity(WattsPerSquareMeter(cd), e, grids, true, topDown).to(Exajoules)), eroi, cd.toString + "MW/km2"))
    PlotHelper.plotXY(total +: listFixed, title = "potential_fixed_bw", xLabel = "Maximum Global Potential [EJ/year]", yLabel = "EROImin", tick = (true, tick, 2.5), legend = true)

    PlotHelper.plotXY(List(
      (eroi.map(e => WindPotential.potential(e, true, offshore.filter(_.waterDepth.toMeters <= 50)).to(Exajoules)), eroi, "Water depth < 50 m"),
      (eroi.map(e => WindPotential.potential(e, true, offshore.filter(g => g.waterDepth.toMeters > 50 && g.waterDepth.toMeters <= 200)).to(Exajoules)), eroi, "50 - 200 m"),
      (eroi.map(e => WindPotential.potential(e, true, offshore.filter(g => g.waterDepth.toMeters > 200 && g.waterDepth.toMeters <= 1000)).to(Exajoules)), eroi, "200 - 1000 m")), title = "offshorePotential_bw", legend = true,
      xLabel = "Maximum Offshore Potential [EJ/year]", yLabel = "EROImin", tick = (true, 20, 2.5))

  }
  def plotPotentialEEAReport(eu: List[GridCell], eroi: List[Double], tick: Double, topDown: Boolean) {
    val offshore = eu.filter(_.offshoreEEZ).filter(_.waterDepth.toMeters <= 1000); val onshore = eu.filter(_.onshore); val grids = offshore ++ onshore
    val total = (eroi.map(e => WindPotential.potential(e, true, grids).to(Exajoules)), eroi, "Total")

    PlotHelper.plotXY(List(
      (eroi.map(e => WindPotential.potentialFixedDensity(WattsPerSquareMeter(10), e, onshore).to(Exajoules) + WindPotential.potentialFixedDensity(WattsPerSquareMeter(8.0 / 1.25), e, offshore).to(Exajoules)), eroi, "Total"),
      (eroi.map(e => WindPotential.potentialFixedDensity(WattsPerSquareMeter(10), e, onshore).to(Exajoules)), eroi, "Onshore"),
      (eroi.map(e => WindPotential.potentialFixedDensity(WattsPerSquareMeter(8.0 / 1.25), e, offshore).to(Exajoules)), eroi, "Offshore")), legend = true, title = "eu_eea",
      xLabel = "Maximum EU-28 Potential [EJ/year]", yLabel = "EROImin", tick = (true, tick, 2.5))

    PlotHelper.plotXY(List(
      (eroi.map(e => WindPotential.potential(e, true, grids).to(Exajoules)), eroi, "Total"),
      (eroi.map(e => WindPotential.potential(e, true, onshore).to(Exajoules)), eroi, "Onshore"),
      (eroi.map(e => WindPotential.potential(e, true, offshore).to(Exajoules)), eroi, "Offshore")), legend = true, title = "eu",
      xLabel = "Maximum EU-28 Potential [EJ/year]", yLabel = "EROImin", tick = (true, tick, 2.5))

  }
  def plotEROIFunction(world: List[GridCell], tick: Double, topDown: Boolean) {
    val offshore = world.filter(_.offshoreEEZ).filter(_.waterDepth.toMeters <= 1000); val onshore = world.filter(_.onshore); val grids = offshore ++ onshore

    PlotHelper.plotXY(List(eroiFunction("EROImin = 0", grids, 0), eroiFunction("EROImin = 8", grids, 8), eroiFunction("EROImin = 12", grids, 12)), "eroiFunction_bw",
      xLabel = "Cumulated Production [EJ/year]", yLabel = "EROI", legend = true, tick = (true, tick, 2.5))

    PlotHelper.plotXY(List(eroiFunction("EROImin = 0", grids, 0),
      eroiFunctionFixedDensity("2 MW/km2", WattsPerSquareMeter(2), grids, 0, true, topDown),
      eroiFunctionFixedDensity("4 MW/km2", WattsPerSquareMeter(4), grids, 0, true, topDown),
      eroiFunctionFixedDensity("9 MW/km2", WattsPerSquareMeter(9), grids, 0, true, topDown)), "eroiFunction_fixed_bw",
      xLabel = "Cumulated Production [EJ/year]", yLabel = "EROI", legend = true, tick = (true, tick, 2.5))

    PlotHelper.plotXY(List(eroiFunction("Total", grids, 0), eroiFunction("Onshore", onshore, 0), eroiFunction("Offshore", offshore, 0)), "eroiFunction0_bw",
      xLabel = "Cumulated Production [EJ/year]", yLabel = "EROI", legend = true, tick = (true, tick, 2.5))
  }
  /*    
     val cd = (0 to 20).map(i => WattsPerSquareMeter(i)).toList
      PlotHelper.plotXY(List(
      (cd.map(_.value), cd.map(potentialFixedDensity(_, 0, grids, topDown = topDown).to(Exajoules)), "Total"),
      (cd.map(_.value), cd.map(potentialFixedDensity(_, 0, onshore, topDown = topDown).to(Exajoules)), "Onshore"),
      (cd.map(_.value), cd.map(potentialFixedDensity(_, 0, offshore, topDown = topDown).to(Exajoules)), "Offshore")), legend = true,
      yLabel = "Wind Potential [EJ/year]", xLabel = "Installed Capacity Density [W/m2]", tick = (true, 2.5, tick))
      */

  def printTable(grids: List[GridCell], eroi: List[Double] = List(2, 5, 8, 10, 12, 15)) {
    val offshore = grids.filter(_.offshoreEEZ).filter(_.waterDepth.toMeters <= 1000); val onshore = grids.filter(_.onshore);
    eroi.map(e =>
      println(round(e)
        + "&" + round(WindPotential.potential(e, true, grids).to(Exajoules)) + "&" + round(WindPotential.netPotential(e, true, grids).to(Exajoules)) + "&" + round(powerInstalled(e, true, grids).to(Terawatts)) + "&" + round(area(e, true, grids).to(SquareKilometers) / 1E6)
        + "&" + round(WindPotential.potential(e, true, onshore).to(Exajoules)) + "&" + round(powerInstalled(e, true, onshore).to(Terawatts))
        + "&" + round(WindPotential.potential(e, true, offshore).to(Exajoules)) + "&" + round(powerInstalled(e, true, offshore).to(Terawatts)) + "\\" + "\\"))
  }
  def printResults(grids: List[GridCell], eroi: List[Double] = List(2, 5, 8, 10, 12, 15)) {
    val offshore = grids.filter(_.offshoreEEZ).filter(_.waterDepth.toMeters <= 1000); val onshore = grids.filter(_.onshore);
    eroi.map(e => println(e + "\t" + round(WindPotential.netPotential(e, true, grids).to(Exajoules)) + "\t" + round(WindPotential.netPotential(e, true, onshore).to(Exajoules)) + "\t" + round(WindPotential.netPotential(e, true, offshore).to(Exajoules))))
  }
  def eroiFunction(name: String = "", cells: List[GridCell], e: Double, suitable: Boolean = true) = {
    val res = Helper.listValueVSCumulated(cells.filter(g => WindPotential.energyPerYear(g, e, suitable).value > 0 && WindPotential.eroi(g, e, suitable) >= 1).map(g => (WindPotential.eroi(g, e, suitable), WindPotential.energyPerYear(g, e, suitable).to(Exajoules))))
    (res._1, res._2, name)
  }
  def eroiFunctionNet(name: String = "", cells: List[GridCell], e: Double, suitable: Boolean = true) = {
    val res = Helper.listValueVSCumulated(cells.filter(g => WindPotential.netEnergyPerYear(g, e, suitable).value > 0 && WindPotential.eroi(g, e, suitable) >= 1).map(g => (WindPotential.eroi(g, e, suitable), WindPotential.netEnergyPerYear(g, e, suitable).to(Exajoules))))
    (res._1, res._2, name)
  }

  def eroiFunctionFixedDensity(name: String = "", density: Irradiance, cells: List[GridCell], e: Double, suitable: Boolean = true, topDown: Boolean = false) = {
    val res = Helper.listValueVSCumulated(cells.filter(g => WindPotential.eroi(g, density, suitable, topDown) >= e).map(g => (WindPotential.eroi(g, density, suitable, topDown), WindPotential.energyPerYear(g, density, suitable, topDown).to(Exajoules))))
    (res._1, res._2, name)
  }

  def round(value: Double) = Math.round(value)
  def round2Dec(value: Double) = (Math.round(value * 100) / 100.0)

  def meanCfCountry(world: WorldGrid, country: String, meanSpeed: Velocity) = {
    val c = world.country(country).filter(_.wind100m.mean >= meanSpeed)
    Math.round(Helper.mean(c.map(g => (g, CapacityFactorCalculation(g) * 1000)))) / 10.0
  }
  /*def meanEfficiency(cells: List[GridCell], density: Irradiance) = {
    Math.round(Helper.mean(cells.map(g => (g, WindPotential.lossFactor(g, density = Some(density)) * WindPotential.availabilityFactor(g) * CapacityFactorCalculation(g) * 1000)))) / 10.0
  }*/
  def meanEfficiency(cells: List[GridCell], eroi_min: Double) = {
    Math.round(Helper.mean(cells.map(g => (g, ((WindPotential.installedCapacity(g, eroi_min, true) * Hours(365 * 24)) / WindPotential.energyPerYear(g, eroi_min, true)) * 1000)))) / 10.0
  }
  def meanCf(cells: List[GridCell]) = {
    Math.round(Helper.mean(cells.map(g => (g, CapacityFactorCalculation(g) * 1000)))) / 10.0
  }
  def meanProductionDensity(cells: List[GridCell], e: Double) = {
    Helper.mean(cells.filter(_.optimalRatedSpeed(e).toMetersPerSecond > 0).map(g => (g, WindPotential.powerDensity(g, e, true).toWattsPerSquareMeter)))
  }
  def minProductionDensity(cells: List[GridCell], e: Double) = cells.filter(_.optimalRatedSpeed(e).toMetersPerSecond > 0).map(WindPotential.powerDensity(_, e, true).toWattsPerSquareMeter).min
  def maxProductionDensity(cells: List[GridCell], e: Double) = cells.filter(_.optimalRatedSpeed(e).toMetersPerSecond > 0).map(WindPotential.powerDensity(_, e, true).toWattsPerSquareMeter).max

  def minCapacityDensity(cells: List[GridCell], e: Double) = cells.filter(_.optimalRatedSpeed(e).toMetersPerSecond > 0).map(WindPotential.capacityDensity(_, e, true).toWattsPerSquareMeter).min
  def maxCapacityDensity(cells: List[GridCell], e: Double) = cells.filter(_.optimalRatedSpeed(e).toMetersPerSecond > 0).map(WindPotential.capacityDensity(_, e, true).toWattsPerSquareMeter).max
  def meanCapacityDensity(cells: List[GridCell], e: Double) = {
    Helper.mean(cells.filter(_.optimalRatedSpeed(e).toMetersPerSecond > 0).map(g => (g, WindPotential.capacityDensity(g, e, true).toWattsPerSquareMeter)))
  }

  //  def potentialTD(world: WorldGrid, eroi_min: Double, suitable: Boolean = true, grids: List[GridCell]): Energy = grids.map(g => WindPotential.energyGeneratedPerYearTopDown(world, g, if (suitable) None else Some(1.0), Some(g.getOptimalCD(eroi_min)))).foldLeft(Joules(0))(_ + _)

  def powerInstalled(eroi_min: Double, suitable: Boolean, grids: List[GridCell]): Power = grids.map(g => WindPotential.installedCapacity(g, eroi_min, suitable)).foldLeft(Watts(0))(_ + _)
  def area(eroi_min: Double, suitable: Boolean, grids: List[GridCell]): Area = grids.map(g => if (g.optimalRatedSpeed(eroi_min).value > 0) (g.suitableArea(suitable,WindPotential)) else SquareKilometers(0)).foldLeft(SquareKilometers(0))(_ + _)

  def testOCDE(world: WorldGrid) {
    val ocde = Helper.getLines("../ocde").map(_(0).toString).toList
    val ocdeGrids = world.countries(ocde).filter(WindPotential.suitabilityFactor(_) > 0).sortBy(CapacityFactorCalculation(_)).reverse
    println(Helper.area(ocdeGrids) + "\t" + ocdeGrids.size)
    for (i <- ocde) {
      println(i + "\t" + Helper.area(ocdeGrids.filter(_.country.name.contains(i))))
    }
    val density = WattsPerSquareMeter(3.125)
    val gridIt = ocdeGrids.toIterator
    var prodTot = 0.0
    var capTot = Watts(0)
    var listCells = List[GridCell]()
    var area = SquareKilometers(0)
    var max = 0
    while (prodTot < 6000) {
      val cell = gridIt.next()
      listCells = listCells :+ cell
      area = area + cell.suitableArea(WindPotential);
      val capCell = WindPotential.installedCapacity(cell, 0, true)
      val prodCell = WindPotential.energyPerYear(cell, density, true, true).to(TerawattHours)
      for (i <- 0 until 100) {
        prodTot = prodTot + prodCell / 100.0
        capTot = capTot + capCell / 100.0
        if (prodTot >= max) {
          max = max + 50
         // println(Math.round(prodTot) + "\t" + meanCf(listCells) + "\t" + meanEfficiency(listCells, density) + "\t" + Math.round(capTot.to(Gigawatts)) + "\t" + Math.round(area.toSquareKilometers / 1E3))
          listCells = List()
        }
      }
    }
  }
}


