

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
import utils.TonOilEquivalent
import utils.TerawattHours
import utils.Terawatts
import squants.time.Hours
import squants.space.Area
import grid.GridCell
import squants.motion.Velocity
import squants.motion.MetersPerSecond
import utils.Thermodynamics
import squants.space.Meters
import org.apache.commons.math3.special.Gamma
import wind_energy.GustavsonWakeEffect
import wind_energy.WindProfile
import wind_energy.WindFarmEnergyInputs
import utils.PetawattHours

object Data {

  def main(args: Array[String]): Unit = {
    println("Start")

    val eroi = (0 to 40).map(_ * 0.5.toDouble).toList
    val t0 = System.currentTimeMillis
    // val world = new WorldGrid("../Model_data/0_20_by0_5_dissipation", Degrees(0.75), eroi)
    val world = new WorldGrid("../Model_data/0_20_by0_5", Degrees(0.75), eroi)
     println("Load Grid in " + (System.currentTimeMillis() - t0) / 1000.0 + " seconds")

    val grids = world.grids.filter(_.EEZ).filter(_.onshore).filter(_.wind71m.mean.toMetersPerSecond >= 6)
     
    for (e <- List(2, 5, 8, 10, 12, 15)) {
      println(e + "&" + round(potential(e, true, grids).to(Exajoules))
        + "&" + round(potentialFixedDensity(WattsPerSquareMeter(2.0), e, grids).to(Exajoules))
        + "&" + round(potentialFixedDensity(WattsPerSquareMeter(4.0), e, grids).to(Exajoules))
        + "&" + round(potentialFixedDensity(WattsPerSquareMeter(9.0), e, grids).to(Exajoules)) + "\\" + "\\")

    }
  }
  def results(world: List[GridCell], eroi: List[Double], tick: Double, topDown: Boolean) {
    val offshore = world.filter(_.offshoreEEZ).filter(_.waterDepth.toMeters <= 1000); val onshore = world.filter(_.onshore); val grids = offshore ++ onshore
    val total = (eroi.map(e => potential(e, true, grids).to(Exajoules)), eroi, "Total")
    PlotHelper.plotXY(List(
      total,
      (eroi.map(e => potential(e, true, onshore).to(Exajoules)), eroi, "Onshore"),
      (eroi.map(e => potential(e, true, offshore).to(Exajoules)), eroi, "Offshore")), legend = true,
      xLabel = "Maximum Global Potential [EJ/year]", yLabel = "EROImin", tick = (true, 5, 2.5))

    PlotHelper.plotXY(List(
      (eroi.map(e => potentialFixedDensity(WattsPerSquareMeter(10), e, onshore).to(Exajoules) + potentialFixedDensity(WattsPerSquareMeter(8.0 / 1.25), e, offshore).to(Exajoules)), eroi, "Total"),
      (eroi.map(e => potentialFixedDensity(WattsPerSquareMeter(10), e, onshore).to(Exajoules)), eroi, "Onshore"),
      (eroi.map(e => potentialFixedDensity(WattsPerSquareMeter(8.0 / 1.25), e, offshore).to(Exajoules)), eroi, "Offshore")), legend = true,
      xLabel = "Maximum Global Potential [EJ/year]", yLabel = "EROImin", tick = (true, 5, 2.5))

    PlotHelper.plotXY(List(
      (eroi.map(e => potential(e, true, onshore).to(Exajoules)), eroi, "Net Energy Maximization"),
      (eroi.map(e => potentialFixedDensity(WattsPerSquareMeter(10), e, onshore).to(Exajoules)), eroi, "EEA Report")), legend = true,
      xLabel = "Maximum Onshore Potential [EJ/year]", yLabel = "EROImin", tick = (true, 5, 2.5))

    PlotHelper.plotXY(List(
      (eroi.map(e => potential(e, true, offshore).to(Exajoules)), eroi, "Net Energy Maximization"),
      (eroi.map(e => potentialFixedDensity(WattsPerSquareMeter(8.0 / 1.25), e, offshore).to(Exajoules)), eroi, "EEA Report")), legend = true,
      xLabel = "Maximum Offshore Potential [EJ/year]", yLabel = "EROImin", tick = (true, 5, 2.5))

    val fixed = List(2, 4, 10)
    val listFixed = fixed.map(cd => (eroi.map(e => potentialFixedDensity(WattsPerSquareMeter(cd), e, grids, true, topDown).to(Exajoules)), eroi, cd.toString + "MW/km2"))

    PlotHelper.plotXY(total +: listFixed, xLabel = "Maximum Global Potential [EJ/year]", yLabel = "EROImin", tick = (true, tick, 2.5), legend = true)

    PlotHelper.plotXY(List(eroiFunction("EROImin = 0", grids, 0), eroiFunction("EROImin = 10", grids, 10), eroiFunctionFixedDensity("2 MW/km2", WattsPerSquareMeter(2), grids, 0, true, topDown), eroiFunctionFixedDensity("4 MW/km2", WattsPerSquareMeter(4), grids, 0, true, topDown)),
      xLabel = "Cumulated Production [EJ/year]", yLabel = "EROI", legend = true, tick = (true, tick, 2.5))

    PlotHelper.plotXY(List(eroiFunction("Total", grids, 0), eroiFunction("Onshore", onshore, 0), eroiFunction("Offshore", offshore, 0)),
      xLabel = "Cumulated Production [EJ/year]", yLabel = "EROI", legend = true, tick = (true, tick, 2.5))

    /*    
     val cd = (0 to 20).map(i => WattsPerSquareMeter(i)).toList
      PlotHelper.plotXY(List(
      (cd.map(_.value), cd.map(potentialFixedDensity(_, 0, grids, topDown = topDown).to(Exajoules)), "Total"),
      (cd.map(_.value), cd.map(potentialFixedDensity(_, 0, onshore, topDown = topDown).to(Exajoules)), "Onshore"),
      (cd.map(_.value), cd.map(potentialFixedDensity(_, 0, offshore, topDown = topDown).to(Exajoules)), "Offshore")), legend = true,
      yLabel = "Wind Potential [EJ/year]", xLabel = "Installed Capacity Density [W/m2]", tick = (true, 2.5, tick))
      */
  }

  def printTable(grids: List[GridCell], eroi: List[Double] = List(2, 5, 8, 10, 12, 15)) {
    val offshore = grids.filter(_.offshoreEEZ).filter(_.waterDepth.toMeters <= 1000); val onshore = grids.filter(_.onshore);
    eroi.map(e =>
      println(round(e)
        + "&" + round(potential(e, true, grids).to(Exajoules)) + "&" + round(netPotential(e, true, grids).to(Exajoules)) + "&" + round(powerInstalled(e, true, grids).to(Terawatts)) + "&" + round(area(e, true, grids).to(SquareKilometers) / 1E6)
        + "&" + round(potential(e, true, onshore).to(Exajoules)) + "&" + round(powerInstalled(e, true, onshore).to(Terawatts))
        + "&" + round(potential(e, true, offshore).to(Exajoules)) + "&" + round(powerInstalled(e, true, offshore).to(Terawatts)) + "\\" + "\\"))
  }
  def printResults(grids: List[GridCell], eroi: List[Double] = List(2, 5, 8, 10, 12, 15)) {
    val offshore = grids.filter(_.offshoreEEZ).filter(_.waterDepth.toMeters <= 1000); val onshore = grids.filter(_.onshore);
    eroi.map(e => println(e + "\t" + round(potential(e, true, grids).to(Exajoules)) + "\t" + round(potential(e, true, onshore).to(Exajoules)) + "\t" + round(potential(e, true, offshore).to(Exajoules))))
  }
  def eroiFunction(name: String = "", cells: List[GridCell], e: Double, suitable: Boolean = true) = {
    val res = Helper.listValueVSCumulated(cells.map(g => (WindPotential.eroi(g, e, suitable), WindPotential.energyPerYear(g, e, suitable).to(Exajoules))))
    (res._1, res._2, name)
  }
  def eroiFunctionFixedDensity(name: String = "", density: Irradiance, cells: List[GridCell], e: Double, suitable: Boolean = true, topDown: Boolean = true) = {
    val res = Helper.listValueVSCumulated(cells.filter(g => WindPotential.eroi(g, density, suitable, topDown) >= e).map(g => (WindPotential.eroi(g, density, suitable, topDown), WindPotential.energyPerYear(g, density, suitable, topDown).to(Exajoules))))
    (res._1, res._2, name)
  }
  def round(value: Double) = Math.round(value)

  def meanCfCountry(world: WorldGrid, country: String, meanSpeed: Velocity) = {
    val c = world.country(country).filter(_.wind100m.mean >= meanSpeed)
    Math.round(Helper.mean(c.map(g => (g, CapacityFactorCalculation(g) * 1000)))) / 10.0
  }
  def meanEfficiency(cells: List[GridCell], density: Option[Irradiance]) = {
    Math.round(Helper.mean(cells.map(g => (g, WindPotential.wakeEffect(g, density = density) * WindPotential.availabilityFactor(g) * CapacityFactorCalculation(g) * 1000)))) / 10.0
  }
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
  def potential(eroi_min: Double, suitable: Boolean = true, grids: List[GridCell]): Energy = grids.map(g => WindPotential.energyPerYear(g, eroi_min, suitable)).foldLeft(Joules(0))(_ + _)
  def netPotential(eroi_min: Double, suitable: Boolean = true, grids: List[GridCell]): Energy = grids.map(g => WindPotential.netEnergyPerYear(g, eroi_min, suitable)).foldLeft(Joules(0))(_ + _)

  //  def potentialTD(world: WorldGrid, eroi_min: Double, suitable: Boolean = true, grids: List[GridCell]): Energy = grids.map(g => WindPotential.energyGeneratedPerYearTopDown(world, g, if (suitable) None else Some(1.0), Some(g.getOptimalCD(eroi_min)))).foldLeft(Joules(0))(_ + _)

  def potentialFixedDensity(density: Irradiance, eroi_min: Double, grids: List[GridCell], suitable: Boolean = true, topDown: Boolean = false): Energy =
    grids.map(g => (if (WindPotential.eroi(g, density, suitable, topDown) >= eroi_min) WindPotential.energyPerYear(g, density, suitable, topDown) else Joules(0))).foldLeft(Joules(0))(_ + _)

  def netPotentialFixedDensity(density: Irradiance, eroi_min: Double, grids: List[GridCell], suitable: Boolean = true, topDown: Boolean = true): Energy =
    grids.map(g => (if (WindPotential.eroi(g, density, suitable, topDown) >= eroi_min) WindPotential.netEnergyPerYear(g, density, suitable, topDown) else Joules(0))).foldLeft(Joules(0))(_ + _)

  def powerInstalled(eroi_min: Double, suitable: Boolean, grids: List[GridCell]): Power = grids.map(g => WindPotential.powerInstalled(g, if (suitable) None else Some(1.0), Some(g.getOptimalCD(eroi_min)))).foldLeft(Watts(0))(_ + _)
  def area(eroi_min: Double, suitable: Boolean, grids: List[GridCell]): Area = grids.map(g => if (g.optimalRatedSpeed(eroi_min).value > 0) (if (suitable) g.suitableArea else g.area) else SquareKilometers(0)).foldLeft(SquareKilometers(0))(_ + _)

  def testOCDE(world: WorldGrid) {
    val ocde = Helper.getLines("../ocde").map(_(0).toString).toList
    val ocdeGrids = world.countries(ocde).filter(WindPotential.suitabilityFactor(_) > 0).sortBy(CapacityFactorCalculation(_)).reverse
    println(Helper.area(ocdeGrids) + "\t" + ocdeGrids.size)
    for (i <- ocde) {
      println(i + "\t" + Helper.area(ocdeGrids.filter(_.country.name.contains(i))))
    }
    val density = Some(WattsPerSquareMeter(3.125))
    val gridIt = ocdeGrids.toIterator
    var prodTot = 0.0
    var capTot = Watts(0)
    var listCells = List[GridCell]()
    var area = SquareKilometers(0)
    var max = 0
    while (prodTot < 6000) {
      val cell = gridIt.next()
      listCells = listCells :+ cell
      area = area + cell.suitableArea;
      val capCell = WindPotential.powerInstalled(cell, density = density)
      val prodCell = WindPotential.energyGeneratedPerYear(cell, density = density).to(TerawattHours)
      for (i <- 0 until 100) {
        prodTot = prodTot + prodCell / 100.0
        capTot = capTot + capCell / 100.0
        if (prodTot >= max) {
          max = max + 50
          println(Math.round(prodTot) + "\t" + meanCf(listCells) + "\t" + meanEfficiency(listCells, density) + "\t" + Math.round(capTot.to(Gigawatts)) + "\t" + Math.round(area.toSquareKilometers / 1E3))
          listCells = List()
        }
      }
    }
  }
}


