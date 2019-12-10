

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
import wind_energy.WakeEffect
import wind_energy.WindProfile
import wind_energy.WindFarmEnergyInputs
import utils.PetawattHours
import wind_energy.NormalizedPowerCurve
import squants.space.Kilometers
import wind_energy.Weibull

object Data {

  import WindPotential._
  import Helper._
  import PlotHelper._
  def main(args: Array[String]): Unit = {

    println("Start")

    val eroi = (2 to 40).map(_ * 0.5.toDouble).toList
    val t0 = System.currentTimeMillis
    val world = WorldGrid()

    println("Load Grid in " + (System.currentTimeMillis() - t0) / 1000.0 + " seconds")
    val eu = world.eu28.filter(_.offshoreEEZ).filter(WindPotential().suitabilityFactor(_) > 0)
    val test = eu.map(i => (WindPotential().efficiency(i, 1),
      WindPotential().power(i, 1, true).to(Terawatts), i.area.toSquareKilometers,
      i.suitableArea(WindPotential()).toSquareKilometers, WindPotential().installedCapacity(i, 1, true).to(Megawatts)))

    val log = new PrintStream(new java.io.FileOutputStream("wind-offshore-eu28"))
    log.print("Efficiency" + "\t" + "TWe" + "\t" + "Total Area [km2]" + "\t" + "Suitable Area [km2]" + "\t" + "Installed Capacity [MWi]" + "\n")
    test.sortBy(_._1).reverse.map(i => {
      log.print(i._1 + "\t" + i._2 + "\t" + i._3 + "\t" + i._4 + "\t" + i._5 + "\n")
    })
    val list = listValueVSCumulated(test.map(i => (i._1, i._2)), true)
    plotXY(List((list._1, list._2.map(_ * 100), "")), xLabel = "Potential Wind Offshore EU-28 [TWe]", yLabel = "Total Cf [%]", title = "wind-offshore-eu28")
    print("END")
  }

  def oneCellTest(cf: Double) {
    val area = SquareKilometers(1000)
    val ratedPower = Megawatts(2)
    val d = Meters(80)
    // val cf = 0.25
    println(WindPotential().onshoreEnergyInputs(Megawatts(1), Megawatts(1) * cf * 0.8 * Hours(365 * 24), Kilometers(0)).toGigajoules)

    def spacing(installedCap: Irradiance) = Math.sqrt((ratedPower) / (d * d * installedCap))

    def powerDensity(installedCap: Irradiance) = {
      val n = spacing(installedCap); val nT = (installedCap * area) / ratedPower
      cf * installedCap * WakeEffect.arrayEfficiency(nT, Math.PI / (4 * n * n), false)
    }
    def power(installedCap: Irradiance) = {
      val n = spacing(installedCap); val nT = (installedCap * area) / ratedPower
      area * cf * installedCap * WakeEffect.arrayEfficiency(nT, Math.PI / (4 * n * n), false)
    }
    def netEnergy(installedCap: Irradiance) = power(installedCap) * Hours(365 * 24) - (WindPotential().onshoreEnergyInputs(installedCap * area, power(installedCap) * Hours(365 * 24), Kilometers(0)) / 25.0)
    def eroiTot(installedCap: Irradiance) = {
      power(installedCap) * Hours(365 * 24) / (WindPotential().onshoreEnergyInputs(installedCap * area, power(installedCap) * Hours(365 * 24), Kilometers(0)) / 25.0)
    }

    val cd = (5 to 150).map(c => WattsPerSquareMeter(c * 0.1)).toList
    PlotHelper.plotXY(List((cd.map(_.toWattsPerSquareMeter), cd.map(powerDensity(_).to(WattsPerSquareMeter)), "")), xLabel = "Installed Capacity Density [Wi/m2]", yLabel = "Net Energy [PJ/year]")
    PlotHelper.dualAxisPlot(
      cd.map(_.toWattsPerSquareMeter), cd.map(netEnergy(_).to(Petajoules)), cd.map(eroiTot(_)), "Installed Capacity Density [Wi/m2]", "Net Energy [PJ/year]", "EROI")

  }

  def printNetOnshoreOffshore(grids: List[GridCell], eroi: List[Double] = List(5, 8, 10, 12)) {
    eroi.map(e => print(Math.round(WindPotential().netPotential(e, true, grids).to(Exajoules)) + ","))
    println()
    eroi.map(e => print(Math.round(WindPotential().netPotential(e, true, grids.filter(_.onshore)).to(Exajoules)) + ","))
    println()
    eroi.map(e => print(Math.round(WindPotential().netPotential(e, true, grids.filter(_.offshore)).to(Exajoules)) + ","))
    println()

  }

  def plotEnveloppe(grids: List[GridCell], eroi: List[Double], tick: (Boolean, Double, Double) = (true, 100, 2)) {
    val total = WindPotential().eroiFunction(grids, 1, true, "Net Energy Maximization")

    PlotHelper.plotXY(List(WindPotential().eroiFunction(grids, 1, true, "EROImin = 1"),
      WindPotential().eroiFunction(grids, 8, true, "EROImin = 8"),
      WindPotential().eroiFunction(grids, 5, true, "EROImin = 5"),
      WindPotential().eroiFunction(grids, 10, true, "EROImin = 10"),
      WindPotential().eroiFunction(grids, 12, true, "EROImin = 12"),
      WindPotential().eroiFunction(grids, 14, true, "EROImin = 14"),
      (eroi.map(e => WindPotential().potential(e, true, grids).to(Exajoules)), eroi, "Enveloppe")), "paper/enveloppe_bw",
      xLabel = "Wind Potential [EJ/year]", yLabel = "EROImin", tick = tick)

    PlotHelper.plotXY(List(WindPotential().eroiFunction(grids, 1, true, "EROImin = 1"),
      WindPotential().eroiFunction(grids, 8, true, "EROImin = 8")), "paper/eroiFunction_8_bw",
      xLabel = "Cumulated  Production [EJ/year]", yLabel = "EROI", tick = tick)

    PlotHelper.plotXY(List(WindPotential().eroiFunction(grids, 1, true, "EROImin = 1"),
      WindPotential().eroiFunction(grids, 8, true, "EROImin = 8"),
      WindPotential().eroiFunction(grids, 5, true, "EROImin = 5"),
      WindPotential().eroiFunction(grids, 12, true, "EROImin = 12")), "paper/eroiFunction_5_8_12_bw",
      xLabel = "Cumulated  Production [EJ/year]", yLabel = "EROI", tick = tick)
  }

  def plotPotential(world: List[GridCell], eroi: List[Double], tick: Double) {

    val offshore = world.filter(_.offshoreEEZ).filter(_.waterDepth.toMeters <= 1000); val onshore = world.filter(_.onshore); val grids = offshore ++ onshore

    val total = WindPotential().potential_eroi(eroi, true, grids, "Total")
    PlotHelper.plotXY(List(
      total), title = "RegardsEco_Figure4",
      xLabel = "Energie Brute Produite [EJ/an]", yLabel = "TRE", tick = (true, 100.0, 2.0))

    PlotHelper.plotXY(List(
      total,
      WindPotential().potential_eroi(eroi, true, onshore, "Onshore"),
      WindPotential().potential_eroi(eroi, true, offshore, "Offshore")), title = "paper/potential_bw", legend = true,
      xLabel = "Maximum Global Potential [EJ/year]", yLabel = "EROImin", tick = (true, 100.0, 2.0))

    val fixed = List(2, 4, 9)
    val listFixed = fixed.map(cd => (eroi.map(e => WindPotential(0.5, true).potentialFixedDensity(WattsPerSquareMeter(cd), e, grids, true).to(Exajoules)), eroi, cd.toString + "MW/km2"))
    PlotHelper.plotXY(total +: listFixed, title = "paper/potential_fixed_bw", xLabel = "Maximum Global Potential [EJ/year]", yLabel = "EROImin", tick = (true, 100.0, 2.0), legend = true)

    PlotHelper.plotXY(List(
      WindPotential().potential_eroi(eroi, true, offshore.filter(_.waterDepth.toMeters <= 50), "Water depth < 50 m"),
      WindPotential().potential_eroi(eroi, true, offshore.filter(g => g.waterDepth.toMeters > 50 && g.waterDepth.toMeters <= 200), "50 - 200 m"),
      WindPotential().potential_eroi(eroi, true, offshore.filter(g => g.waterDepth.toMeters > 200 && g.waterDepth.toMeters <= 1000), "200 - 1000 m")), title = "paper/offshorePotential_bw", legend = true,
      xLabel = "Maximum Offshore Potential [EJ/year]", yLabel = "EROImin", tick = (true, 20.0, 2.0))

  }
  def plotPotentialEEAReport(eu: List[GridCell], eroi: List[Double], tick: Double) {
    val offshore = eu.filter(_.offshoreEEZ).filter(_.waterDepth.toMeters <= 1000); val onshore = eu.filter(_.onshore); val grids = offshore ++ onshore
    val total = (eroi.map(e => WindPotential().potential(e, true, grids).to(Exajoules)), eroi, "Total")

    PlotHelper.plotXY(List(
      (eroi.map(e => WindPotential().potentialFixedDensity(WattsPerSquareMeter(10), e, onshore).to(Exajoules) + WindPotential().potentialFixedDensity(WattsPerSquareMeter(8.0 / 1.25), e, offshore).to(Exajoules)), eroi, "Total"),
      (eroi.map(e => WindPotential().potentialFixedDensity(WattsPerSquareMeter(10), e, onshore).to(Exajoules)), eroi, "Onshore"),
      (eroi.map(e => WindPotential().potentialFixedDensity(WattsPerSquareMeter(8.0 / 1.25), e, offshore).to(Exajoules)), eroi, "Offshore")), legend = true, title = "paper/eu_eea",
      xLabel = "Maximum EU-28 Potential [EJ/year]", yLabel = "EROImin", tick = (true, 5.0, 2.0))

    PlotHelper.plotXY(List(
      WindPotential().potential_eroi(eroi, true, grids, "Total"),
      WindPotential().potential_eroi(eroi, true, onshore, "Onshore"),
      WindPotential().potential_eroi(eroi, true, offshore, "Offshore")), legend = true, title = "paper/eu",
      xLabel = "Maximum EU-28 Potential [EJ/year]", yLabel = "EROImin", tick = (true, 5, 2.0))

  }

  def printTable(grids: List[GridCell], eroi: List[Double] = List(2, 5, 8, 10, 12, 15)) {
    val offshore = grids.filter(_.offshoreEEZ).filter(_.waterDepth.toMeters <= 1000); val onshore = grids.filter(_.onshore);
    eroi.map(e =>
      println(round(e)
        + "&" + round(WindPotential().potential(e, true, grids).to(Exajoules)) + "&" + round(WindPotential().netPotential(e, true, grids).to(Exajoules)) + "&" + round(WindPotential().powerInstalled(e, true, grids).to(Terawatts)) + "&" + round(WindPotential().area(e, true, grids).to(SquareKilometers) / 1E6)
        + "&" + round(WindPotential().potential(e, true, onshore).to(Exajoules)) + "&" + round(WindPotential().powerInstalled(e, true, onshore).to(Terawatts))
        + "&" + round(WindPotential().potential(e, true, offshore).to(Exajoules)) + "&" + round(WindPotential().powerInstalled(e, true, offshore).to(Terawatts)) + "\\" + "\\"))
  }
  def printResults(grids: List[GridCell], eroi: List[Double] = List(2, 5, 8, 10, 12, 15)) {
    val offshore = grids.filter(_.offshoreEEZ).filter(_.waterDepth.toMeters <= 1000); val onshore = grids.filter(_.onshore);
    eroi.map(e => println(e + "\t" + round(WindPotential().netPotential(e, true, grids).to(Exajoules)) + "\t" + round(WindPotential().netPotential(e, true, onshore).to(Exajoules)) + "\t" + round(WindPotential().netPotential(e, true, offshore).to(Exajoules))))
  }

  def round(value: Double) = (Math.round(value * 100) / 100.0) // Math.round(value)
  def round2Dec(value: Double) = (Math.round(value * 100) / 100.0)

  def testOCDE(world: WorldGrid) {
    val ocde = Helper.getLines("../ocde").map(_(0).toString).toList
    val ocdeGrids = world.countries(ocde).filter(WindPotential().suitabilityFactor(_) > 0).sortBy(CapacityFactorCalculation(_)).reverse
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
      area = area + cell.suitableArea(WindPotential());
      val capCell = WindPotential().installedCapacity(cell, 0, true)
      val prodCell = WindPotential().energyPerYear(cell, density, true).to(TerawattHours)
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


