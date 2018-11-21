package runs_history

import grid._
import utils._
import squants.space._
import squants.energy._
import wind_energy.WindPotential
import wind_energy.CapacityFactorCalculation
import squants.radio.WattsPerSquareMeter
import squants.time.Hours
import squants.motion.MetersPerSecond

// All the information to run results for the wind paper 
// "Global Wind Potential with Physical and EROI constraints"

object WindPotential_12_2017 {
  val eroi_min = (2 until 40).map(_ * 0.5).toList

  val results = new WorldGrid("runs_history/wind_2017/results_wind_2017", Degrees(0.75), eroi_min, 34, 151, true, false)
  def results_bottom_up = new WorldGrid("runs_history/wind_2017/results_wind_2017_bottom_up", Degrees(0.75), eroi_min, 34, 151, true, false)
  def results_1_5_dissipation = new WorldGrid("runs_history/wind_2017/results_wind_2017_1_5_dissipation", Degrees(0.75), eroi_min, 34, 151, true, false)
  def results_max1we = new WorldGrid("runs_history/wind_2017/results_wind_2017_max1we", Degrees(0.75), eroi_min, 34, 151, true, false)
  

  def main(args: Array[String]): Unit = {
    plotForPaper
    printResultsForPaper
  }

  def plotForPaper {
    val g = results.grids.filter(_.EEZ)
    plotEnveloppe(g, eroi_min)
    plotPotential(g, eroi_min, 100.0)
    plotPotentialEEAReport(results.eu28, eroi_min, 5.0)
    plotPotentialTDBU
  }

  def printArea(c: String) {
    println(c + "\t" + results.grids.filter(_.onshore).map(_.area(c).toSquareKilometers).sum)
  }

  def printPotential(g: List[GridCell], c: String, eroi: Double) {
    val x = g.filter(_.country.name.contains(c)).filter(_.EEZ)
    val on = x.filter(_.onshore)
    val off = x.filter(_.offshore)
    val p = WindPotential(0.5, true)
    println(c + " & " +
      math.round(Helper.area(on).to(SquareKilometers)) + "&" +
      round2Dec(on.map(p.suitableArea(_)).foldLeft(SquareKilometers(0))(_ + _).to(SquareKilometers) / 1E6) + " & " +
      round2Dec(p.installedCapacity(eroi, true, on).to(Terawatts)) + "&" +
      round2Dec(p.potential(eroi, true, on).to(PetawattHours)) + "&" +
      math.round(Helper.area(off).to(SquareKilometers)) + "&" +
      round2Dec(off.map(p.suitableArea(_)).foldLeft(SquareKilometers(0))(_ + _).to(SquareKilometers) / 1E6) + " & " +
      round2Dec(p.installedCapacity(eroi, true, off).to(Terawatts)) + "&" +
      round2Dec(p.potential(eroi, true, off).to(PetawattHours)) + "\\" + "\\")
  }

  def printSimplePotential(c: List[String], eroi: Double, name: String) {
    val g = if (c.isEmpty) results.grids else results.countries(c)
    val p = WindPotential(0.5, true)
    println(name + "\t" +
      round2Dec(potential(p, eroi, true, g, c)) + "\t" + round2Dec(potential(p, eroi, true, g.filter(_.onshore), c)) + "\t" + round2Dec(potential(p, eroi, true, g.filter(_.offshore), c)))
  }
  def printOnshorePotential(c: List[String], cf: Double, name: String) {
    val p = WindPotential(0.5, true)
    val tot = (if (c.isEmpty) results.grids else results.countries(c)).filter(_.EEZ).filter(_.onshore)
    val g = tot.filter(CapacityFactorCalculation(_) >= 0.15)
    if (c.isEmpty) println(name + "\t" +
      math.round(tot.map(x => x.area.toSquareKilometers).sum) + "\t" +
      math.round(g.map(x => x.suitableArea(p).toSquareKilometers).sum) + "\t" + round(potential(p, 1.0, true, g, c)))
    else
      println(name + "\t" +
        math.round(tot.map(x => x.area.toSquareKilometers * x.country.proportion(c)).sum) + "\t" +
        math.round(g.map(x => x.suitableArea(p).toSquareKilometers * x.country.proportion(c)).sum) + "\t" + round(potential(p, 1.0, true, g, c)))
  }

  def potential(p: WindPotential, eroi_min: Double, suitable: Boolean = true, grids: List[GridCell], c: List[String]): Energy =
    if (c.isEmpty) grids.map(g => p.energyPerYear(g, eroi_min, suitable)).foldLeft(Joules(0))(_ + _)
    else grids.map(g => p.energyPerYear(g, eroi_min, suitable) * g.country.proportion(c)).foldLeft(Joules(0))(_ + _)

  def plotPotentialTDBU {
    val p = WindPotential(0.5, false)
    var t0 = System.currentTimeMillis()
    val ke = p.potential_eroi(eroi_min, true, results.grids.filter(_.EEZ), "Eq. (10)")
    println("Load 1 " + (System.currentTimeMillis() - t0) / 1000.0 + " s "); t0 = System.currentTimeMillis()
    val bu = p.potential_eroi(eroi_min, true, results_bottom_up.grids.filter(_.EEZ), "Unconstrained")
    println("Load 2 " + (System.currentTimeMillis() - t0) / 1000.0 + " s "); t0 = System.currentTimeMillis()
    val ke_1_5 = p.potential_eroi(eroi_min, true, results_1_5_dissipation.grids.filter(_.EEZ), "1.5 * Eq. (10)")
    println("Load 3 " + (System.currentTimeMillis() - t0) / 1000.0 + " s "); t0 = System.currentTimeMillis()
    val we1 = p.potential_eroi(eroi_min, true, results_max1we.grids.filter(_.EEZ), "Max 1 We/m2")
    println("Load 3 " + (System.currentTimeMillis() - t0) / 1000.0 + " s "); t0 = System.currentTimeMillis()

    PlotHelper.plotXY(List(
      ke, ke_1_5, we1, bu),
      xLabel = "Maximum Global Potential [EJ/year]", yLabel = "EROImin", tick = (true, 200, 2), title = "paper/potential_BU_TD_2", legend = true)

    val indexes = List(2, 8, 14, 18, 22, 28)
    indexes.map(i => println(ke._2(i).toInt + " & " + math.round(ke._1(i)) + " & "
      + math.round(ke_1_5._1(i)) + " & "
      + math.round(we1._1(i)) + " & "
      + math.round(bu._1(i)) + "\\" + "\\"))

  }

  def printResultsForPaper {
    val p = WindPotential(0.5, false)
    val g = results.grids.filter(_.EEZ)
    val er1 = List(2, 5, 8, 10, 12, 15).map(_.toDouble)
    val er = List(5, 8, 10, 12).map(_.toDouble)
    val bu_g = results_bottom_up.grids.filter(_.EEZ)
    val on = bu_g.filter(_.onshore).filter(_.wind71m.mean.toMetersPerSecond >= 6)

    er1.map(e =>
      println(e.toInt + " & " + round(p.potential(e, true, on)) + " & " + round(p.potentialFixedDensity(WattsPerSquareMeter(2), e, on, true))
        + " & " + round(p.potentialFixedDensity(WattsPerSquareMeter(4), e, on, true))
        + " & " + round(p.potentialFixedDensity(WattsPerSquareMeter(9), e, on, true)) + "\\" + "\\"))

    printTable(g, er1)
    // Abstract
    println("Abstract")
    println(round(p.netPotential(5, true, g)) + "\t" + round(p.netPotential(8, true, g)) + "\t" +
      round(p.netPotential(10, true, g)) + "\t" + round(p.netPotential(12, true, g)))
    // 4.2
    println("Section 4.2")
    println(round(p.potential(1, true, g)) + "\t" + round(p.potential(1, true, g.filter(c => p.eroi(c, 1, true) >= 10))))
    println("Section 4.2")
    println(round(p.potential(8, true, g)) + "\t" + round(p.potential(5, true, g)) + "\t" + round(p.potential(12, true, g)) + "\t" + round(p.potential(15, true, g)))
    println("Onshore - Offshore")
    println(round(p.potential(1, true, g.filter(_.onshore))) + "\t" + round(p.potential(1, true, g.filter(_.offshore))))
    println("Offshore 0-50 + 50-1000")
    println(round(p.potential(1, true, g.filter(_.offshore).filter(i => i.waterDepth.toMeters <= 50))) + "\t" +
      round(p.potential(1, true, g.filter(_.offshore).filter(i => i.waterDepth.toMeters > 50))))

    println("EU 28")
    val eu = results.eu28.filter(_.EEZ)
    println(round(p.potential(5, true, eu)) + "\t" + p.potential(15, true, eu).to(Exajoules))
    println("Results EEA")
    println(
      round(p.potentialFixedDensity(WattsPerSquareMeter(10), 1, eu.filter(_.onshore), true) + p.potentialFixedDensity(WattsPerSquareMeter(8.0 / 1.25), 1, eu.filter(_.offshore), true)) +
        "\t" + round(p.potentialFixedDensity(WattsPerSquareMeter(10), 2, eu.filter(_.onshore), true) + p.potentialFixedDensity(WattsPerSquareMeter(8.0 / 1.25), 2, eu.filter(_.offshore), true)) +
        "\t" + round(p.potentialFixedDensity(WattsPerSquareMeter(10), 5, eu.filter(_.onshore), true) + p.potentialFixedDensity(WattsPerSquareMeter(8.0 / 1.25), 5, eu.filter(_.offshore), true)))
    println("Final Results")

    println("Bottom Up")
    println(round(WindPotential(0.5, false).potential(1, true, bu_g)) + "\t" +
      round(WindPotential(0.5, false).potential(1, true, bu_g.filter(_.onshore))) + "\t" + round(WindPotential(0.5, false).potential(1, true, bu_g.filter(_.offshore))))
    println("Total")
    er.map(i => print(round(WindPotential(0.5, false).potential(i, true, bu_g)) + "\t"))
    println("Onshore")
    er.map(i => print(round(WindPotential(0.5, false).potential(i, true, bu_g.filter(_.onshore))) + "\t"))
    println("Offshore")
    er.map(i => print(round(WindPotential(0.5, false).potential(i, true, bu_g.filter(_.offshore))) + "\t"))
    println("Top Down")
    println("Total")
    er.map(i => print(round(p.potential(i, true, g)) + "\t"))
    println("Onshore")
    er.map(i => print(round(p.potential(i, true, g.filter(_.onshore))) + "\t"))
    println("Offshore")
    er.map(i => print(round(p.potential(i, true, g.filter(_.offshore))) + "\t"))
    println("EU")
    er.map(i => print(round(p.netPotential(i, true, eu)) + "\t"))
    println("Onshore")
    er.map(i => print(round(p.netPotential(i, true, eu.filter(_.onshore))) + "\t"))
    println("Offshore")
    er.map(i => print(round(p.netPotential(i, true, eu.filter(_.offshore))) + "\t"))

    // 

  }
  def round(e: Energy) = Math.round(e.to(Exajoules)).toInt
  def round2Dec(e: Energy) = (Math.round(e.to(Exajoules) * 100) / 100.0)
  def round(value: Double) = (Math.round(value * 100) / 100.0) // Math.round(value)
  def round2Dec(value: Double) = (Math.round(value * 100) / 100.0)

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

}

