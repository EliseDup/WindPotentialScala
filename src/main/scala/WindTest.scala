import wind_energy.WindPotential
import grid.WorldGrid
import utils.Helper
import utils.TerawattHours
import squants.radio.WattsPerSquareMeter
import wind_energy.CapacityFactorCalculation
import utils.PlotHelper
import squants.space.Degrees
import utils.Exajoules
import utils.PetawattHours
import utils.Petajoules
import squants.space.SquareKilometers
import squants.energy.Megawatts
import squants.energy.Gigawatts
import squants.energy.Gigajoules
import squants.time.Hours
import grid.BareAreas
import grid.Forests
import wind_energy.WakeEffect
import squants.energy.Energy
import utils.TonOilEquivalent
import grid.GridCell
import utils.Terawatts
import squants.space.NauticalMiles
import squants.space.Meters
import grid._
import squants.energy.Joules

object WindTest {

  def main(args: Array[String]): Unit = {

    val folder = "../model_data/countries"
    val e = (2 to 40).map(_ * 0.5).toList
    val p = WindPotential(0.5, false)
    val w = WorldGrid.simple()
    printArea(w, "Mexico")
    printArea(w, "Saudi Arabia")
    printArea(w, "Russia")
  }

  def printArea(w: WorldGrid, c: String) {
    println(c + "\t" + w.grids.filter(_.onshore).map(_.area(c).toSquareKilometers).sum)
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

  def printSimplePotential(w: WorldGrid, c: List[String], eroi: Double, name: String) {
    val g = if (c.isEmpty) w.grids else w.countries(c)
    val p = WindPotential(0.5, true)
    println(name + "\t" +
      round2Dec(potential(p, eroi, true, g, c)) + "\t" + round2Dec(potential(p, eroi, true, g.filter(_.onshore), c)) + "\t" + round2Dec(potential(p, eroi, true, g.filter(_.offshore), c)))
  }
  def printOnshorePotential(w: WorldGrid, c: List[String], cf: Double, name: String) {
    val p = WindPotential(0.5, true)
    val tot = (if (c.isEmpty) w.grids else w.countries(c)).filter(_.EEZ).filter(_.onshore)
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
    val e = (2 to 40).map(_ * 0.5).toList
    val p = WindPotential(0.5, false)
    var t0 = System.currentTimeMillis()
    val ke = p.potential_eroi(e, true, WorldGrid().grids.filter(_.EEZ), "Eq. (10)")
    println("Load 1 " + (System.currentTimeMillis() - t0) / 1000.0 + " s "); t0 = System.currentTimeMillis()
    val bu = p.potential_eroi(e, true, WorldGrid.bottomUp().grids.filter(_.EEZ), "Unconstrained")
    println("Load 2 " + (System.currentTimeMillis() - t0) / 1000.0 + " s "); t0 = System.currentTimeMillis()
    val ke_1_5 = p.potential_eroi(e, true, WorldGrid("../Model_data/Wind_Optimization/1_20_by0_5_1_5_dissipation").grids.filter(_.EEZ), "1.5 * Eq. (10)")
    println("Load 3 " + (System.currentTimeMillis() - t0) / 1000.0 + " s "); t0 = System.currentTimeMillis()
    val we1 = p.potential_eroi(e, true, WorldGrid("../Model_data/Wind_Optimization/1_20_by0_5_1We").grids.filter(_.EEZ), "Max 1 We/m2")
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

  def plotForPaper {
    val e = (2 to 40).map(_ * 0.5).toList
    val w = WorldGrid()
    val g = w.grids.filter(_.EEZ)
    // Data.plotEnveloppe(g, e)
    Data.plotPotential(g, e, 100.0)
    Data.plotPotentialEEAReport(w.eu28, e, 5.0)
    plotPotentialTDBU
  }

  def printResultsForPaper {
    val e = (2 to 40).map(_ * 0.5).toList
    val p = WindPotential(0.5, false)
    val w = WorldGrid()
    val g = w.grids.filter(_.EEZ)
    val er1 = List(2, 5, 8, 10, 12, 15).map(_.toDouble)
    val er = List(5, 8, 10, 12).map(_.toDouble)
    val bu = WorldGrid.bottomUp()
    val bu_g = bu.grids.filter(_.EEZ)
    val on = bu_g.filter(_.onshore).filter(_.wind71m.mean.toMetersPerSecond >= 6)

    er1.map(e =>
      println(e.toInt + " & " + round(p.potential(e, true, on)) + " & " + round(p.potentialFixedDensity(WattsPerSquareMeter(2), e, on, true))
        + " & " + round(p.potentialFixedDensity(WattsPerSquareMeter(4), e, on, true))
        + " & " + round(p.potentialFixedDensity(WattsPerSquareMeter(9), e, on, true)) + "\\" + "\\"))

    Data.printTable(g, er1)
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
    val eu = w.eu28.filter(_.EEZ)
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
  def round2Dec(value: Double) = (Math.round(value * 100) / 100.0)

}