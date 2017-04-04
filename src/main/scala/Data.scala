

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

object Data {

  def main(args: Array[String]): Unit = {
println(MegawattHours(23000).toGigajoules)
println(MegawattHours(17000).toGigajoules)
    val t0 = System.currentTimeMillis()
    val world = WorldGrid.simple()
    println("Load Grid in " + (System.currentTimeMillis() - t0) / 1000.0 + " seconds")
    val offshore = world.grids.filter(_.offshore).filter(_.waterDepth.toMeters <= 1000)
    val grids = world.onshoreGrids ++ offshore

    world.writeGrid("world_c_k", grids)
    /* 
    println(potential(0.0, true, grids).to(Exajoules))
    println(potential(12.0, true, grids).to(Exajoules))
      
    println(potential(0.0, false, grids).to(Exajoules))
    println(potential(12.0, false, grids).to(Exajoules))
    
    println(potentialFixedDensity(WattsPerSquareMeter(2), 0.0, grids).to(Exajoules))
    println(potentialFixedDensity(WattsPerSquareMeter(4), 0.0, grids).to(Exajoules))
    println(potentialFixedDensity(WattsPerSquareMeter(10), 0.0, grids).to(Exajoules))
    

    val ocde = Helper.getLines("../ocde").map(_(0).toString).toList
    val ocdeGrids = world.countries(ocde).filter(WindPotential.suitabilityFactor(_) > 0).sortBy(CapacityFactorCalculation(_)).reverse
    println(Helper.area(ocdeGrids) + "\t" + ocdeGrids.size)
    for (i <- ocde) {
      println(i + "\t" + Helper.area(ocdeGrids.filter(_.country.name.contains(i))))
    }
    val density = Some(WattsPerSquareMeter(3.125))
    val gridIt =ocdeGrids.toIterator
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

    val eroi_min = (0 to 40).map(_ * 0.5).toList

    val c = List(2, 10, 20)
    val l = c.map(c => (eroi_min, eroi_min.map(e => potentialFixedDensity(WattsPerSquareMeter(c), e, grids).to(Exajoules)), c.toString + "W/km2"))

    PlotHelper.plotXY(List((eroi_min, eroi_min.map(e => potential(e, true, grids).to(Exajoules)), "Optimal CD")) ++ l, legend = true,
      yLabel = "Global Wind Potential [EJ/year]", xLabel = "EROImin", tick = (true, 2.5, 250))

    val cd = (0 to 1000).map(i => WattsPerSquareMeter(i * 0.025)).toList

    PlotHelper.plotXY(List(
      (cd.map(_.value), cd.map(potentialFixedDensity(_, 0, grids).to(Exajoules)), "Total"),
      (cd.map(_.value), cd.map(potentialFixedDensity(_, 0, world.onshoreGrids).to(Exajoules)), "Onshore"),
      (cd.map(_.value), cd.map(potentialFixedDensity(_, 0, offshore).to(Exajoules)), "Offshore")), legend = true,
      yLabel = "Global Wind Potential [EJ/year]", xLabel = "Installed Capacity Density [W/m2]", tick = (true, 2.5, 250))
*/
  }

  def meanCfCountry(world: WorldGrid, country: String, meanSpeed: Velocity) = {
    val c = world.country(country).filter(_.wind100m.mean >= meanSpeed)
    Math.round(Helper.mean(c.map(g => (g, CapacityFactorCalculation(g) * 1000)))) / 10.0
  }
  def meanEfficiency(cells: List[GridCell], density: Option[Irradiance]) = {
    Math.round(Helper.mean(cells.map(g => (g, WindPotential.wakeEffect(g, density = density) * WindPotential.availabilityFactor(g) * CapacityFactorCalculation(g) * 1000)))) / 10.0
  }
  def meanCf(cells: List[GridCell]) = {
    Math.round(Helper.mean(cells.map(g => (g, CapacityFactorCalculation(g) * 1000)))) / 10.0
  }

  def potential(eroi_min: Double, suitable: Boolean = true, grids: List[GridCell]): Energy = grids.map(g => WindPotential.energyGeneratedPerYear(g, if (suitable) None else Some(1.0), Some(g.getOptimalCD(eroi_min)))).foldLeft(Joules(0))(_ + _)
  def potentialTD(world: WorldGrid, eroi_min: Double, suitable: Boolean = true, grids: List[GridCell]): Energy = grids.map(g => WindPotential.energyGeneratedPerYearTopDown(world, g, if (suitable) None else Some(1.0), Some(g.getOptimalCD(eroi_min)))).foldLeft(Joules(0))(_ + _)

  def potentialFixedDensity(density: Irradiance, eroi_min: Double, grids: List[GridCell]): Energy =
    grids.map(g => (if (WindPotential.EROI(g, density = Some(density)) > eroi_min) WindPotential.energyGeneratedPerYear(g, density = Some(density)) else Joules(0))).foldLeft(Joules(0))(_ + _)

  def powerInstalled(eroi_min: Double, suitable: Boolean, grids: List[GridCell]): Power = grids.map(g => WindPotential.powerInstalled(g, if (suitable) None else Some(1.0), Some(g.getOptimalCD(eroi_min)))).foldLeft(Watts(0))(_ + _)
  def area(eroi_min: Double, suitable: Boolean, grids: List[GridCell]): Area = grids.map(g => if (g.getOptimalCD(eroi_min).value > 0) (if (suitable) g.suitableArea else g.area) else SquareKilometers(0)).foldLeft(SquareKilometers(0))(_ + _)

}

