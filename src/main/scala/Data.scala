

import utils.Helper
import java.io.PrintStream
import utils.PlotHelper
import squants.space.Degrees
import gridData.WorldGrid
import windEnergy.CapacityFactorCalculation
import gridData.WindPotential
import squants.space.SquareKilometers
import squants.radio.WattsPerSquareMeter
import utils.Exajoules
import squants.energy.Joules

object Data {

  def main(args: Array[String]): Unit = {
    val world = new WorldGrid("../WindPotentialPy/results/10years_grid", Degrees(0.75))
    val old = new WorldGrid("results/worldWind5years.txt", Degrees(0.5), old = true)
    world.writeGrid("new")
    old.writeGrid("old")
    
    println(world.totalArea + "\t" + old.totalArea)
    println(world.grids.map(g => WindPotential.suitabilityFactor(g) * g.area).foldLeft(SquareKilometers(0))(_ + _) + "\t" + old.grids.map(g => WindPotential.suitabilityFactor(g) * g.area).foldLeft(SquareKilometers(0))(_ + _))
    
    println("NEW " + world.grids.map(g => WindPotential.energyGeneratedPerYear(g, Some(1.0), Some(WattsPerSquareMeter(4)))).foldLeft(Joules(0))(_ + _).to(Exajoules))
    println("NEW SUITABLE " + world.grids.map(g => WindPotential.energyGeneratedPerYear(g, density=Some(WattsPerSquareMeter(4)))).foldLeft(Joules(0))(_ + _).to(Exajoules))
    
     println("OLD " + old.grids.map(g => WindPotential.energyGeneratedPerYear(g, Some(1.0), Some(WattsPerSquareMeter(4)))).foldLeft(Joules(0))(_ + _).to(Exajoules))
     println("OLD SUITABLE " + old.grids.map(g => WindPotential.energyGeneratedPerYear(g, density=Some(WattsPerSquareMeter(4)))).foldLeft(Joules(0))(_ + _).to(Exajoules))
   
    PlotHelper.cumulativeDensity(List((world.grids.map(CapacityFactorCalculation(_)), "New"), (old.grids.map(CapacityFactorCalculation(_)), "Old")))
    PlotHelper.cumulativeDensity(List((world.grids.map(_.windSpeed.toMetersPerSecond), "New"), (old.grids.map(_.windSpeed.toMetersPerSecond), "Old")))

  }
}