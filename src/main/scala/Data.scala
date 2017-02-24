

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
import windEnergy.WindPowerTransmission

object Data {

  def main(args: Array[String]): Unit = {
    val world = new WorldGrid("../model_data/data+cd_0_75_23_02.txt", Degrees(0.75))
    val grids = world.onshoreGrids ++ world.grids.filter(_.offshore).filter(_.waterDepth.toMeters <= 1000)
    val onshore = world.onshoreGrids.filter(_.elevation.toMeters <= 2000)
    
    println("Mean Factor" + "\t" + Helper.mean(onshore.map(g => (g,WindPotential.suitabilityFactor(g)))))
    println("SuitableArea" + "\t"+ onshore.map(_.suitableArea).foldLeft(SquareKilometers(0))(_ + _).toSquareKilometers / 1E6)
    
    println(">2000m" + "\t" + onshore.filter(_.elevation.toMeters >=2000).map(_.area).foldLeft(SquareKilometers(0))(_ + _).toSquareKilometers / 1E6)
    println("TotalArea " + "\t" + Helper.area(world.onshoreGrids).toSquareKilometers/1E6)
   
    println("Spars" + "\t" + onshore.map(g => g.area * (g.landCovers.sparseVegetation + g.landCovers.grassland + g.landCovers.bareAreas)).foldLeft(SquareKilometers(0))(_ + _).toSquareKilometers / 1E6)
    println("Forests" + "\t" + onshore.map(g => g.area * (g.landCovers.forests)).foldLeft(SquareKilometers(0))(_ + _).toSquareKilometers / 1E6)
    println("Croplands" + "\t" + onshore.map(g => g.area * (g.landCovers.croplands)).foldLeft(SquareKilometers(0))(_ + _).toSquareKilometers / 1E6)
    println("Shrubland" + "\t" + onshore.map(g => g.area * (g.landCovers.shrubland)).foldLeft(SquareKilometers(0))(_ + _).toSquareKilometers / 1E6)
    println("MosaicVegCrop" + "\t" + onshore.map(g => g.area * (g.landCovers.mosaicVegetationCropland)).foldLeft(SquareKilometers(0))(_ + _).toSquareKilometers / 1E6)
    println("MosaciGroForsSrub" + "\t" + onshore.map(g => g.area * (g.landCovers.mosaicGrasslandForestShrubland)).foldLeft(SquareKilometers(0))(_ + _).toSquareKilometers / 1E6)
    println("Urban" + "\t" + onshore.map(g => g.area * (g.landCovers.urbanAreas)).foldLeft(SquareKilometers(0))(_ + _).toSquareKilometers / 1E6)
    println("IceWater" +"\t" + onshore.map(g => g.area * (g.landCovers.waterBodies + g.landCovers.floodedAreas + g.landCovers.ice + g.landCovers.wetlands)).foldLeft(SquareKilometers(0))(_ + _).toSquareKilometers / 1E6)
  
  }
}