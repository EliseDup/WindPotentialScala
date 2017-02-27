

import utils.Helper
import java.io.PrintStream
import utils.PlotHelper
import squants.space.Degrees
import grid.WorldGrid
import wind_energy.CapacityFactorCalculation
import grid.WindPotential
import squants.space.SquareKilometers
import squants.radio.WattsPerSquareMeter
import utils.Exajoules
import squants.energy.Joules
import wind_energy.WindPowerTransmission
import grid.GlobCoverClasses
import java.io.FileOutputStream

object Data {

  def main(args: Array[String]): Unit = {
    val t0 = System.currentTimeMillis()
    val world = new WorldGrid("../model_data/data+cd_0_75_23_02.txt", Degrees(0.75))
    println("Load Grid in " + (System.currentTimeMillis() - t0)/1000.0 + " seconds")
    val grids = world.onshoreGrids ++ world.grids.filter(_.offshore).filter(_.waterDepth.toMeters <= 1000)
    val onshore = world.onshoreGrids.filter(_.elevation.toMeters <= 2000)
    
    println("Mean Factor" + "\t" + Helper.mean(onshore.map(g => (g,WindPotential.suitabilityFactor(g)))))
    println("SuitableArea" + "\t"+ onshore.map(_.suitableArea).foldLeft(SquareKilometers(0))(_ + _).toSquareKilometers / 1E6)
    
    println(">2000m" + "\t" + onshore.filter(_.elevation.toMeters >=2000).map(_.area).foldLeft(SquareKilometers(0))(_ + _).toSquareKilometers / 1E6)
    println("TotalArea " + "\t" + Helper.area(world.onshoreGrids).toSquareKilometers/1E6)
   
    for(t <- GlobCoverClasses.types) {
      println(t.name + "\t" + onshore.map(_.area(t)).foldLeft(SquareKilometers(0))(_ + _).toSquareKilometers/ 1E6 )
    }
  }
}

object ProtectedAreaProcessing {
  def main(args: Array[String]): Unit = {
    // Each 0.75 X 0.75 degrees cell was divided in 10 X 10 subcells
    val nFiles = 100
    val filePrefix = "res_grid0_075_suitable_"
    val fileLocation = "../resources/RScripts_Elise/"
    val grid = Helper.getLines(fileLocation+ "grid0_75_suitable", "\t").drop(1).map(l => (l(0).toDouble, l(1).toDouble))
    val n = grid.size
    val subN = n / nFiles
    val outputFile = new PrintStream(new java.io.FileOutputStream("res_grid0_75_suitable"))
    for(f <-  1 to nFiles){
      println("Progress - " + f + "/100")
      val res = Helper.getLines(fileLocation+filePrefix+f.toString, ",").drop(1).map(l => if(l(1).toBoolean) 1 else 0)
      val nPts = res.size / 100
      for(i <- 0 until nPts){
        // May fail for the last one !
        val gridIndex = (f-1)*nPts + i
        val nTrue = (0 until 100).map(j => res(j + i*100)).sum
        outputFile.print(grid(gridIndex)._1.toString + "\t" + grid(gridIndex)._2.toString  + "\t" + nTrue.toString + "\n")
      }
    }
      
    outputFile.close()
  }
}