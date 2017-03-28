package data_processing

import java.io.PrintStream
import utils.Helper

object ProtectedAreaProcessing {
  def main(args: Array[String]): Unit = {
    protectedAreaResults
    protectedAreaGrid
  }
  // From detailed 0.075 x 0.075 degrees cells' results divided in 100 files, retrieve the % of protected areas in the initial 0.75 x 0.75 file
  def protectedAreaResults {
    // Each 0.75 X 0.75 degrees cell was divided in 10 X 10 subcells
    val nFiles = 100
    val filePrefix = "res_grid0_075_suitable_"
    val fileLocation = "../resources/RScripts_Elise/"
    val grid = Helper.getLines(fileLocation + "grid0_75_suitable", "\t").drop(1).map(l => (l(0).toDouble, l(1).toDouble))
    val n = grid.size
    val subN = n / nFiles
    val outputFile = new PrintStream(new java.io.FileOutputStream("test"))
    for (f <- 1 to nFiles) {
      println("Progress - " + f + "/100")
      val res = Helper.getLines(fileLocation + filePrefix + f.toString, ",").drop(1).map(l => if (l(1).toBoolean) 1 else 0)
      val latsLons = Helper.getLines(fileLocation + "grid0_075_suitable_" + f.toString, "\t").drop(1).map(l => (l(1).toDouble, l(0).toDouble))

      val nPts = res.size / 100
      for (i <- 0 until nPts) {
        // May fail for the last one !
        val nTrue = (0 until 100).map(j => res(j + i * 100)).sum

        val lat = latsLons(i * 100)._1 + 0.375 - 0.075 / 2
        val lon = latsLons(i * 100)._2 + 0.375 - 0.075 / 2
        outputFile.print(lat.toString + "\t" + lon.toString + "\t" + nTrue.toString + "\n")
      }
    }

    outputFile.close()

  }
  // From protected area on suitable areas, retrieve results for the whole grid
  def protectedAreaGrid {
    val res = Helper.getLines("test", "\t").map(l => (l(0).toDouble, l(1).toDouble, l(2).toInt))
    val grid = Helper.getLines("../model_data/grid0_75", "\t").map(l => (l(0).toDouble, l(1).toDouble))
    val outputFile = new PrintStream(new java.io.FileOutputStream("res"))

    for (g <- grid) {
      val p = res.find(i => i._1.equals(g._1) && i._2.equals(g._2)).getOrElse((0, 0, 0))._3
      outputFile.print(g._1.toString + "\t" + g._2.toString + "\t" + p.toDouble.toString + "\n")
    }
    outputFile.close()

  }
}