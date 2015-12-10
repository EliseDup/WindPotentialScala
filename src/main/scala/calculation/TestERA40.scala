package calculation

import scala.io.Source
import utils.PlotHelper
import utils.Helper
import java.io.FileWriter
import au.com.bytecode.opencsv.CSVWriter

object TestERA40 {
  def main(args: Array[String]) = {
    // Helper.txtToCSV(Helper.ressourcesPy +"windCLCEurope", Helper.ressourcesPy+"windEurope.csv")
    val era = new ERA40Wind()

    PlotHelper.cumulativeDensity(era.grids.map(_.windSpeed), 100, "10 meters mean speed")
    PlotHelper.cumulativeDensity(era.grids.map(_.windSpeed(80)), 100, "80 meters mean speed")

    val z0 = (0 until 100).map(_ * 0.01).toList
    val coef = z0.map(z => Math.log(80 / z) / Math.log(10 / z)).toList
    // PlotHelper.plotXY((z0,coef,"Conversion ratio VS roughness length"))
  }
}
class ERA40Wind {
  // Coefficients for wind extrapolation depends on CLC class
  val clcClasses = new CorineLandCoverClasses()
  val grids: List[GridObject] = {
    val lines = Source.fromFile(Helper.ressourcesPy + "windCLCEurope").getLines().toList
    lines.map(l => GridObject(l,clcClasses)).toList
  }
  println("nGrid" + grids.size)
  (1 to 5).map(i => 
    println("Level 1 :"+i+" "+grids.filter(g => 
    g.clc.level1==i).size))
}

/**
 * From data of ERA-40 dataset
 *
 */
class GridObject(val uWind: Double, val vWind: Double, val windSpeed: Double,
    val lat: Double, val lon: Double, val clc: CorineLandCoverClass) {

  def windSpeed(h: Double, z0 : Double): Double = windSpeed * math.log(h / z0) / math.log(10 / z0)
  def windSpeed(h : Double):Double = clc.hubHeigthConversionRatio._2 * windSpeed
  
  val powerDensity = Math.pow(windSpeed, 3) * 0.625
}

object GridObject {
  def apply(line: String, clcClasses : CorineLandCoverClasses) = {
    val csvLine = line.split("\t")
    val clcClass =  if (csvLine(5).equals("NA")) clcClasses(50) else clcClasses(csvLine(5).toInt)
    new GridObject(csvLine(0).toDouble, csvLine(1).toDouble, csvLine(2).toDouble, csvLine(3).toDouble,
      csvLine(4).toDouble, clcClass)
  }
}

