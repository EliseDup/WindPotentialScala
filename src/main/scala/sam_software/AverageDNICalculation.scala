package sam_software

import java.io.File
import utils.Helper._
import java.io.PrintStream

object AverageDNICalculation {

  val sam_folder = new File("/Users/../Applications/SAM.app/Contents/solar_resource/")
  val output_dni = new PrintStream(new java.io.FileOutputStream("sam_locations_dni"))
  val output_ghi = new PrintStream(new java.io.FileOutputStream("sam_locations_ghi"))

  def main(args: Array[String]): Unit = {
    /*    for (f <- sam_folder.listFiles) {
      val res = calculateAverageDNI(f.getAbsolutePath)
      output_dni.print(f.getName + "\t" + res._1 + "\t" + res._2 + "\n")
    }
    output_dni.close()
  */

    for (f <- sam_folder.listFiles.filter(i => !i.getAbsolutePath.endsWith("(INTL).csv"))) {
      val res = calculateAverageGHI(f.getAbsolutePath)
      output_ghi.print(f.getName + "\t" + res._1 + "\t" + res._2 + "\n")
    }
    output_ghi.close()
  }

  def calculateAverageDNI(file: String): (Double, Double) = {
    val (lat, dni): (Double, Array[String]) = {
      if (file.endsWith("(SUNY).csv")) {
        val lines = getLines(file, ",")
        (lines(1)(5).toDouble, lines.map(i => i(7)).toArray)
      } else if (file.endsWith("(TMY3).csv") || file.endsWith("(TMY2).csv") || file.endsWith("(TMY).csv")) {
        val dni = getLines(file, ",").map(i => i(5)).toArray
        (dni(1).toDouble, dni)
      } else {
        val dni = getLines(file, ",").map(i => i(4)).toArray
        (dni(1).toDouble, dni)
      }
    }
    val avg_dni = ((3 until dni.length).map(dni(_).toDouble).sum) * 24 / (dni.length - 3) / 1000.0
    println(file + "\t" + lat + "\t" + avg_dni)
    (lat, avg_dni)
  }

  def calculateAverageGHI(file: String): (Double, Double) = {
    val (lat, dni): (Double, Array[String]) = {
      if (file.endsWith("(SUNY).csv")) {
        val lines = getLines(file, ",")
        (lines(1)(5).toDouble, lines.map(i => i(8)).toArray)
      } else if (file.endsWith("(TMY3).csv") || file.endsWith("(TMY2).csv") || file.endsWith("(TMY).csv")) {
        val lines = getLines(file, ",")
        val dni = lines.map(i => i(4)).toArray
        (lines(1)(5).toDouble, dni)
      } else {
        println(file)
        val dni = getLines(file, ",").map(i => i(4)).toArray
        (dni(1).toDouble, dni)
      }
    }
    val avg_dni = ((3 until dni.length).map(dni(_).toDouble).sum) * 24 / (dni.length - 3) / 1000.0
    println(file + "\t" + lat + "\t" + avg_dni)
    (lat, avg_dni)
  }
}