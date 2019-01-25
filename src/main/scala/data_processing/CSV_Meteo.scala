

package data_processing

import utils.Helper
import scala.io.Source
import java.io.PrintStream
import java.io.PrintWriter
import java.io.FileOutputStream
import java.io.File
import utils.PlotHelper

object CSV_Meteo {
  def main(args: Array[String]): Unit = {
    val t0 = System.currentTimeMillis()
    plotData("49.705","2.786")
    println("Loading Time " + (System.currentTimeMillis() - t0) / 1000 + " seconds ")
  }

  def writeData {
    val read = Source.fromFile("../G_grid_hourly_2016.csv").getLines()
    for (l <- read) {
      val data = l.split(" ")
      val file = new PrintWriter(new FileOutputStream(new File("resultsCSV/data_lat" + data(1) + "_lon" + data(2) + ".txt"), true))
      file.print(data(3) + "\r\n")
      file.close()
    }
  }
  
  def plotData(lat: String, lon: String) {
    val data = Helper.getLines("resultsCSV/data_lat" + lat + "_lon" + lon + ".txt", ",").map(_(0).toDouble).toList
    val t = Helper.getLines("resultsCSV/time.txt", ",").map(_(0).toDouble).toList
    println(data.size + "\t" + t.size)
    PlotHelper.plotXY((0 until data.size).map(_.toDouble).toList, data)
    PlotHelper.plotXY((0 until 24).map(_.toDouble).toList, (0 until 24).map(data(_)).toList)
  }
  
  def loadLatLon {
    val n = 6093
    val out_stream = new PrintStream(new java.io.FileOutputStream("resultsCSV/lat_lon.txt"))
    val read = Source.fromFile("../G_grid_hourly_2016.csv").getLines()
    var i = 0
    for (l <- read) {
      if (i < n) {
        val data = l.split(" ")
        out_stream.print(data(1) + "\t" + data(2) + "\r\n")
        i = i + 1
      }
    }
    out_stream.close()
  }
  def loadTime {
    val out_stream = new PrintStream(new java.io.FileOutputStream("resultsCSV/time.txt"))
    val read = Source.fromFile("../G_grid_hourly_2016.csv").getLines()
    for (l <- read) {
      val data = l.split(" ")
      if (data(1) == "49" && data(2) == "5.275")
        out_stream.print(data(0) + "\r\n")
    }
    out_stream.close()
  }
}