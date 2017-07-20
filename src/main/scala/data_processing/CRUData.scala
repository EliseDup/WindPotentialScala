package data_processing

import scala.io.Source
import java.io.PrintStream

object DataPreprocessing {

  def main(args: Array[String]): Unit = {
  
    readCRUData("../resources/cru_data/crad6190/crad6190.dat","solar")
    // readIrradianceData()
    /* val res = Helper.getLines(Helper.resultsPy + "irradiance").map(l => (l(0).toDouble, l(1).toDouble, l(2).toDouble))

    val sorted = res.sortBy(_._2)
    val sorted2 = sorted.sortBy(i => -i._1)
    val out_stream = new PrintStream(new java.io.FileOutputStream("sorted"))

    sorted2.map(l => out_stream.print(l._1.toString + "\t" + l._2.toString + "\t" + l._3.toString + "\n"))*/
  }

  /**
   * CRU file format :
   * http://www.ipcc-data.org/observ/clim/observed_fileformat.html
   *
   * "../ressources/CRU/crad6190/crad6190.dat"
   */
  def readCRUData(from: String, to: String) {

    val lon = (0 until 720).map(i => 0.25 + i * 0.5)
    val lat = (0 until 360).map(i => (89.75 - i * 0.5)).toList

    val file = Source.fromFile(from).getLines().toList
    val dim = file.size
    val months = dim / 360

    var data = Array.ofDim[Double](360, 720)
    val data_month = Array.ofDim[Double](360, 720, 12)

    for (k <- 0 until months) {
      for (i <- 0 until 360) {
        val res = file(k * 360 + i + 2).split("-|\\ ").filter(i => i != "")
        assert(res.size == 720)
        for (j <- 0 until 720) {
          val value = if (res(j) == "9999") 0.0 else res(j).toString.toDouble
          data(i)(j) = data(i)(j) + value
          data_month(i)(j)(k) = value
        }
      }
    }

    val out_stream = new PrintStream(new java.io.FileOutputStream(to))
    for (i <- 0 until 360) {
      for (j <- 0 until 720) {
        data(i)(j) = data(i)(j) / 12.0
        out_stream.print(lat(i) + "\t" + lon(j) + "\t" + data(i)(j))
        for (k <- 0 until 12) {
          out_stream.print("\t" + data_month(i)(j)(k))
        }
        out_stream.print("\n")
      }
    }
    out_stream.close()

  }

}