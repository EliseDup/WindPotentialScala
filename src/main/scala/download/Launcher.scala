package download

import scalax.chart.api._

object Launcher {
  def main(args: Array[String]) = {
    val wind = new WindData
    val meanFactor = wind.windData.map(_.capacityFactor).sum / wind.n
    println(meanFactor + " (" + wind.n + "observations)")
  }
}