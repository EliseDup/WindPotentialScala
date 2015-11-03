package download

object Launcher {
  
  def main(args: Array[String]) = {
    val wind = new WindData
    val meanFactor = wind.windData.map(_.capacityFactor).sum / wind.n
    println(meanFactor + " (" + wind.n + "observations)")
     val helper = new PlotHelper
   //  helper.plotTime(wind.times, wind.windData.map(_.capacity), "Actuals", "")
    helper.plotXY(wind.windData.map(_.capacityFactor), "Capacity Factor",  wind.windData.map(_.capacity), "Total installed", "")
  }

}
