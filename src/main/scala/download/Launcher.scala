package download

object Launcher {

  def main(args: Array[String]) = {
    val wind = new WindData
    val solar = new SolarData
    println("Wind : " + wind.meanFactor + " (" + wind.n + "observations)")
    println("Solar : " + solar.meanFactor + " (" + solar.n + "observations)")

    val helper = new PlotHelper
    helper.plotTime(wind.times, wind.data.map(_.capacity), "", "")
    //helper.plotXY(wind.windData.map(_.capacityFactor), "Capacity Factor",  wind.windData.map(_.capacity), "Total installed", "")
    //helper.histogram(wind.data.map(_.capacityFactor), 100, "")
  }
  
}
