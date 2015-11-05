package download

object Launcher {
  
  def main(args: Array[String]) = {
    val wind = new WindData
    val solar = new SolarData
    val load = new LoadData
    println("Wind : " + wind.meanFactor + " (" + wind.n + "observations)")
    println("Solar : " + solar.meanFactor + " (" + solar.n + "observations)")

    val helper = new PlotHelper
    helper.plotTime(wind.times, wind.data.map(_.capacity), "", "")
    //helper.histogram(wind.data.map(_.capacityFactor), 100, "CAPACITY FACTOR WIND")
    //helper.histogram(solar.data.map(_.capacityFactor), 100, "CAPACITY FACTOR SOLAR")
    //helper.plotTime(load.correctedData.map(_.time), load.correctedData.map(_.load))
  }
  
}
