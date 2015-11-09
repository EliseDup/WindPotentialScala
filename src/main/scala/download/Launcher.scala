package download

import java.io.ObjectInputStream
import java.io.FileInputStream

object Launcher {
  
  def main(args: Array[String]) = {
    val wind = new ObjectInputStream(new FileInputStream("results/wind")).readObject().asInstanceOf[WindData]
    val solar = new ObjectInputStream(new FileInputStream("results/solar")).readObject().asInstanceOf[SolarData]
    val load = new ObjectInputStream(new FileInputStream("results/load")).readObject().asInstanceOf[LoadData]
    
    val helper = new PlotHelper
    helper.plotTime(wind.monthlyAverages.map(_.time), wind.monthlyAverages.map(_.capacityFactor), "", "")
    //helper.histogram(wind.data.map(_.actual/4.0), 100, "CAPACITY FACTOR WIND")
    //helper.histogram(solar.data.map(_.capacityFactor), 100, "CAPACITY FACTOR SOLAR")
    //helper.plotTime(load.correctedData.map(_.time), load.correctedData.map(_.load))
    //helper.repartition(wind.data.map(_.actual/4.0),100,"Capacity Factor in # hours")
  
    // Analysis for 2014
    val dataYear = wind.dataYear(2013)
    val cap = dataYear.map(_.capacity).max
    val prod = dataYear.map(_.actual).sum/4.0
    val factor = prod/(cap.toDouble*365*24)
    println("TOTAL LOAD" + fullLoadPercent(0,true))
    (2012 to 2015).map(y => println("LOAD YEAR" +"\t"+ y +"\t"+ fullLoadPercent(y,false)))
    // Load hour = # hours with more than 0 production
    def loadPercent(year : Int, total : Boolean) : Double = {
      val d = if(total) wind.data else wind.dataYear(year)
      val nonZeros = d.filter(_.actual>0).size
      nonZeros/d.size.toDouble
    }
     def fullLoadPercent(year : Int, total : Boolean) : Double = {
      val d = if(total) wind.data else wind.dataYear(year)
      val nonZeros = d.filter(i => i.actual==i.capacity).size
      nonZeros/d.size.toDouble
    }
  }
  
}
