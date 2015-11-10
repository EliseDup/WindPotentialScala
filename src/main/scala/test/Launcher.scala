

package test

import java.io.ObjectInputStream
import java.io.FileInputStream
import org.jfree.data.category.DefaultCategoryDataset
import utils.PlotHelper
import historicalData.LoadData
import historicalData.SolarData
import historicalData.WindData
import utils.Helper
import historicalData.MeteoData

object Launcher {
  
  def main(args: Array[String]) = {
    val wind = Helper.readResult("wind").asInstanceOf[WindData]
    val solar = Helper.readResult("solar").asInstanceOf[SolarData]
    val load = Helper.readResult("load").asInstanceOf[LoadData]
    val meteo =  Helper.readResult("meteo").asInstanceOf[MeteoData]
    
    val helper = new PlotHelper

    val dataset = new DefaultCategoryDataset()
    wind.monthlyAverages.map(i => {dataset.addValue(i.capacityFactor, i.time.getYear,i.time.getMonthOfYear)})
    
    helper.barChart(dataset)
   
    helper.repartition(meteo.windSpeed,10)
   
  }
}
