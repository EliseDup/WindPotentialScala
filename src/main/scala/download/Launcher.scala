package download

import java.io.ObjectInputStream
import java.io.FileInputStream
import org.jfree.data.category.DefaultCategoryDataset
import utils.PlotHelper

object Launcher {
  
  def main(args: Array[String]) = {
    val wind = new ObjectInputStream(new FileInputStream("results/wind")).readObject().asInstanceOf[WindData]
    val solar = new ObjectInputStream(new FileInputStream("results/solar")).readObject().asInstanceOf[SolarData]
    val load = new ObjectInputStream(new FileInputStream("results/load")).readObject().asInstanceOf[LoadData]
    
    val helper = new PlotHelper

    val dataset = new DefaultCategoryDataset()
    wind.monthlyAverages.map(i => {
      println(i)
      dataset.addValue(i.capacityFactor, i.time.getYear,i.time.getMonthOfYear)})
    
    helper.barChart(dataset)
   
   
  }
}
