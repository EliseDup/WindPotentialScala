package utils

import org.joda.time.DateTime
import org.jfree.data.time.TimeSeries
import org.jfree.data.time.Minute
import org.jfree.data.time.TimeSeriesCollection
import org.jfree.chart.ChartFactory
import org.jfree.chart.ChartPanel
import org.jfree.ui.ApplicationFrame
import org.jfree.data.xy.XYSeries
import org.jfree.data.xy.XYSeriesCollection
import org.jfree.chart.plot.PlotOrientation
import org.jfree.chart.JFreeChart
import org.jfree.data.statistics.HistogramDataset
import org.jfree.data.category.DefaultCategoryDataset
import org.jfree.data.xy.WindDataset
import org.jfree.data.xy.DefaultWindDataset
import historicalData.WindData
import org.jfree.data.general.DefaultPieDataset
import historicalData.MeteoData
import historicalData.Observation

object PlotHelper {

  def plotTime(series: List[(List[DateTime], List[Double], String)]) { plotTime(series, "") }
  def plotTime(serie: (List[DateTime], List[Double], String)) { plotTime(List(serie), "") }
  /**
   * Plot a list of time series in on frame
   */
  def plotTime(series: List[(List[DateTime], List[Double], String)], title: String) {
    val dataset = new TimeSeriesCollection()
    series.map { s =>
      val serie = new TimeSeries(s._3)
      (0 until s._1.size).map(i => serie.addOrUpdate(new Minute(s._1(i).toDate()), s._2(i)))
      dataset.addSeries(serie)
    }
    val chart = ChartFactory.createTimeSeriesChart(title, "", "", dataset, true, true, false)
    createFrame(chart)
  }
  
def plotObservations(series: List[(List[Observation])]) { 
val list = for(i <- series) yield (i.map(_.time), i.map(_.value),i(0).name)
plotTime(list,"")
}
def plotObservationsWithName(series: List[(List[Observation],String)]) { 
val list = for(i <- series) yield (i._1.map(_.time), i._1.map(_.value),i._2)
plotTime(list,"")
}
  def plotXY(xys: List[(List[Double], List[Double], String)]) { plotXY(xys, "") }
  def plotXY(xy: (List[Double], List[Double], String)) { plotXY(List(xy), "") }
  def plotXY(xys: List[(List[Double], List[Double], String)], title: String) {
    val dataSet = new XYSeriesCollection()
    xys.map { xy =>
      val serie = new XYSeries(xy._3)
      (0 until xy._1.size).map(i => serie.add(xy._1(i), xy._2(i)))
      dataSet.addSeries(serie)
    }
    val chart = ChartFactory.createXYLineChart(title, "", "", dataSet, PlotOrientation.VERTICAL,
      true, true, false)
    createFrame(chart)
  }

  def histogram(values: List[Double], n: Int, title: String) {
    val dataSet = new HistogramDataset()
    dataSet.addSeries(title, values.toArray, n)
    val chart = ChartFactory.createHistogram("",null, null,dataSet,PlotOrientation.VERTICAL,true, true, false)
    createFrame(chart)
  }

  def repartition(values: List[Double], n: Int) {
    repartition(values, n, "")
  }
  def repartition(values: List[Double], n: Int, title: String) {
    val min = Math.max(0, values.min)
    val max = values.max
    val dataset = new DefaultCategoryDataset()
    // Ajouter le nombre de données dans le nième intervalles en min et max
    val inter = (max - min) / n.toDouble
    var total = 0.0
    for (i <- 0 until n) {
      val size = values.filter(j => j >= i * inter && j < (i + 1) * inter).size
      total = total + size / 4.0
      dataset.addValue(size / 4.0, "", (i + 1) * inter)
    }
    val chart = ChartFactory.createBarChart(title,null,null,dataset,PlotOrientation.VERTICAL,true,true,false)
    createFrame(chart)
  }

  def createFrame(chart: JFreeChart) {
    val chartPanel = new ChartPanel(chart)
    chartPanel.setPreferredSize(new java.awt.Dimension(500, 270))
    val frame = new ApplicationFrame("")
    frame.setContentPane(chartPanel)
    frame.pack()
    frame.setVisible(true)
  }

  def windPlot(meteoData: MeteoData) {
    val serie = new XYSeries("")
    meteoData.windDegrees.map(i => serie.add(i._2, meteoData.observations.filter(_.windDir.equalsIgnoreCase(i._1)).map(_.windSpeed).sum))

    val dataSet = new XYSeriesCollection()
    dataSet.addSeries(serie)

    val chart = ChartFactory.createPolarChart("Wind direction distrubtion", dataSet, true, true, false)
    createFrame(chart)
  }
  def barChart(dataset: DefaultCategoryDataset) {
    val chart = ChartFactory.createBarChart("", null, null, dataset,PlotOrientation.VERTICAL, true, true, false)
    createFrame(chart)
  }

}