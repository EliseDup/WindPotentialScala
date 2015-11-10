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

class PlotHelper {

  def plotTime(t: List[DateTime], values: List[Double]) {
    plotTime(t, values, "", "")
  }
  def plotTime(t: List[DateTime], values: List[Double], label: String, title: String) {
    assert(t.size == values.size)
    val series = new TimeSeries(label)
    (0 until t.size).map(i => series.addOrUpdate(new Minute(t(i).toDate()), values(i)))
    val dataset = new TimeSeriesCollection();
    dataset.addSeries(series);
    val chart = ChartFactory.createTimeSeriesChart("", "Date", label, dataset, true, true, false)
    createFrame(chart)
  }
  def plotXY(x: List[Double], y: List[Double]) {
    plotXY(x, "", y, "", "")
  }
  def plotXY(x: List[Double], labelX: String, y: List[Double], labelY: String, title: String) {
    assert(x.size == y.size)
    val series = new XYSeries(title)
    (0 until x.size).map(i => series.add(x(i), y(i)))
    val dataSet = new XYSeriesCollection()
    dataSet.addSeries(series)
    val chart = ChartFactory.createXYLineChart(title, labelX, labelY, dataSet, PlotOrientation.VERTICAL,
      true, true, false)
    createFrame(chart)
  }

  def histogram(values: List[Double], n: Int, title: String) {
    val dataSet = new HistogramDataset()
    dataSet.addSeries(title, values.toArray, n)
    val chart = ChartFactory.createHistogram("",
      null,
      null,
      dataSet,
      PlotOrientation.VERTICAL,
      true,
      true,
      false)
    createFrame(chart)
  }

  def repartition(values: List[Double], n: Int, title: String) {
    val min = values.min
    val max = values.max
    val dataset = new DefaultCategoryDataset()
    // Ajouter le nombre de données dans le nième intervalles en min et max
    val inter = (max - min) / n
    var total = 0.0
    for (i <- 0 until n) {
      val size = values.filter(j => j >= i * inter && j < (i + 1) * inter).size
      total = total + size / 4.0
      dataset.addValue(size / 4.0, "", (i + 1) * inter)
    }
    val chart = ChartFactory.createBarChart(title,
      null,
      null,
      dataset,
      PlotOrientation.VERTICAL,
      true,
      true,
      false)
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

  def barChart(dataset: DefaultCategoryDataset) {
    val chart = ChartFactory.createBarChart("",
      null,
      null,
      dataset,
      PlotOrientation.VERTICAL,
      true,
      true,
      false)
    createFrame(chart)
  }
}