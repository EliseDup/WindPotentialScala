package download

import org.joda.time.DateTime
import org.jfree.data.time.TimeSeries
import org.jfree.data.time.Minute
import org.jfree.data.time.TimeSeriesCollection
import org.jfree.chart.ChartFactory
import org.jfree.chart.ChartPanel
import org.jfree.ui.ApplicationFrame
import java.awt.Dimension
import org.jfree.data.xy.XYSeries
import org.jfree.data.xy.XYDataset
import org.jfree.data.xy.DefaultXYDataset
import org.jfree.data.xy.XYSeriesCollection
import org.jfree.chart.plot.PlotOrientation
import org.jfree.chart.JFreeChart
import org.jfree.data.statistics.HistogramDataset

class PlotHelper {

  def plotTime(t: List[DateTime], values: List[Double], label: String, title: String) {
    assert(t.size == values.size)
    val series = new TimeSeries(label)
    (0 until t.size).map(i => series.addOrUpdate(new Minute(t(i).toDate()), values(i)))
    val dataset = new TimeSeriesCollection();
    dataset.addSeries(series);
    val chart = ChartFactory.createTimeSeriesChart("", "Date", label, dataset, true, true, false)
    createFrame(chart)
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
  
  def histogram(values : List[Double], n : Int, title : String){
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
  
  def createFrame(chart : JFreeChart){
    val chartPanel = new ChartPanel(chart)
    chartPanel.setPreferredSize(new java.awt.Dimension(500, 270))
    val frame = new ApplicationFrame("")
    frame.setContentPane(chartPanel)
    frame.pack()
    frame.setVisible(true)
  }
}