package utils

import org.joda.time.DateTime
import org.jfree.data.time._
import org.jfree.chart._
import org.jfree.ui.ApplicationFrame
import org.jfree.data.xy._
import org.jfree.chart.plot._
import org.jfree.chart.JFreeChart
import org.jfree.data.category.DefaultCategoryDataset

import historicalData.WindEnergyData
import org.jfree.data.general.DefaultPieDataset
import historicalData.MeteoData
import historicalData.Observation
import org.jfree.chart.axis._
import org.jfree.data.statistics._
import org.jfree.ui.RectangleEdge
import org.jfree.chart.renderer.xy._
import java.awt.Color
import java.awt.image.BufferedImage
import java.awt.geom._
import java.awt.BasicStroke
import java.io.FileOutputStream
import com.sun.image.codec.jpeg.JPEGCodec

object PlotHelper {

  var i = 0
  /**
   * Plot a list of time series in on frame
   */
  def plotTime(serie: (List[DateTime], List[Double], String)) { plotTime(List(serie)) }
  def plotTime(series: List[(List[DateTime], List[Double], String)], title: String = "", xLabel: String = "", yLabel: String = "", legend: Boolean = false) {
    val dataset = new TimeSeriesCollection()
    series.map { s =>
      val serie = new TimeSeries(s._3)
      (0 until s._1.size).map(i => serie.addOrUpdate(new Minute(s._1(i).toDate()), s._2(i)))
      dataset.addSeries(serie)
    }
    val chart = ChartFactory.createTimeSeriesChart(title, xLabel, yLabel, dataset, legend, true, false)
    createFrame(chart)
  }

  def plotObservations(series: List[(List[Observation])]) {
    val list = for (i <- series) yield (i.map(_.time), i.map(_.value), i(0).name)
    plotTime(list)
  }
  def plotObservationsWithName(series: List[(List[Observation], String)]) {
    val list = for (i <- series) yield (i._1.map(_.time), i._1.map(_.value), i._2)
    plotTime(list)
  }
  def plotXY(xy: (List[Double], List[Double])) { plotXY(List((xy._1, xy._2, ""))) }
  def plotXY(x: List[Double], y: List[Double]) { plotXY(List((x, y, ""))) }
  def plotXY(xy: (List[Double], List[Double], String)) { plotXY(List(xy)) }
  def plotXY(xy: List[( (List[Double],List[Double]), String)], xLabel: String, yLabel: String) { plotXY(xy.map(i => (i._1._1,i._1._2,i._2)), xLabel=xLabel,yLabel=yLabel) }
  
  def plotXY(xys: List[(List[Double], List[Double], String)], title: String = "", xLabel: String = "", yLabel: String = "",
    legend: Boolean = false, logX: Boolean = false, logY: Boolean = false, save: Boolean = true) {
    val dataSet = new XYSeriesCollection()
    xys.map { xy =>
      val serie = new XYSeries(xy._3)
      (0 until xy._1.size).map(i => serie.add(xy._1(i), xy._2(i)))
      dataSet.addSeries(serie)
    }
    val chart = ChartFactory.createXYLineChart(title, xLabel, yLabel, dataSet, PlotOrientation.VERTICAL, legend, false, false)
    val plot = chart.getXYPlot();
    val yAxis = plot.getRangeAxis().asInstanceOf[NumberAxis];
    if (logX) plot.setDomainAxis(new LogarithmicAxis(""))
    if (logY) plot.setRangeAxis(new LogarithmicAxis(""))
    createFrame(chart, save)
  }

  def histogram(values: List[Double], n: Int = 100, title: String = "", xLabel: String = "", yLabel: String = "", legend: Boolean = false) {
    val dataSet = new HistogramDataset()
    dataSet.addSeries(title, values.toArray, n)
    val chart = ChartFactory.createHistogram(title, xLabel, yLabel, dataSet, PlotOrientation.VERTICAL, legend, false, false)
    val plot = chart.getXYPlot();
    createFrame(chart)
  }

  def cumulativeDensity(values: List[Double]) { cumulativeDensity(List((values, "")), legend = false) }
  def cumulativeDensity(values: List[(List[Double], String)], n: Int = 100, title: String = "", xLabel: String = "", yLabel: String = "", legend: Boolean = true) {
    val dataset = new XYSeriesCollection
    values.map(v => {
      val min = Math.min(0, v._1.min)
      val size = v._1.size
      val inter = (v._1.max - min) / n
      val serie = new XYSeries(v._2)
      for (i <- 0 until n) {
        val y = min + i * inter
        val percent = v._1.filter(_ >= y).size / size.toDouble
        serie.add(percent * 100, y)
      }
      dataset.addSeries(serie)
    })
    val chart = ChartFactory.createXYLineChart(title, xLabel, yLabel, dataset, PlotOrientation.VERTICAL, legend, false, false)
    createFrame(chart, false)
  }
  def repartition(value: List[Double]) { repartition(List((value, ""))) }
  def repartition(values: List[(List[Double], String)], n: Int = 10, title: String = "", xLabel: String = "", yLabel: String = "", legend: Boolean = false) {
    val dataset = new DefaultCategoryDataset()
    val allValues = values.map(_._1).flatten
    val min = allValues.min // Math.max(0, allValues.min)
    val max = allValues.max
    val inter = (max - min) / n.toDouble

    for (v <- values) {
      // Ajouter le nombre de données dans le nième intervalles en min et max  
      for (i <- 0 until n) {
        val size = v._1.filter(j => j >= i * inter && j < (i + 1) * inter).size
        dataset.addValue(size, v._2, (i + 1) * inter)
      }
    }
    val chart = ChartFactory.createBarChart(title, xLabel, yLabel, dataset, PlotOrientation.VERTICAL, legend, false, false)
    createFrame(chart, save = false)
  }
  def scatterPlot(values: List[(Double, Double)], title: String = "", xLabel: String = "", yLabel: String = "") {
    val plot = new FastScatterPlot(Array(values.map(_._1.toFloat).toArray, values.map(_._2.toFloat).toArray),
      new NumberAxis(xLabel), new NumberAxis(yLabel));
    val chart = new JFreeChart(title, plot);
    createFrame(chart, true)
  }
  def createFrame(chart: JFreeChart, save: Boolean = true) {
   if (save) {
  /*       val dashedStroke = new BasicStroke(
        1.0f, BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND,
        1.0f, Array(10.0f, 6.0f), 0.0f)

       val r = chart.getXYPlot().getRenderer().asInstanceOf[XYLineAndShapeRenderer]
      r.setSeriesPaint(0, Color.BLUE); 
   	 r.setSeriesPaint(1, Color.BLUE);
      r.setSeriesStroke(1, dashedStroke)
  		r.setSeriesPaint(2, Color.BLUE); r.setSeriesPaint(3, Color.BLUE);
      r.setSeriesStroke(3, dashedStroke)
      r.setSeriesPaint(4, Color.GREEN); r.setSeriesPaint(5, Color.GREEN);
      r.setSeriesStroke(5, dashedStroke)

      r.setBaseShapesVisible(false);
      r.setBaseShapesFilled(true);
      r.setDrawSeriesLineAsPath(true);
      chart.getXYPlot().setRenderer(r);*/
      val plot = chart.getPlot()
      
      plot.setBackgroundPaint(Color.WHITE)
      ChartUtilities.writeScaledChartAsPNG(new FileOutputStream(i + ".jpg"), chart, 500, 300, 2, 2)
      i = i + 1

    }
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
  def barChart(dataset: DefaultCategoryDataset, title: String = "", xLabel: String = "", yLabel: String = "", legend: Boolean = true) {
    val chart = ChartFactory.createBarChart(title, xLabel, yLabel, dataset, PlotOrientation.VERTICAL, legend, true, false)
    createFrame(chart)
  }

}