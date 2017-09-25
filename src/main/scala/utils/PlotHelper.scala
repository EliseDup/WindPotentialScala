package utils

import org.joda.time.DateTime
import org.jfree.data.time._
import org.jfree.chart._
import org.jfree.ui.ApplicationFrame
import org.jfree.data.xy._
import org.jfree.chart.plot._
import org.jfree.chart.JFreeChart
import org.jfree.data.category.DefaultCategoryDataset
import org.jfree.chart.block.BlockBorder
import historical_data.WindEnergyData
import org.jfree.data.general.DefaultPieDataset
import historical_data.MeteoData
import historical_data.Observation
import org.jfree.chart.axis._
import org.jfree.data.statistics._
import org.jfree.ui.RectangleEdge
import org.jfree.chart.renderer.xy._
import java.awt.Color
import java.awt.Font
import java.awt.image.BufferedImage
import java.awt.geom._
import java.awt.BasicStroke
import java.io.FileOutputStream
import com.sun.image.codec.jpeg.JPEGCodec
import org.jfree.util.ShapeUtilities

object PlotHelper {

  def main(args: Array[String]): Unit = {
    val x = (1 to 1000).map(_ * 0.001).toList
    plotXY(List((x, x.map(Math.pow(_, 2)), "x^2"), (x, x.map(_ * 3), "3x"), (x, x.map(_ * 2), "2x"), (x, x, "x")), legend = true)

  }
  val colors = List(Color.BLUE, Color.RED, Color.GREEN, Color.MAGENTA, Color.ORANGE, Color.CYAN, Color.PINK)
  val dashed = List(stroke(Array(1.0f, 0.0f)), stroke(Array(6.0f, 3.0f)), stroke(Array(1.0f, 3.0f)), stroke(Array(6.0f, 3.0f, 1.0f, 3.0f)))

  // val shapes = List(ShapeUtilities.createDiagonalCross(1.0f, 1.0f),ShapeUtilities.createUpTriangle(2.0f),ShapeUtilities.createDiamond(2.0f))

  def stroke(attr: Array[Float], lineWidth: Float = 1.0f) = new BasicStroke(lineWidth, BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND, 1.0f, attr, 0.0f)
  def line(lineWidth: Float = 1.0f) = stroke(Array(1.0f, 0.0f), lineWidth)
  def dash(lineWidth: Float = 1.0f) = stroke(Array(6.0f, 3.0f), lineWidth)
  def dots(lineWidth: Float = 1.0f) = stroke(Array(1.0f, 3.0f), lineWidth)
  def dotsDash(lineWidth: Float = 1.0f) = stroke(Array(6.0f, 3.0f, 1.0f, 3.0f), lineWidth)

  var i = 0
  /**
   * Plot a list of time series in on frame
   */
  def plotTime(times: List[DateTime], values: List[Double]) { plotTime(List((times, values, ""))) }
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
  def plotXY(xy: List[((List[Double], List[Double]), String)], xLabel: String, yLabel: String) { plotXY(xy.map(i => (i._1._1, i._1._2, i._2)), xLabel = xLabel, yLabel = yLabel) }

  def plotXY(xys: List[(List[Double], List[Double], String)], title: String = "", xLabel: String = "", yLabel: String = "",
    legend: Boolean = false, logX: Boolean = false, logY: Boolean = false, save: Boolean = true, tick: (Boolean, Double, Double) = (false, 1, 1)) {

    val dataSet = new XYSeriesCollection()
    xys.map { xy =>
      val serie = new XYSeries(xy._3)
      (0 until xy._1.size).map(i => serie.add(xy._1(i), xy._2(i)))
      dataSet.addSeries(serie)
    }
    val chart = ChartFactory.createXYLineChart("", xLabel, yLabel, dataSet, PlotOrientation.VERTICAL, legend, false, false)
    val plot = chart.getXYPlot();
    val yAxis = plot.getRangeAxis().asInstanceOf[NumberAxis];
    if (logX) plot.setDomainAxis(new LogarithmicAxis(""))
    if (logY) plot.setRangeAxis(new LogarithmicAxis(""))
    //plot.getRangeAxis().setRange(1, 20)
    //plot.getDomainAxis().setRange(0,800)
    createFrame(chart, name = title, save = save, tick = tick)
  }

  def dualAxisPlot(x: List[Double], y1: List[Double], y2: List[Double], xLabel: String, yLabel1: String, yLabel2: String) {
    val dataSet1 = new XYSeriesCollection(); val dataSet2 = new XYSeriesCollection();
    val serie1 = new XYSeries("Primary Axis"); val serie2 = new XYSeries("Secondary Axis");
    (0 until x.size).map { i =>
      serie1.add(x(i), y1(i))
      serie2.add(x(i), y2(i))
    }
    dataSet1.addSeries(serie1); dataSet2.addSeries(serie2);
    val chart = ChartFactory.createXYLineChart("", xLabel, yLabel1, dataSet1, PlotOrientation.VERTICAL, false, false, false)
    val axis2 = new NumberAxis(yLabel2);
    val plot = chart.getXYPlot();
    val r1 = new XYLineAndShapeRenderer(); val r2 = new XYLineAndShapeRenderer();
    r1.setSeriesPaint(1, Color.BLUE);
    r2.setSeriesPaint(1, Color.GREEN);
    r1.setBaseShapesVisible(false);
    r1.setBaseShapesFilled(true);
    r1.setDrawSeriesLineAsPath(true);
    r2.setBaseShapesVisible(false);
    r2.setBaseShapesFilled(true);
    r2.setDrawSeriesLineAsPath(true);

    plot.setRenderer(0, r1); plot.setRenderer(1, r2);
    plot.setRangeAxis(1, axis2);
    plot.setDataset(1, dataSet2);
    plot.mapDatasetToRangeAxis(1, 1);
    createFrame(chart)

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
    createFrame(chart)
  }
  def repartition(value: List[Double]) { repartition(List((value, ""))) }

  def repartition(values: List[(List[Double], String)], n: Int = 10, title: String = "", xLabel: String = "", yLabel: String = "", legend: Boolean = true) {
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
    createFrame(chart, save = false, xy = false)
  }
  def scatterPlot(values: List[(Double, Double)], title: String = "", xLabel: String = "", yLabel: String = "") {
    val y = new LogarithmicAxis(yLabel)
    val x = new NumberAxis(xLabel)
    val plot = new FastScatterPlot(Array(values.map(_._1.toFloat).toArray, values.map(_._2.toFloat).toArray), x, y);
    val chart = new JFreeChart(title, plot);
    createFrame(chart, name = title, save = true, xy = false)
  }

  def createFrame(chart: JFreeChart, name: String = "", save: Boolean = true, shape: Boolean = false, xy: Boolean = true, bw: Boolean = true, tick: (Boolean, Double, Double) = (false, 1, 1)) {

    applyChartTheme(chart, tick)

    if (xy) {
      val n = chart.getXYPlot().getSeriesCount()
      val r = chart.getXYPlot().getRenderer().asInstanceOf[XYLineAndShapeRenderer]
    /*  if (n == 7) {
        for (i <- 0 to 5) {
          r.setSeriesPaint(i, Color.DARK_GRAY)
        }
        for (i <- 1 to 5) {
          r.setSeriesStroke(i, dots(1.0f))
        }
        r.setSeriesStroke(0, line(1.0f))
        r.setSeriesPaint(6, Color.BLACK)
        r.setSeriesStroke(6, line(1.5f))

      } else if (n == 2) {
        r.setSeriesStroke(0, line(1.0f))
        r.setSeriesPaint(0, Color.BLACK)

        r.setSeriesStroke(1, dots(1.0f))
        r.setSeriesPaint(1, Color.BLACK)

      } else if (n == 4) {
        r.setSeriesStroke(0, line(1.0f))
        r.setSeriesPaint(0, Color.BLACK)

        for (i <- 1 to 4) {
          r.setSeriesStroke(i, dots(1.0f))
          r.setSeriesPaint(i, Color.BLACK)
        }
      } else */
      if (!bw) {
        for (i <- 0 until Math.min(n, colors.size)) r.setSeriesPaint(i, colors(i))
        chart.getXYPlot().setRenderer(r);
      } else {
        for (i <- 0 until Math.min(n, dashed.size)) {
          r.setSeriesPaint(i, Color.BLACK)
          r.setSeriesStroke(i, dashed(i))
        }
      }

      /* else if (n == 1) {
        r.setSeriesPaint(0, Color.BLACK)
        r.setSeriesStroke(0, line(1.5f))
      } else if (n <= 6) {
        r.setSeriesPaint(0, Color.DARK_GRAY)
        r.setSeriesStroke(1, line(1.0f))
        for (i <- 1 to n - 1) {
          r.setSeriesPaint(i, Color.BLACK)
          r.setSeriesStroke(i, dots(1.5f))
        }
      }*/
      r.setBaseShapesVisible(false);
      r.setBaseShapesFilled(true);
      r.setDrawSeriesLineAsPath(true);
      chart.getXYPlot().setRenderer(r);

    }
    if (save) {
      ChartUtilities.writeScaledChartAsPNG(new FileOutputStream((if (name.isEmpty()) i else name) + ".jpg"), chart, 500, 270, 5, 5)
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

  def applyChartTheme(chart: JFreeChart, tick: (Boolean, Double, Double)) {
    val chartTheme = StandardChartTheme.createJFreeTheme().asInstanceOf[StandardChartTheme]

    // The default font used by JFreeChart unable to render Chinese properly.
    // We need to provide font which is able to support Chinese rendering.
    val oldExtraLargeFont = chartTheme.getExtraLargeFont();
    val oldLargeFont = chartTheme.getLargeFont();
    val oldRegularFont = chartTheme.getRegularFont();
    val oldSmallFont = chartTheme.getSmallFont();

    val extraLargeFont = new Font("Avenir", oldExtraLargeFont.getStyle(), oldExtraLargeFont.getSize());
    val largeFont = new Font("Avenir", oldLargeFont.getStyle(), oldLargeFont.getSize());
    val regularFont = new Font("Avenir", oldRegularFont.getStyle(), oldRegularFont.getSize());
    val smallFont = new Font("Avenir", oldSmallFont.getStyle(), oldSmallFont.getSize());

    chartTheme.setExtraLargeFont(extraLargeFont);
    chartTheme.setLargeFont(largeFont);
    chartTheme.setRegularFont(regularFont);
    chartTheme.setSmallFont(smallFont);
    chartTheme.apply(chart);

    if (chart.getLegend() != null) chart.getLegend().setFrame(BlockBorder.NONE);
    val plot = chart.getPlot()
    plot.setBackgroundPaint(Color.WHITE)
    plot.setOutlinePaint(Color.WHITE)

    if (tick._1 && plot.isInstanceOf[XYPlot]) {
      val xyPlot = chart.getXYPlot
      xyPlot.getDomainAxis().asInstanceOf[NumberAxis].setTickUnit(new NumberTickUnit(tick._2))
      xyPlot.getRangeAxis().asInstanceOf[NumberAxis].setTickUnit(new NumberTickUnit(tick._3))

    }

  }

}
