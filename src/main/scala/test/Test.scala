package test

import calculation.WindTurbine
import utils.PlotHelper
import utils.Helper
import historicalData.WindData
import historicalData.MeteoData
import org.joda.time.DateTime
import org.jfree.data.time.TimeSeries
import org.jfree.data.time.Minute
import org.jfree.data.time.TimeSeriesCollection
import org.jfree.chart.ChartFactory

object Test {
  def main(args: Array[String]) = {
    val turbine = new WindTurbine(2000, 80)
    val speeds = (0 until 3000).map(i => i * 0.01).toList
    val power = speeds.map(v => turbine.power(v))
  //  PlotHelper.plotXY(speeds, power)

    // Estimate wind power from data
    val wind = Helper.readResult("wind").asInstanceOf[WindData].data.filter(i => i.time.getYear==2015 && i.time.getMonthOfYear==10).toList
    val meteo = Helper.readResult("meteo").asInstanceOf[MeteoData].data.filter(_.time.getYear==2015).toList
    val nTurbines = wind(wind.size - 1).capacity / 2.0
    
    val predictions ={
      for(i <- meteo) yield {
        println(i.time + "\t" + i.windSpeed + "\t" + turbine.power(i.windSpeed))
        new Prediction(i.time, nTurbines*turbine.power(turbine.windExtrapolation(i.windSpeed,10.0,80.0,0.6))/1000.0)
      }
    }
    
    val predicted = new TimeSeries("Predicted")
    predictions.map(p => predicted.addOrUpdate(new Minute(p.time.toDate()), p.value))
    val actuals = new TimeSeries("Actual")
    wind.map(p => actuals.addOrUpdate(new Minute(p.time.toDate()), p.actual))
    
    val dataset = new TimeSeriesCollection();
    dataset.addSeries(predicted);dataset.addSeries(actuals)
    val chart = ChartFactory.createTimeSeriesChart("", "", "", dataset, true, true, false)
    PlotHelper.createFrame(chart)
  
  }
  
  class Prediction(val time : DateTime, val value : Double)
}