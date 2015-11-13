package test

import utils.PlotHelper
import utils.Helper
import historicalData.WindData
import historicalData.MeteoData
import org.joda.time.DateTime
import org.jfree.data.time.TimeSeries
import org.jfree.data.time.Minute
import org.jfree.data.time.TimeSeriesCollection
import org.jfree.chart.ChartFactory
import calculation.WindTurbineSpecifications
import historicalData.Observation
import org.apache.commons.math3.distribution.WeibullDistribution

object Test {
  def main(args: Array[String]) = {
    val turbine = WindTurbineSpecifications.TwoMW()
    val s = (0 until 250).map(i => 0.1*i).toList
    //PlotHelper.plotXY( (s,s.map(s => turbine.power(s)),"2MW"))
  
    // Estimate wind power from data
    val wind = Helper.readResult("wind").asInstanceOf[WindData].dataYearMonth(2015, 10)
    val meteo = Helper.readResult("meteoBruxelles").asInstanceOf[MeteoData].dataYearMonth(2015, 10)
    val nTurbines = wind(wind.size - 1).capacity / 2.0

    val predictions = meteo.map(i => new Observation(i.time, nTurbines * turbine.power(i.windSpeed, 10) / 1000.0, "Prediction"))
  //  PlotHelper.plotObservations(List(predictions, wind))
    def res(city : String) = (Helper.meteo(city).dataYearMonth(2015, 10),city)
   
    PlotHelper.plotObservationsWithName(List(res("Bruxelles"),res("Paris"),res("Amsterdam"),res("Londres")))
  }

  
}