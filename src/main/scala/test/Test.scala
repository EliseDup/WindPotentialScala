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
import calculation.TheoriticalWindTurbine
import historicalData.Observation
import org.apache.commons.math3.distribution.WeibullDistribution
import historicalData.MeteoDataLoader
import calculation.Enercon82_2000

object Test {
  def main(args: Array[String]) = {
    windPrediction("Bruxelles")
    windRepartition
  }

  def windPrediction(city: String) {
    val turbine = new Enercon82_2000(98, 0.6)

    // Estimate wind power from data
    val wind = Helper.readResult("wind").asInstanceOf[WindData].dataYear(2015)
    val meteo = Helper.readResult("meteo" + city).asInstanceOf[MeteoData].dataYear(2015)
    val nTurbines = wind(wind.size - 1).capacity*Math.pow(10,3) / turbine.ratedPower

    val predictions = meteo.map(i => new Observation(i.time, nTurbines * turbine.power(i.windSpeed) / 1000.0, "Prediction"))
    PlotHelper.plotObservationsWithName(List((predictions, "Predictions"), (wind, "Actuals")))

  }
  def windRepartition {
    def res(city: String) = (Helper.meteo(city).dataYear(2015), city)
    def windSpeed(city: String) = (Helper.meteo(city).dataYear(2015).map(_.windSpeed), city)
    val list = MeteoDataLoader.cities.map(c => windSpeed(c._1))
    PlotHelper.repartition(list, 10)

  }
}