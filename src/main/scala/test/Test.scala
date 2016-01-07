package test

import utils.PlotHelper
import utils.Helper
import historicalData.WindEnergyData
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
import historicalData.MeteoStations
import squants.motion.MetersPerSecond
import squants.space.Meters
import calculation.WindTurbine2MW

object Test {
  def main(args: Array[String]) = {
    
    val meteo =Helper.readResult("meteoBrussels").asInstanceOf[MeteoData]
    val stations = Helper.readResult("meteoStations").asInstanceOf[MeteoStations]
   // windPrediction("Brussels")
   windRepartition(List("Brussels"))
  }

  def windPrediction(city: String) {
    val turbine = new WindTurbine2MW

    // Estimate wind power from data
    val wind = Helper.readResult("wind").asInstanceOf[WindEnergyData].dataYearMonth(2015,10)
    val meteo = Helper.readResult("meteo" + city).asInstanceOf[MeteoData].dataYearMonth(2015,10)
    val nTurbines = wind(wind.size - 1).capacity*Math.pow(10,3) / turbine.components.ratedPower.value

    val predictions = meteo.map(i => new Observation(i.time, nTurbines * turbine.power(MetersPerSecond(i.windSpeed),Meters(i.station.elev),Meters(0.4)).value / 1000.0, "Prediction"))
    PlotHelper.plotObservationsWithName(List((predictions, "Predictions"), (wind, "Actuals")))
    val error = Helper.rmse(wind,predictions)
        println("ERROR" + error)
  }
  def windRepartition(cities : List[String]) {
    def res(city: String) = (Helper.meteo(city).dataYear(2015), city)
    def windSpeed(city: String) = (Helper.meteo(city).dataYear(2015).map(_.windSpeed), city)
    val list = cities.map(c => windSpeed(c))
    PlotHelper.repartition(list, 30)

  }
}