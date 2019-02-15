package historical_data

import org.joda.time.DateTime
import utils.Helper
import java.io.BufferedWriter
import java.io.FileWriter
import au.com.bytecode.opencsv.CSVWriter
import scala.io.Source.{ fromInputStream }
import java.net._
import java.io.BufferedReader
import java.io.InputStreamReader
import java.io.PrintStream
import utils.PlotHelper
/**
 * Load all the data about meteo conditions in a given city in the period [start,end] and
 * save the object in a file
 *
 * We get a measure every 20 minutes
 *
 * http://www.wunderground.com/about/faq/international_cities.asp
 * To get information about the airport ID for a given, city, the latitude longitude, elevation,
 */
object MeteoDataLoader {

  /**
   * EBBR = Bruxelles
   * EBAW = Anvers
   * EBCI = Charleroi
   * EBLB = Spa
   * EBOS = Ostende
   * LFPO = Paris
   * EHAM = Amsterdam
   * EGLL = Londres
   * LEMD = Madrid
   * LIRU = Rome
   * 			... => http://www.wunderground.com/history/
   */
  val stations = new MeteoStations()
  val cities = stations.stations.filter(_.countryID.equals("BX")).filter(_.stationID.nonEmpty).filter(c => !c.city.equals("Genk") && !c.city.equals("Ghent") && !c.city.equals("Brasschaat"))

  def main(args: Array[String]) = {
    for (year <- (2017 to 2018)) {
      val prefix = year.toString
      val start = new DateTime(year, 1, 1, 0, 0)
      val end = new DateTime(year, 12, 31, 0, 0)
      cities.map(c => {
        println("Load Meteo for :" + c.city)
        val res = new MeteoData(c, start, end)
        res.writeToTxt("results/meteo/" + c.city + prefix)
        val obs = new MeteoDataLoaded("results/meteo/" + c.city + prefix)
        val out_stream = new PrintStream(new java.io.FileOutputStream("results/hourly/" + c.city + prefix))
        obs.hourlyAverages.map(i => out_stream.print(i.toTxt() + "\n"))
        out_stream.close()
        println(c.city + " printed")

      })
      println("Finished")
    }
  }
}

object Test {
  def main(args: Array[String]) = {
    val obs = new MeteoDataLoaded("results/meteo/meteoKoksijde5years")

    val out_stream = new PrintStream(new java.io.FileOutputStream("results/hourly/windKoksijde5years"))
    obs.hourlyAverages.map(i => out_stream.print(i.toTxt() + "\n"))
    out_stream.close()
    println("printed")

    PlotHelper.plotTime(
      List(
        (obs.hourlyAverages.map(_.time), obs.hourlyAverages.map(_.value), "Hour"),
        (obs.dailyAverages.map(_.time), obs.dailyAverages.map(_.value), "Day"),
        (obs.monthlyAverages.map(_.time), obs.monthlyAverages.map(_.value), "Month")))

  }
}