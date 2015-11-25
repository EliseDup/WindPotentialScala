package historicalData

import org.joda.time.DateTime
import utils.Helper

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
   *			... => http://www.wunderground.com/history/
   */
  val stations = new MeteoStations()
  Helper.saveResult("meteoStations", stations)
  val cities = List("Brussels","Roma","Paris","Amsterdam")

  def main(args: Array[String]) = {

    val start = new DateTime(2015, 1, 1, 0, 0)
    val end = new DateTime(2015, 10, 31, 0, 0)

    cities.map(c => {
      println("Load Meteo for :" + c)
      Helper.saveResult("meteo" + c, new MeteoData(stations.stations.find(_.city.equals(c)).get, start, end))
    })
    println("Finished")
  }
}