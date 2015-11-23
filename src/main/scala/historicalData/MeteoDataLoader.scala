package historicalData

import org.joda.time.DateTime
import utils.Helper

/**
 * Load all the data about meteo conditions in a given city in the period [start,end] and
 * save the object in a file
 *
 * We get a measure every 20 minutes
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
  
  val cities = List(("Bruxelles", "EBBR"),
    ("Paris", "LFPO"),
   // ("Londres", "EGLL"),
    ("Rome", "LIRU"),
    ("Madrid", "LEMD")/*,
    ("Ostende","Ostende"),
    ("Spa","EBLB")*/)

  def main(args: Array[String]) = {

    val start = new DateTime(2015, 1, 1, 0, 0)
    val end = new DateTime(2015, 10, 31, 0, 0)

    cities.map(c => {
      println("Load Meteo for :" + c._1)
      Helper.saveResult("meteo" + c._1, new MeteoData(c._2, start, end))
    })
    println("Finished")
  }
}