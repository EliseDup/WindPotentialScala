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
  def main(args: Array[String]) = {
    val start = new DateTime(2015, 1, 1, 0, 0)
    val end = new DateTime(2015, 10, 31, 0, 0)
    val city = "Bruxelles"
    
    Helper.saveObject("meteo",new MeteoData(city, start, end))
      
  }
}