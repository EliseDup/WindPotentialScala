package historicalData

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

  val cities = stations.stations.filter(s => s.city.equals("Brussels"))

  def main(args: Array[String]) = {
        
    val start = new DateTime(2012, 1, 1, 0, 0)
    val end = new DateTime(2016, 12, 31, 0, 0)
    cities.map(c => {
      println("Load Meteo for :" + c.city)
      val res = new MeteoData(c, start, end)
      res.writeToTxt("results/meteo"+c.city)
    })
    println("Finished")
  }
}

object Test {
  def main(args: Array[String]) = {
    val obs = new MeteoDataLoaded("results/meteoBrussels")
    
     val out_stream = new PrintStream(new java.io.FileOutputStream("BrusselsHourlyWind"))
    obs.hourlyAverages.map(i => out_stream.print(i.toTxt() + "\n"))
    out_stream.close()
    println("printed")
    
    PlotHelper.plotTime(
        List( 
            (obs.hourlyAverages.map(_.time), obs.hourlyAverages.map(_.value),"Hour"),
            (obs.dailyAverages.map(_.time), obs.dailyAverages.map(_.value),"Day"),
            (obs.monthlyAverages.map(_.time), obs.monthlyAverages.map(_.value),"Month")
            ))
    

  }
}