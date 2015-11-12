package historicalData

import org.joda.time.DateTime
import org.joda.time.Days
import org.joda.time.format.DateTimeFormat
import scala.io.Source.{ fromInputStream }
import java.net._
import play.api.libs.json._

/**
 *  Ligne du csv :
 *  HeureCET,TempératureC,Point de roséeC,Humidité,Pression au niveau de la merhPa,VisibilitéKm,Wind Direction,Vitesse du ventKm/h,Vitesse des rafalesKm/h,Précipitationmm,Evénements,Conditions,WindDirDegrees,DateUTC
 */

class MeteoEntry(val time: DateTime, val temp: Double, val humidity: Double,
    val pressure: Double, val windDir: String, val windSpeedkmh: Double, val conditions: String) extends Serializable {

  val windSpeed = windSpeedkmh / 3.6
  override def toString() = "Meteo for " + time + ", temp :" + temp + ", wind : " + windSpeed + "[m/s]"
}

object MeteoEntry extends Serializable {
  @transient val dateFormat = DateTimeFormat.forPattern("yyyy/MM/dd hh:mm a")

  def apply(day: String, csvLine: Array[String]) = {
    new MeteoEntry(dateFormat.parseDateTime(day + " " + csvLine(0)),
      doubleOrNAN(csvLine(1)),
      doubleOrNAN(csvLine(3)),
      doubleOrNAN(csvLine(4)),
      csvLine(6),
      doubleOrZero(csvLine(7)),
      csvLine(12))
  }

  def parseDouble(s: String) = try { Some(s.toDouble) } catch { case _ => None }
  def doubleOrNAN(s: String): Double = parseDouble(s) match {
    case None => Double.NaN
    case Some(d) => d
  }
  def doubleOrZero(s: String): Double = parseDouble(s) match {
    case None => 0.0
    case Some(d) => d
  }
}

/**
 * URL to get a csv with data every 20 minutes :
 *
 * http://www.wunderground.com/history/airport/EBBR/2012/11/10/DailyHistory.html?req_city=Bruxelles&req_state=&req_statename=Belgium&reqdb.zip=00000&reqdb.magic=1&reqdb.wmo=06451&format=1
 */
class MeteoData(val city: String, val start: DateTime, val end: DateTime) extends Serializable {
  val nDay = Days.daysBetween(start, end).getDays
  @transient val dayFormat = DateTimeFormat.forPattern("yyyy/MM/dd")
  
  val windDirections = Array("North", "NNE", "NE", "ENE", 
      "East", "ESE", "SE", "SSE", 
      "South", "SSW", "SW", "WSW", 
      "West", "WNW", "NW", "NNW")
  val windDegrees = (0 until windDirections.size).map(i => (windDirections(i),22.5*i)).toArray
  
  val data = createMeteoData
  val times = data.map(_.time)
  val temp = data.map(_.temp)
  val windSpeed = data.map(_.windSpeed)

  def createMeteoData: List[MeteoEntry] = {
    (for (i <- 0 until nDay) yield createMeteoDay(i)).flatten.toList
  }
  def createMeteoDay(i: Int): List[MeteoEntry] = {
    val day = start.plusDays(i).toString(dayFormat)
    println("Load meteo for " + day)
    val urlString = "http://www.wunderground.com/history/airport/EBBR/" +
      day +
      "/DailyHistory.html?req_city=" +
      city +
      "&req_state=&req_statename=Belgium&reqdb.zip=00000&reqdb.magic=1&reqdb.wmo=06451&format=1"

    val csv = fromInputStream(new URL(urlString).openStream).getLines
    (for (line <- csv; if !line.contains("Time") && line.nonEmpty) yield MeteoEntry(day, line.split(","))).toList
  }
}