package historicalData

import org.joda.time.DateTime
import org.joda.time.Days
import org.joda.time.format.DateTimeFormat
import scala.io.Source.{ fromInputStream }
import java.net._

/**
 *  Ligne du csv :
 *  HeureCET,TempératureC,Point de roséeC,Humidité,Pression au niveau de la merhPa,VisibilitéKm,Wind Direction,Vitesse du ventKm/h,Vitesse des rafalesKm/h,Précipitationmm,Evénements,Conditions,WindDirDegrees,DateUTC
 */

class MeteoEntry(time: DateTime, val temp: Double, val humidity: Double,
    val pressure: Double, val windDir: String, val windSpeedkmh: Double, val conditions: String) extends Observation(time, temp, "Meteo") {

  val windSpeed = windSpeedkmh / 3.6
  override def toString() = "Meteo for " + time + ", temp :" + temp + ", wind : " + windSpeed + "[m/s]"
}

object MeteoEntry {
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
 *  - The important think is the AIRPORT and the date: 
 *   Brussels = EBBR
 *   Anvers = 
 *
 * http://www.wunderground.com/history/airport/EBBR/2012/11/10/DailyHistory.html?req_city=Bruxelles&req_state=&req_statename=Belgium&reqdb.zip=00000&reqdb.magic=1&reqdb.wmo=06451&format=1
 */
class MeteoData(val airport: String, val start: DateTime, val end: DateTime) extends HistoricalData[MeteoEntry]("Meteo") {
  
  val temp = observations.map(_.temp)
  val windSpeed = observations.map(_.windSpeed)

  val windDirections = Array("North", "NNE", "NE", "ENE",
    "East", "ESE", "SE", "SSE",
    "South", "SSW", "SW", "WSW",
    "West", "WNW", "NW", "NNW")
  val windDegrees = (0 until windDirections.size).map(i => (windDirections(i), 22.5 * i)).toArray

  def createData: List[MeteoEntry] = {
    val horizonObs = Days.daysBetween(start, end).getDays
    (for (i <- 0 to horizonObs) yield createMeteoDay(i)).flatten.toList
  }
  def createMeteoDay(i: Int): List[MeteoEntry] = {
    @transient val dayFormat = DateTimeFormat.forPattern("yyyy/MM/dd")
    val day = start.plusDays(i).toString(dayFormat)
    println("Load meteo for " + day)
    val urlString = "http://www.wunderground.com/history/airport/"+
      airport+"/" +
      day +
      "/DailyHistory.html?req_city=Bruxelles&req_state=&req_statename=Belgium&reqdb.zip=00000&reqdb.magic=1&reqdb.wmo=06451&format=1"

    val csv = fromInputStream(new URL(urlString).openStream).getLines
    (for (line <- csv; if !line.contains("Time") && line.nonEmpty) yield MeteoEntry(day, line.split(","))).toList
  }
  
  def mean(t : DateTime, list : List[MeteoEntry]) = 
    new MeteoEntry(t, mean(list.map(_.temp)),mean(list.map(_.humidity)),
        mean(list.map(_.pressure)),"NA",mean(list.map(_.windSpeedkmh)),"NA")
  def mean(list : List[Double]):Double= list.sum / list.size.toDouble
}