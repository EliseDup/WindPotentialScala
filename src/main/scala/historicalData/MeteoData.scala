package historicalData

import org.joda.time.DateTime
import org.joda.time.Days
import org.joda.time.format.DateTimeFormat
import scala.io.Source.{ fromInputStream }
import java.net._
import org.apache.poi.hssf.usermodel.HSSFWorkbook
import java.io.FileOutputStream
import java.io.BufferedWriter
import java.io.FileWriter
import au.com.bytecode.opencsv.CSVWriter
import scala.collection.mutable.ArrayBuffer
import utils.Helper
import java.io.PrintStream
import java.util.Locale

/**
 *  Ligne du csv :
 *  HeureCET,TempératureC,Point de roséeC,Humidité,Pression au niveau de la merhPa,VisibilitéKm,Wind Direction,Vitesse du ventKm/h,Vitesse des rafalesKm/h,Précipitationmm,Evénements,Conditions,WindDirDegrees,DateUTC
 */

class MeteoEntry(val station: MeteoStation, time: DateTime, val temp: Double, val humidity: Double,
    val pressure: Double, val windDir: String, val windSpeedkmh: Double, val conditions: String) extends Observation(time, temp, "Meteo") {

  val windSpeed = windSpeedkmh / 3.6
  override def toString() = "Meteo for " + time + ", temp :" + temp + ", wind : " + windSpeed + "[m/s]"
  override def toTxt() = time.toString(DateTimeFormat.forPattern("dd/MM/yyyy HH:mm:ss")) + "\t" + temp.toString +"\t" + windDir.toString + "\t" +windSpeed.toString

}

object MeteoEntry {
  @transient val dateFormat = DateTimeFormat.forPattern("yyyy/MM/dd hh:mm a")

  def apply(station: MeteoStation, day: String, csvLine: Array[String]) = {
    new MeteoEntry(station,
      dateFormat.parseDateTime(day + " " + csvLine(0)),
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
class MeteoData(val station: MeteoStation, val start: DateTime, val end: DateTime) extends HistoricalData[MeteoEntry]("Meteo") {

  val temp = observations.map(_.temp)
  val averageTemp = temp.sum / n

  val windSpeed = observations.map(_.windSpeed)
  val averageWind = windSpeed.sum / n

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
    //if (start.plusDays(i).dayOfMonth() == 1) 
    println("Load meteo for " + day)
    val urlString = "https://www.wunderground.com/history/airport/" +
      station.stationID + "/" +
      day +
      "/DailyHistory.html?req_city=Bruxelles&req_state=&req_statename=Belgium&reqdb.zip=00000&reqdb.magic=1&reqdb.wmo=06451&format=1"
    val csv =
      try {
        fromInputStream(new URL(urlString).openStream).getLines
      } catch {
        case e: Exception => List()
      }
    (for (line <- csv; if !line.contains("Time") && line.nonEmpty && line.split(",").size > 1) yield MeteoEntry(station, day, line.split(","))).toList
  }

  def mean(t: DateTime, list: List[MeteoEntry]) =
    new MeteoEntry(station, t, mean(list.map(_.temp)), mean(list.map(_.humidity)),
      mean(list.map(_.pressure)), "NA", mean(list.map(_.windSpeedkmh)), "NA")
  def mean(list: List[Double]): Double = list.sum / list.size.toDouble

  def writeToCSV = {
    val out = new BufferedWriter(new FileWriter("results/meteo" + station.city + ".csv"));
    val writer = new CSVWriter(out);
    writer.writeNext(Array("Time", "Temp", "WindDir", "WindSpeed"))
    for (i <- observations) {
      writer.writeNext(Array(i.time.toDate.toString, i.temp.toString, i.windDir.toString, i.windSpeed.toString))
    }
    out.close
  }
}

class MeteoDataLoaded(val file: String) extends HistoricalData[Observation]("Wind") {
  def createData = {
    val lines = Helper.getLines(file, "\t")
    lines.filter(_.nonEmpty).map(l => new Observation(DateTime.parse(l(0), DateTimeFormat.forPattern("dd/MM/yyyy HH:mm:ss")), l(3).toDouble, "Wind"))
  }
  def mean(t: DateTime, list: List[Observation]) = {
    new Observation(t, list.map(_.value).sum / list.size, "Wind")
  }
}
