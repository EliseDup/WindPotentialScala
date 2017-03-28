package historical_data

import org.joda.time.DateTime
import org.joda.time.Days
import org.joda.time.Months
import java.io.PrintStream
import org.joda.time.format.DateTimeFormat
/**
 * Generic class to load and store historical data
 */

class Observation(val time: DateTime, val value: Double, val name: String) extends Serializable {
  override def toString() = "Observation of " + name + " on " + time + " : " + value
  
  def toTxt() = time.toString(DateTimeFormat.forPattern("dd/MM/yyyy HH:mm:ss")) + "\t" + value.toString
}

abstract class HistoricalData[A <: Observation](name: String) extends Serializable {
  val observations = createData
  
  def createData: List[A]
  def mean(t:DateTime,list : List[A]) : A
 
  val n = observations.size
  val times = observations.map(_.time)
  val values = observations.map(_.value)
  
  def nDays = {
    if(observations.size == 0) 0
    else Days.daysBetween(observations(0).time, observations(n - 1).time).getDays() + 1
  }
  
  def nMonths = Months.monthsBetween(observations(0).time, observations(n - 1).time).getMonths() + 1

  def sameHour(d1: DateTime, d2: DateTime): Boolean = d1.dayOfYear == d2.dayOfYear && d1.hourOfDay == d2.hourOfDay
  def sameDay(d1: DateTime, d2: DateTime): Boolean = d1.dayOfYear == d2.dayOfYear
  def sameMonth(d1: DateTime, d2: DateTime): Boolean = d1.monthOfYear == d2.monthOfYear

  def hourlyAverages = {
    (for (i <- 0 to Math.min(365,nDays); h <- 0 until 24) yield {
      val day = observations(0).time.withTimeAtStartOfDay.plusDays(i).plusHours(h)
      mean(day,observations.filter(i => sameHour(day, i.time)).filter(_.value > 0))
    }).toList
  }
  def dailyAverages = {
    (for (i <- 0 to Math.min(365,nDays) ) yield {
      val day = observations(0).time.withTimeAtStartOfDay.plusDays(i)
      mean(day,observations.filter(i => sameDay(day, i.time)).filter(_.value > 0))
    }).toList
  }
  def monthlyAverages = {
    (for (i <- 0 until 12 ) yield {
      val month = observations(0).time.withTimeAtStartOfDay.withDayOfMonth(1).plusMonths(i)
      mean(month,observations.filter(i => sameMonth(month, i.time)).filter(_.value > 0))
      //new PowerGenerationObservation(month, d.map(_.forecast / 4.0).sum, d.map(_.actual / 4.0).sum, d.map(_.capacity / 4.0).sum, name)
    }).toList
  }
  
  // Return a subset with the observations for a given year, or a given month of a year
  def dataYear(y: Int) = observations.filter(t => t.time.getYear == y && t.value >=0).toList
  def dataYearMonth(y: Int, m: Int) = observations.filter(t => t.time.getYear == y && t.time.getMonthOfYear == m && t.value >=0).toList

def writeToTxt(fileName : String, obs : List[A] = observations) = {
    val out_stream = new PrintStream(new java.io.FileOutputStream(fileName))
    obs.map(i => out_stream.print(i.toTxt() + "\n"))
    out_stream.close()
  }
}