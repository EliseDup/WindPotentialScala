package historicalData

import org.joda.time.DateTime
import org.joda.time.Days
import org.joda.time.Months
/**
 * Generic class to load and store historical data
 */

class Observation(val time: DateTime, val value: Double, val name: String) extends Serializable {
  override def toString() = "Observation of " + name + " on " + time + " : " + value
}

abstract class HistoricalData[A <: Observation](name: String) extends Serializable {
  val observations = createData
  def createData: List[A]

  val n = observations.size
  val times = observations.map(_.time)
  val values = observations.map(_.value)

  // Return a subset with the observations for a given year, or a given month of a year
  def dataYear(y: Int) = observations.filter(_.time.getYear == y).toList
  def dataYearMonth(y: Int, m: Int) = observations.filter(t => t.time.getYear == y && t.time.getMonthOfYear == m).toList

  def nDays = Days.daysBetween(observations(0).time, observations(n - 1).time).getDays() + 1
  def nMonths = Months.monthsBetween(observations(0).time, observations(n - 1).time).getMonths() + 1

  def sameDay(d1: DateTime, d2: DateTime): Boolean = d1.getYear == d2.getYear && d1.dayOfYear == d2.dayOfYear
  def sameMonth(d1: DateTime, d2: DateTime): Boolean = d1.getYear == d2.getYear && d1.monthOfYear == d2.monthOfYear
}