package utils

import squants.energy._
import squants.MetricSystem
import squants.mass._
import squants.space._
import squants.thermal._
import squants.motion._
import squants.radio._
import grid.GridCell
import wind_energy.WindProfile

import squants.time.Time
import squants.time.Hours
import org.joda.time.Days
import org.jfree.data.time.Month
import squants.time.Seconds

object TerawattHours extends EnergyUnit {
  val conversionFactor = Watts.conversionFactor * MetricSystem.Tera
  val symbol = "TWh"
}
object PetawattHours extends EnergyUnit {
  val conversionFactor = Watts.conversionFactor * MetricSystem.Peta
  val symbol = "PWh"
}
object TonOilEquivalent extends EnergyUnit {
  val conversionFactor = 11.63 * Watts.conversionFactor * MetricSystem.Mega
  val symbol = "toe"
}
object MegaTonOilEquivalent extends EnergyUnit {
  val conversionFactor = TonOilEquivalent.conversionFactor * MetricSystem.Mega
  val symbol = "Mtoe"
}
object Exajoules extends EnergyUnit {
  val conversionFactor = Joules.conversionFactor * MetricSystem.Exa
  val symbol = "EJ"
}

object Petajoules extends EnergyUnit {
  val conversionFactor = Joules.conversionFactor * MetricSystem.Peta
  val symbol = "PJ"
}
object Terawatts extends PowerUnit {
  val conversionFactor = MetricSystem.Tera
  val symbol = "TW"
}
// Gray is J/kg
object GigajoulesPerton extends SpecificEnergyUnit {
  val conversionFactor = MetricSystem.Giga / MetricSystem.Kilo
  val symbol = "GJ/t"
}
object GigaHectares extends AreaUnit {
  val symbol = "Gha"
  val conversionFactor = 10000d * MetricSystem.Giga
}

object Trigo {
  def sin(angle: Angle) = Math.sin(angle.toRadians)
  def cos(angle: Angle) = Math.cos(angle.toRadians)
  def tan(angle: Angle) = Math.tan(angle.toRadians)
  def acos(d: Double) = Radians(Math.acos(d))
  def asin(d: Double) = Radians(Math.asin(d))
  def atan(d: Double) = Radians(Math.atan(d))
}
object DayMonth {
  val dayMiddleMonth = List(16, 45, 75, 105, 136, 166, 197, 228, 258, 289, 319, 350)
  val dayInMonths = List(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
  val firstDayMonth = dayInMonths.scanLeft(0)(_ + _)
  // From n the day in the year (1 to 365) (!! not starting with 0 !!), returns the index of the month (0 to 11), the # of days in the month, and
  // the index of the specific day in the given month (1 to 31) (!! not starting with 0 !!)
  def month_dayInMonth(n: Int) = {
    assert(n <= 365)
    val month = if(n >= 335) 11 else (0 to 11).indexWhere(i => n >= firstDayMonth(i) && n < firstDayMonth(i + 1))
    val dayInMonth = n - (0 until month).map(dayInMonths(_)).sum + 1
    (month, dayInMonths(month), dayInMonth)
  }

  def test {
    val n2 = month_dayInMonth(2) // 3 janvier
    assert(n2._1 == 0 && n2._2 == 31 && n2._3 == 3)
    val n70 = month_dayInMonth(70) // 12 mars
    assert(n70._1 == 2 && n70._2 == 31 && n70._3 == 12)
    val n150 = month_dayInMonth(150) // 31 mai
    assert(n150._1 == 4 && n150._2 == 31 && n150._3 == 31)
    val n250 = month_dayInMonth(250) // 8 septembre
    assert(n250._1 == 8 && n250._2 == 30 && n250._3 == 8)
    val n364 = month_dayInMonth(364) // 31 décembre
    assert(n364._1 == 11 && n364._2 == 31 && n364._3 == 31)
  }
}
