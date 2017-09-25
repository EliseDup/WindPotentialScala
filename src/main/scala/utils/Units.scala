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
  val dayMiddleMonth = List(15, 45, 74, 105, 135, 166, 196, 227, 258, 288, 319, 349)
  val dayInMonths = List(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
  val firstDayMonth = dayInMonths.scanLeft(0)(_ + _)
  def month_dayInMonth(n: Int) = {
    val month = firstDayMonth.indexWhere(i => n >= firstDayMonth(i) && n < firstDayMonth(i + 1))
    val dayInMonth = n - (0 until month - 1).map(dayInMonths(_)).sum
    (month, dayInMonth)
  }
}