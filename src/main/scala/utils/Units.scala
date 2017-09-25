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
import org.apache.commons.math3.special.Gamma
import squants.time.Time
import squants.time.Hours
import org.joda.time.Days
import org.jfree.data.time.Month
import squants.time.Seconds

object WindPower {
  // Pressure = 1bar * (1 - 0.0065 z / T)^5.255
  def pressure(elevation: Length) = Pascals(101325) * Math.pow(1 - 0.0065 * elevation.toMeters / 288.15, 5.255)
  // density = P/RT
  def airDensity(elevation: Length) = KilogramsPerCubicMeter(pressure(elevation).toPascals / (287.05 * 288.15))
  // Power Density = 1/2 pho v^3
  def windPowerDensity(windSpeed: Velocity, elevation: Length) = WattsPerSquareMeter(0.5 * airDensity(elevation).toKilogramsPerCubicMeter * Math.pow(windSpeed.toMetersPerSecond, 3))
  def theoreticalWindPowerDensity(cell: GridCell, wind: WindProfile) = WattsPerSquareMeter(0.5 * airDensity(wind.height + cell.altitude).toKilogramsPerCubicMeter * Math.pow(wind.c.toMetersPerSecond, 3) * Gamma.gamma(1 + 3.0 / wind.k))
}

object SolarPower {
  import Trigo._; import DayMonth._;

  val solarConstant = WattsPerSquareMeter(1367);

  def inverseRelativeDistanceEarthSun(d: Int) = (1 + 0.033 * cos(Degrees(360 / 365.0 * d)))
  def incidentRadiation(d: Int) = solarConstant * inverseRelativeDistanceEarthSun(d)

  // Declination is the angle made between the plane of the equator and the line joining the two centres of the earth and the sun
  def solarDeclination(d: Int) = Degrees(23.45) * sin(Degrees(360.0 * (284 + d) / 365.0))
  // The sunset hour angle,when the incidence angle is 90°
  def sunsetHourAngle(d: Int, lat: Angle) = acos(-tan(lat) * tan(solarDeclination(d)))
  def dayLength(d: Int, lat: Angle) = 2 / 15 * sunsetHourAngle(d, lat)

  def hourAngle(d: Int, h: Int) = Radians(2 * Math.PI * (12.0 - h) / 24.0 /*- (latitude - latitude_zone)*/ - TEQ(d)) //Degrees(15) * (hourOfDay - 12)
  // The correction TEQ (“equation of time”) accounts for the variations in solar time caused by changes in the rotational and orbital motion of the Earth
  def TEQ(d: Int) = {
    if (d <= 20) -0.0113 - 0.0019 * d
    else if (d <= 135) -0.0227 - 0.393 * Math.cos(0.0357 * (d - 43))
    else if (d <= 240) -0.0061 + 0.0218 * Math.cos(0.0449 * (d - 135))
    else if (d <= 335) 0.0275 + 0.0436 * Math.cos(0.036 * (d - 306))
    else -0.002 * (d - 359)
  }

  def cosZenith(d: Int, h: Int, lat: Angle) = sin(solarDeclination(d)) * sin(lat) + cos(solarDeclination(d)) * cos(lat) * cos(hourAngle(d, h))
  def zenith(d: Int, h: Int, lat: Angle) = acos(cosZenith(d, h, lat))

  def hourlyRadiation(d: Int, h: Int, lat: Angle) = Math.max(0, cosZenith(d, h, lat)) * incidentRadiation(d)
  // Daily Average Radiation in W /m^2
  def dailyRadiation(d: Int, lat: Angle): Irradiance = {
    // For lat beyond +/- 66.55°
    // (tan δ – tan φ) ≥ 1 there is no sunset, i.e. 24 hours of daylight;
    // (tan δ – tan φ) ≤ 1 there is no sunrise, i.e. 24 hours of darkness.
    if (sunsetHourAngle(d, lat).value.isNaN()) WattsPerSquareMeter(0)
    else 1.0 / Math.PI * incidentRadiation(d) * (cos(lat) * cos(solarDeclination(d)) * sin(sunsetHourAngle(d, lat)) + sunsetHourAngle(d, lat).toRadians * sin(lat) * sin(solarDeclination(d)))
  }
  def monthlyRadiation(m: Int, lat: Angle) = dailyRadiation(dayMiddleMonth(m), lat)
  def yearlyRadiation(lat: Angle) = 1.0 / 12 * (0 until 12).map(monthlyRadiation(_, lat)).foldLeft(WattsPerSquareMeter(0))(_ + _)

  // The daily diffuse irradiation (Hd) isdefined by the Erbs correlations [39]: 
  // the daily total diffuse fraction depends on the sunset hour angle (ws) and is defined as:
  def dailyDiffuseFraction(kt: Double, d: Int, lat: Angle) = {
    val angle = sunsetHourAngle(d, lat)
    if (angle.toDegrees <= 81.4) {
      if (kt < 0.715) 1 - 0.2727 * kt + 2.4495 * Math.pow(kt, 2) + 9.3879 * Math.pow(kt, 4)
      else 0.143
    } else {
      if (kt < 0.715) 1 + 0.2832 * kt - 2.557 * Math.pow(kt, 2) + 0.8448 * Math.pow(kt, 3)
      else 0.175
    }
  }
  // Erbs, 1982 
  def diffuseFraction(kt: Double): Double = {
    if (kt <= 0.22) 1 - 0.09 * kt
    else if (kt < 0.8) 0.9511 - 0.1604 * kt + 4.39 * Math.pow(kt, 2) - 16.64 * Math.pow(kt, 3) + 12.34 * Math.pow(kt, 4)
    else 0.165
  }
}

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