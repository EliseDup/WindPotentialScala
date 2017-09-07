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

object Thermodynamics {

  // WIND
  // Pressure = 1bar * (1 - 0.0065 z / T)^5.255
  def pressure(elevation: Length) = Pascals(101325) * Math.pow(1 - 0.0065 * elevation.toMeters / 288.15, 5.255)
  // density = P/RT
  def airDensity(elevation: Length) = KilogramsPerCubicMeter(pressure(elevation).toPascals / (287.05 * 288.15))
  // Power Density = 1/2 pho v^3
  def windPowerDensity(windSpeed: Velocity, elevation: Length) = WattsPerSquareMeter(0.5 * airDensity(elevation).toKilogramsPerCubicMeter * Math.pow(windSpeed.toMetersPerSecond, 3))

  def theoreticalWindPowerDensity(cell: GridCell, wind: WindProfile) = WattsPerSquareMeter(0.5 * airDensity(wind.height + cell.altitude).toKilogramsPerCubicMeter * Math.pow(wind.c.toMetersPerSecond, 3) * Gamma.gamma(1 + 3.0 / wind.k))

  def sin(angle: Angle) = Math.sin(angle.toRadians)
  def cos(angle: Angle) = Math.cos(angle.toRadians)
  def tan(angle: Angle) = Math.tan(angle.toRadians)
  def acos(d: Double) = Radians(Math.acos(d))
  def asin(d: Double) = Radians(Math.asin(d))
  def atan(d: Double) = Radians(Math.atan(d))

  // SOLAR
  val solarConstant = WattsPerSquareMeter(1353);
  // val albedo = 0.35
  
  def inverseRelativeDistanceEarthSun(d: Int) =  (1 + 0.033 * cos(Degrees(360 / 365.0 * d)))
  def incidentRadiation(d: Int) = solarConstant * inverseRelativeDistanceEarthSun(d)
  
  // Declination is the angle made between the plane of the equator and the line joining the two centres of the earth and the sun
  def solarDeclination(d: Int) = Degrees(23.45) * sin(Degrees(360.0 * (284 + d) / 365.0))
  // The sunset hour angle,when the incidence angle is 90°
  def sunsetHourAngle(d : Int, lat : Angle) = acos(-tan(lat)*tan(solarDeclination(d)))
  
  def hourAngle(d: Int, h: Int) = Radians(2 * Math.PI * (12.0 - h) / 24.0 /*- (latitude - latitude_zone)*/ - TEQ(d)) //Degrees(15) * (hourOfDay - 12)
  // The correction TEQ (“equationof time”) accounts for the variations in solar time caused by changes in the rotational and orbital motion of the Earth
  def TEQ(d: Int) = {
    if (d <= 20) -0.0113 - 0.0019 * d
    else if (d <= 135) -0.0227 - 0.393 * Math.cos(0.0357 * (d - 43))
    else if (d <= 240) -0.0061 + 0.0218 * Math.cos(0.0449 * (d - 135))
    else if (d <= 335) 0.0275 + 0.0436 * Math.cos(0.036 * (d - 306))
    else -0.002 * (d - 359)
  }

  def cosZenith(d: Int, h: Int, lat: Angle) = sin(solarDeclination(d)) * sin(lat) + cos(solarDeclination(d)) * cos(lat) * cos(hourAngle(d, h))
  def zenith(d: Int, h: Int, lat: Angle) = acos(cosZenith(d, h, lat))

  def radiation(d: Int, h: Int, lat: Angle) = Math.max(0, cosZenith(d, h, lat)) * incidentRadiation(d)
  def dailyRadiation(d: Int, lat: Angle) = 1.0 / 24 * (0 until 24).map(radiation(d, _, lat)).foldLeft(WattsPerSquareMeter(0))(_ + _)
  val dayInMonth = List(31,28,31,30,31,30,31,31,30,31,30,31)
  val firstDayMonth = dayInMonth.scanLeft(0)(_ + _)
  def monthlyRadiation(m: Int, lat: Angle) = {
    1.0 / dayInMonth(m) * (0 until dayInMonth(m)).map(i => dailyRadiation(firstDayMonth(m) + i, lat)).foldLeft(WattsPerSquareMeter(0))(_ + _)
  }
 
  def yearlyRadiation(lat: Angle) = 1.0 / 365 * (0 until 365).map(dailyRadiation(_, lat)).foldLeft(WattsPerSquareMeter(0))(_ + _)

  // The daily diffuse irradiation (Hd) isdefined by the Erbs correlations [39]: 
  // the daily total diffuse fraction depends on the sunset hour angle (ws) and is defined as:
  def dailyDiffuseFraction(clearnessIndex: Double, d: Int, lat : Angle) = {
    val angle = sunsetHourAngle(d, lat)
    if (angle.toDegrees <= 81.4) {
      if (clearnessIndex < 0.715) 1 - 0.2727 * clearnessIndex + 2.4495 * Math.pow(clearnessIndex, 2) + 9.3879 * Math.pow(clearnessIndex, 4)
      else 0.143
    } else {
      if (clearnessIndex < 0.715) 1 + 0.2832 * clearnessIndex - 2.557 * Math.pow(clearnessIndex, 2) + 0.8448 * Math.pow(clearnessIndex, 3)
      else 0.175
    }
  }
}