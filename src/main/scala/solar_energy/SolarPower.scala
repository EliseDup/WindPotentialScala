package solar_energy

import utils._
import squants.radio._
import squants.space._

object SolarPower {

  import Trigo._; import DayMonth._;

  val solarConstant = WattsPerSquareMeter(1367);

  def inverseRelativeDistanceEarthSun(d: Int) = (1 + 0.033 * cos(Degrees(360 / 365.0 * d)))
  def incidentExtraterrestrialRadiation(d: Int) = solarConstant * inverseRelativeDistanceEarthSun(d)

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

  def hourlyExtraterrestrialRadiation(d: Int, h: Int, lat: Angle) = Math.max(0, cosZenith(d, h, lat)) * incidentExtraterrestrialRadiation(d)
  // Daily Average Radiation in W /m^2
  def dailyExtraterrestrialRadiation(d: Int, lat: Angle): Irradiance = {
    // For lat beyond +/- 66.55°
    // (tan δ – tan φ) ≥ 1 there is no sunset, i.e. 24 hours of daylight;
    // (tan δ – tan φ) ≤ 1 there is no sunrise, i.e. 24 hours of darkness.
    if (sunsetHourAngle(d, lat).value.isNaN()) WattsPerSquareMeter(0)
    else 1.0 / Math.PI * incidentExtraterrestrialRadiation(d) * (cos(lat) * cos(solarDeclination(d)) * sin(sunsetHourAngle(d, lat)) + sunsetHourAngle(d, lat).toRadians * sin(lat) * sin(solarDeclination(d)))
  }

  def monthlyExtraterrestrialRadiation(m: Int, lat: Angle) = dailyExtraterrestrialRadiation(dayMiddleMonth(m), lat)
  def yearlyExtraterrestrialRadiation(lat: Angle) = 1.0 / 12 * (0 until 12).map(monthlyExtraterrestrialRadiation(_, lat)).foldLeft(WattsPerSquareMeter(0))(_ + _)

  // Erbs, 1982 : the ratio from global to diffuse radiation
  def diffuseFraction(kt: Double): Double = {
    if (kt <= 0.22) 1 - 0.09 * kt
    else if (kt < 0.8) 0.9511 - 0.1604 * kt + 4.39 * Math.pow(kt, 2) - 16.64 * Math.pow(kt, 3) + 12.34 * Math.pow(kt, 4)
    else 0.165
  }

  // The daily diffuse irradiation (Hd) isdefined by the Erbs correlations [39]: 
  // the daily total diffuse fraction depends on the sunset hour angle (ws) and is defined as:
  def dailyDiffuseFraction(kt: Double, d: Int, lat: Angle) = {
    val ws = sunsetHourAngle(d, lat)
    if (ws.toDegrees <= 81.4) {
      if (kt < 0.715) 1 - 0.2727 * kt + 2.4495 * Math.pow(kt, 2) - 11.951 * Math.pow(kt, 3) + 9.3879 * Math.pow(kt, 4)
      else 0.143
    } else {
      if (kt < 0.715) 1 + 0.2832 * kt - 2.557 * Math.pow(kt, 2) + 0.8448 * Math.pow(kt, 3)
      else 0.175
    }
  }

  def ratioHourlyToDailyDiffuseRadiation(d: Int, h: Int, lat: Angle) = {
    val ws = sunsetHourAngle(d, lat); val w = hourAngle(d, h)
    (Math.PI / 24.0) * ((cos(w) - cos(ws)) / (sin(ws) - ws.toRadians * cos(ws)))
  }
  def ratioHourlyToDailyGlobalRadiation(d: Int, h: Int, lat: Angle) = {
    val ws = sunsetHourAngle(d, lat); val w = hourAngle(d, h);
    val a = 0.409 + 0.5016 * sin(Radians(ws.toRadians - 1.047)); val b = 0.6609 - 0.4767 * sin(Radians(ws.toRadians - 1.047));
    Math.max(0, (Math.PI / 24.0) * (a + b * cos(w)) * ((cos(w) - cos(ws)) / (sin(ws) - ws.toRadians * cos(ws))))
  }

  /**
   * Value for a given, latitude, irradiance (mean by year) or irradiance_month (mean by month), month index = 0 to 11
   */
  def yearlyClearnessIndex(latitude: Angle, irradiance: Irradiance) = {
    if (Math.abs(latitude.toDegrees) >= 65 || yearlyExtraterrestrialRadiation(latitude).value == 0) 0.0
    else irradiance / yearlyExtraterrestrialRadiation(latitude)
  }
  def monthlyClearnessIndex(latitude: Angle, irradiance_month: Irradiance, month: Int) = {
    if (Math.abs(latitude.toDegrees) >= 65 || monthlyExtraterrestrialRadiation(month, latitude).value == 0) 0.0
    else irradiance_month / monthlyExtraterrestrialRadiation(month, latitude)
  }
  // Daily Clearness Index 
  val kmin = 0.05; def kmax(latitude: Angle, irradiance_month: Irradiance, month: Int) = {
    val kav = monthlyClearnessIndex(latitude, irradiance_month, month)
    0.613 + 0.267 * kav - 11.9 * Math.pow(kav - 0.75, 8)
  }
  def epsilon(latitude: Angle, irradiance_month: Irradiance, month: Int) = {
    val k = kmax(latitude, irradiance_month, month)
    (k - kmin) / (k - monthlyClearnessIndex(latitude, irradiance_month, month))
  }
  def sigma(latitude: Angle, irradiance_month: Irradiance, month: Int) = {
    val eps = epsilon(latitude, irradiance_month, month)
    -1.498 + (1.184 * eps - 27.182 * Math.exp(-1.5 * eps)) / (kmax(latitude, irradiance_month, month) - kmin)
  }
  def dailyClearnessIndex(latitude: Angle, irradiance_month: Irradiance, n: Int) = {
    val (m, ndm, ndk) = month_dayInMonth(n)
    val alpha = (ndk - 0.5) / ndm
    val sig = sigma(latitude, irradiance_month, m)
    (1 / sig) * (Math.log((1 - alpha) * Math.exp(sig * kmin) + alpha * (Math.exp(sig * kmax(latitude, irradiance_month, m)))))
  }

  def directIrradiance(latitude: Angle, irradiance: Irradiance) = irradiance * (1 - diffuseFraction(yearlyClearnessIndex(latitude, irradiance)))
  def monthlyDirectIrradiance(latitude: Angle, irradiance_month: Irradiance, month: Int) = irradiance_month * (1 - diffuseFraction(monthlyClearnessIndex(latitude, irradiance_month, month)))

  // Daily global, direct and diffuse radiation is computed base on the estimated daily clearness index and diffuse to global ratio
  def dailyRadiation(latitude: Angle, irradiance_month: Irradiance, n: Int) = dailyClearnessIndex(latitude, irradiance_month, n) * dailyExtraterrestrialRadiation(n, latitude)
  def dailyDirectRadiation(latitude: Angle, irradiance_month: Irradiance, n: Int) = (1 - dailyDiffuseFraction(dailyClearnessIndex(latitude, irradiance_month, n), n, latitude)) * dailyRadiation(latitude, irradiance_month, n)
  def dailyDiffuseRadiation(latitude: Angle, irradiance_month: Irradiance, n: Int) = dailyDiffuseFraction(dailyClearnessIndex(latitude, irradiance_month, n), n, latitude) * dailyRadiation(latitude, irradiance_month, n)

  // Ratio of the global daily (kWh/m^2/day) for a day. So to get a value in W/m^2 = ratio * 24
  def hourlyRadiation(latitude: Angle, irradiance_month: Irradiance, n: Int, h: Int) = dailyRadiation(latitude, irradiance_month, n) * ratioHourlyToDailyGlobalRadiation(n, h, latitude) * 24
  def hourlyDiffuseRadiation(latitude: Angle, irradiance_month: Irradiance, n: Int, h: Int) = dailyDiffuseRadiation(latitude, irradiance_month, n) * ratioHourlyToDailyDiffuseRadiation(n, h, latitude) * 24
  def hourlyDirectRadiation(latitude: Angle, irradiance_month: Irradiance, n: Int, h: Int) = hourlyRadiation(latitude, irradiance_month, n, h) - hourlyDiffuseRadiation(latitude, irradiance_month, n, h)

}