package calculation

import utils.PlotHelper
import embodiedEnergy._
import squants.motion._
import squants.energy._
import squants.space._
import squants.mass._
/**
 * 4 roughness classes :
 *  - 0 : z_0 = 0.0002 m (water areas, lakes)
 *  - 1 : z_0 = 0.03 m (open areas with few windbreaks)
 *  - 2 : z_0 = 0.1 m (farm land with windbreaks)
 *  - 3 : z_0 = 0.4 m (urban districts, forests, and farm land with mny windbreaks)
 */

abstract class WindTurbine(val diameter: Length, val hubHeight: Length,
    val cutInSpeed: Velocity, val ratedSpeed: Velocity, val cutOutSpeed: Velocity, val powerCoefficient: Double) {

  val components: WindTurbineComponents

  // Embodied Energy = E_fix + P_I t
  // E_fix = fix part for construction and deconstruction
  // P_I t = a part that increases with time, e.g. maintenance and fuel provisioning 
  val lifeTime = 20; val radius = diameter / 2
  val area = Math.PI * radius * radius; val airDensity = KilogramsPerCubicMeter(1.225)
  def theoriticalPowerInWind(speed: Velocity) = Watts(0.5 * airDensity.toKilogramsPerCubicMeter * area.toSquareMeters * Math.pow(speed.toMetersPerSecond, 3))

  def power(windSpeed: Velocity, h0: Length, z0: Length): Power = power(windExtrapolation(windSpeed, h0, z0))
  def windExtrapolation(v0: Velocity, h0: Length, z0: Length): Velocity = v0 * (Math.log(hubHeight / z0) / Math.log(h0 / z0))

  def power(speed: Velocity) = {
    if (speed < cutInSpeed || speed > cutOutSpeed) Watts(0.0)
    else if (speed > ratedSpeed) components.ratedPower
    else Watts(math.min(components.ratedPower.toWatts, powerCoefficient*theoriticalPowerInWind(speed).toWatts))
  }
  
  def plot = {
    val s = (0 until cutOutSpeed.toMetersPerSecond.toInt+5).toList
    val p = s.map(i => power(MetersPerSecond(i)).toKilowatts)
    PlotHelper.plotXY(List((s.map(_.toDouble),p,"")),xLabel="Wind Speed [m/s]", yLabel="Power [kW]",title= "Power of " + components.ratedPower + " Turbine")
  }
}

class WindTurbine2MW extends WindTurbine(Meters(82), Meters(98),
  MetersPerSecond(3), MetersPerSecond(12), MetersPerSecond(22), 0.38){// with PowerCurve {
  val components = new WindTurbine2MWComponents
  val specs = List(
    (0, 0), (1, 0), (2, 3), (3, 25), (4, 82), (5, 174),
    (6, 321), (7, 532), (8, 815), (9, 1180), (10, 1580),
    (11, 1810), (12, 1980), (13, 2050), (14, 2050), (15, 2050),
    (16, 2050), (17, 2050), (18, 2050), (19, 2050), (20, 2050),
    (21, 2050), (22, 2050), (23, 2050), (24, 2050), (25, 2050), (26, 0))
  val ratedPower = components.ratedPower
  // Use the power curve !!
  //override def power(windSpeed: Velocity) = powerFromCurve(windSpeed)
}

class WindTurbine3MW extends WindTurbine(Meters(90), Meters(80),
  MetersPerSecond(4), MetersPerSecond(16), MetersPerSecond(25), 0.25) {
  val components = new WindTurbine3MWComponents
}

class WindTurbine850kW extends WindTurbine(Meters(52), Meters(60),
  MetersPerSecond(4), MetersPerSecond(16), MetersPerSecond(25), 0.25) {
  val components = new WindTurbine850kWComponents
}

trait PowerCurve {
  val specs: List[(Int, Int)]
  val ratedPower: Power
  def powerCurve = specs.map(i => (i._1, Kilowatts(i._2)))
  def powerFromCurve(windSpeed: Velocity): Power = powerCurve.find(i => i._1 == Math.floor(windSpeed.value).toInt).getOrElse((0, Kilowatts(0)))._2
  def plotCurve = PlotHelper.plotXY((powerCurve.map(_._1.toDouble), powerCurve.map(_._2.value), "Power Curve of " + ratedPower + " Turbine"))
}