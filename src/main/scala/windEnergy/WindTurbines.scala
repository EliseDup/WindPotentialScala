package windEnergy

import utils.PlotHelper
import construction._
import squants.motion._
import squants.energy._
import squants.space._
import squants.mass._
import squants.SquantifiedDouble
import windEnergy.WindTurbineComponents

/**
 * Extrapolation to calculate the wind speed at height H from wind speed v0
 * measured at a given height h0
 *
 * V = V_0 (ln(H/z_0) / ln (h_0/z_0))
 *
 * with z_0 = roughness length (metres)
 * 0.0002	= Water surface
 * 0.0024	= Completely open ground with a smooth surface, e.g. concrete runways at the airports, mowed grassland, etc.
 * 0.03 =	Open farming areas fitted with no fences and hedgerows and very scattered buildings. Only softly rounded hills.
 * 0.055 =	Farming land dotted with some houses and 8 m tall sheltering hedgerows within a distance of some 1,250 metres.
 * 0.1 = Farming land dotted with some houses and 8 m tall sheltering hedgerows within a distance of some 500 metres.
 * 0.2	= Farming land dotted with many houses, shrubs and plants, or with 8 m tall sheltering hedgerows of some 250 metres.
 * 0.4	= Villages, hamlets and small towns, farming land with many or tall sheltering hedgerows, forest areas and very rough and uneven terrain
 * 0.8	= Large cities dotted with high rise buildings.
 * 1.6	= Very large cities dotted with high rise buildings and skyscrapers.
 *
 * This is the logarithmic wind profile for neutral conditions, in which thermal effects have been discarded
 */

/*
 * N.B.: Power coefficient should be evaluated in order to have a curve close to the power curve when it is not available,
 * the maximum being the Betz Limit = 16/27 = 0.59
 */
class WindTurbine(val specs: WindTurbineComponents, val powerCoefficient: Double, val nPerSquareKM: Double) {
  
  val ratedPower = specs.ratedPower
  // Embodied Energy = E_fix + P_I t
  // E_fix = fix part for construction and deconstruction
  // P_I t = a part that increases with time, e.g. maintenance and fuel provisioning 
  val lifeTime = 20; val radius = specs.diameter / 2
  val area = Math.PI * radius * radius; val airDensity = KilogramsPerCubicMeter(1.225)

  // We approximate the power by the Cubic power curve
  def theoriticalPowerInWind(speed: Velocity) = Watts(0.5 * airDensity.toKilogramsPerCubicMeter * area.toSquareMeters * Math.pow(speed.toMetersPerSecond, 3))

  def power(windSpeed: Velocity, h0: Length, z0: Length): Power = power(windExtrapolation(windSpeed, h0, z0))
  def windExtrapolation(v0: Velocity, h0: Length, z0: Length): Velocity = v0 * (Math.log(specs.hubHeight / z0) / Math.log(h0 / z0))

  def power(speed: Velocity) = {
    if (speed < specs.cutInSpeed || speed > specs.cutOutSpeed) Watts(0.0)
    else if (speed > specs.ratedSpeed) specs.ratedPower
    else Watts(math.min(specs.ratedPower.toWatts, powerCoefficient * theoriticalPowerInWind(speed).toWatts))
  }
  
  def plot = {
    val s = (0 until specs.cutOutSpeed.toMetersPerSecond.toInt + 5).toList
    val p = s.map(i => power(MetersPerSecond(i)).toKilowatts)
    PlotHelper.plotXY(List((s.map(_.toDouble), p, "")), xLabel = "Wind Speed [m/s]", yLabel = "Power [kW]", title = "Power of " + ratedPower + " Turbine")
  }
  override def toString() = specs.toString()
}

/**
 * When the power curve is available, use this
 */
trait PowerCurve {
  val curve: List[(Int, Double)]
  val ratedPower: Power
  def powerCurve = curve.map(i => (i._1, Kilowatts(i._2)))
  def powerFromCurve(windSpeed: Velocity): Power = powerCurve.find(i => i._1 == Math.floor(windSpeed.value).toInt).getOrElse((0, Kilowatts(0)))._2
  def plotCurve = PlotHelper.plotXY(List((powerCurve.map(_._1.toDouble), powerCurve.map(_._2.value), "")), xLabel = "Wind Speed [m/s]", yLabel = "Power [kW]", title = "Power curve of " + ratedPower + " Turbine")
}

class WindTurbine2MW extends WindTurbine(new WindTurbineComponents("2MW"), 0.38, 4) with PowerCurve {
  val curve = List(
    (0, 0.0), (1, 0.0), (2, 0.0), (3, 0.0), (4, 66.3), (5, 152.0),
    (6, 280.0), (7, 457.0), (8, 690.0), (9, 978.0), (10, 1296.0),
    (11, 1598.0), (12, 1818.0), (13, 1935.0), (14, 1980.0), (15, 1995.0),
    (16, 1999.0), (17, 2000.0), (18, 2000.0), (19, 2000.0), (20, 2000.0),
    (21, 2000.0), (22, 2000.0), (23, 2000.0), (24, 2000.0), (25, 2000.0), (26, 0.0))
  // Use the power curve !!
  override def power(windSpeed: Velocity) = powerFromCurve(windSpeed)
}

// ONSHORE
class WindTurbineWithPower(power : String) extends WindTurbine(new WindTurbineComponents(power), 0.25, 4)

