package calculation

import utils.PlotHelper
/**
 * 4 roughness classes :
 *  - 0 : z_0 = 0.0002 m (water areas, lakes)
 *  - 1 : z_0 = 0.03 m (open areas with few windbreaks)
 *  - 2 : z_0 = 0.1 m (farm land with windbreaks)
 *  - 3 : z_0 = 0.4 m (urban districts, forests, and farm land with mny windbreaks)
 */

abstract class WindTurbine(val ratedPower: Double, val diameter: Double,
    val hubHeight: Double, val z0: Double) {
  val powerCurve: List[(Int, Int)]
  def power(windSpeed: Double): Double = powerCurve.find(i => i._1 == Math.floor(windSpeed).toInt).getOrElse((0, 0))._2

  // Embodied Energy = E_fix + P_I t
  // E_fix = fix part for construction and deconstruction
  // P_I t = a part that increases with time, e.g. maintenance and fuel provisioning 
  val lifeTime = 20
  // 12.9 TJ [we expressed power in kW]
  val energyDemandConstruction = 12.9 * Math.pow(10, 9)
  val energyDemandMaintenance = 0.3 * Math.pow(10, 9)
  val energyDemandDecommissioning = 0 // Negligible ?

  def power(windSpeed: Double, h0: Double): Double = power(windExtrapolation(windSpeed, h0, z0))
  def windExtrapolation(v0: Double, h0: Double, z0: Double): Double = v0 * (Math.log(hubHeight / z0) / Math.log(h0 / z0))
  def plot = PlotHelper.plotXY((powerCurve.map(_._1.toDouble), powerCurve.map(_._2.toDouble), "Power Curve of " + ratedPower + "kW Turbine"))
}

class Enercon82_2000(hubHeight: Double) extends WindTurbine(2000, 82, hubHeight, 0.4) {
  val powerCurve = List(
    (0, 0), (1, 0), (2, 3), (3, 25), (4, 82), (5, 174),
    (6, 321), (7, 532), (8, 815), (9, 1180), (10, 1580),
    (11, 1810), (12, 1980), (13, 2050), (14, 2050), (15, 2050),
    (16, 2050), (17, 2050), (18, 2050), (19, 2050), (20, 2050),
    (21, 2050), (22, 2050), (23, 2050), (24, 2050), (25, 2050), (26, 0))

}