package calculation

import historicalData.WindData
import utils.PlotHelper
import historicalData.MeteoData

class WindTurbineSpecifications(val ratedPower: Double,
    val cutInSpeed: Double, val ratedSpeed: Double, val cutOutSpeed: Double,
    val diameter: Double, val hubHeight : Double, val powerCoefficient : Double) {
  /**
   * Theoritical power captured by the rotor = 1/2 rho A C_p u^3
   * With : - rho : air density (1,2 kg/m^3)
   *        - A = pi R^2 : the surface covered by the blades
   *        - C_p : the power coefficient
   *        - u : the wind speed
   */
   val area = Math.PI * Math.pow(diameter / 2.0, 2); val rho = 1.2; val betzLimit = 16 / 27.0;
  def theoriticalPower(speed: Double): Double = powerCoefficient * 0.5 * area * rho * Math.pow(speed, 3) / 1000.0

  // Power at a given speed [kW]
  def power(speed : Double, height : Double, z0 : Double):Double = power(windExtrapolation(speed, height, hubHeight, z0))
  def power(speed : Double, height : Double):Double = power(windExtrapolation(speed, height, hubHeight, 0.6))
  def power(speed: Double): Double = {
    if (speed < cutInSpeed || speed > cutOutSpeed) 0.0
    else if (speed > ratedSpeed) ratedPower
    else Math.min(ratedPower, theoriticalPower(speed))
  }
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

  def windExtrapolation(v0: Double, h0: Double, h: Double, z0: Double): Double = {
    v0 * (Math.log(h / z0) / Math.log(h0 / z0))
  }
}
object WindTurbineSpecifications {
  // 2MW turbine specifications
  def TwoMW() = new WindTurbineSpecifications(2000, 3, 12, 22, 80, 80, 0.38)
}