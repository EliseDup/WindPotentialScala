package calculation

import historicalData.WindData
import utils.PlotHelper
import historicalData.MeteoData

class WindTurbine(val ratedPower : Double, val radius : Double){
 
  val yieldTurbine = 0.38
  val area = Math.PI*Math.pow(radius/2.0,2)
  
  val cutInSpeed = 3
  val ratedSpeed = 12
  val cutOutSpeed = 22
  // Power at a given speed [kW]
  def power(speed : Double) : Double = {
    if(speed < cutInSpeed || speed > cutOutSpeed) 0.0
    else if (speed > ratedSpeed) ratedPower
    else maxPower(speed)*yieldTurbine
  }
  
  /**
   * Maximum power = 1/2 A rho v^3 => in kW!
   * rho = 1.2 kg/m^3
   * A = pi r^2
   * maximum yield given by betz limit
   */
  val rho = 1.2; val betzLimit = 16/27.0
  def maxPower(speed : Double) : Double = (0.5*area*rho*Math.pow(speed,3))/1000.0
  
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
  
  def windExtrapolation(v0 : Double, h0 : Double, h : Double, z0 : Double) : Double = {
    v0*(Math.log(h/z0)/Math.log(h0/z0))
  }
}