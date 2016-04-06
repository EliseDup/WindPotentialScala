package windEnergy

import org.apache.commons.math3.distribution.WeibullDistribution
import utils.PlotHelper
import squants.motion.MetersPerSecond
import squants.energy.Kilowatts
import org.apache.commons.math3.special.Gamma
import org.apache.commons.math3.distribution.GammaDistribution

class Weibull(meanspeed: Double, k: Double) {
  val wb = new WeibullDistribution(k, meanspeed)
  val x = wb.sample(1000000).toList
  def plot = PlotHelper.histogram(x, 100, "Weibull distribution scale :" + meanspeed + ", shape:" + k)

  def capacityFactor(turbine: WindTurbine): Double = x.map(v => turbine.power(MetersPerSecond(v))).foldLeft(Kilowatts(0))(_ + _) / x.size / turbine.ratedPower
}

/**
 * Comparative analysis on power curve models of wind turbine generator in estimating capacity factor
 * 
 * Tian-Pau Chang, Feng-Jiao Liu, Hong-Hsi Ko, Shih-Ping Cheng, Li-Chung Sun, Shye-Chorng Kuo
 */
object CapacityFactorCalculation {

  val vc = 4.0
  // Rated speed vr can be optimized given the Weibull parameter of the wind speed in a location
  // val vr = 15.0
  val vf = 25.0

  def output(speed: Double, k: Double, vr: Double) = {
    if (speed < vc) 0.0
    else if (speed <= vr) (Math.pow(speed, k) - Math.pow(vc, k)) / (Math.pow(vr, k) - Math.pow(vc, k))
    else if (speed <= vf) 1.0
    else 0.0
  }
  def scaleParameter(speed: Double, k: Double) = speed / Gamma.gamma(1 + 1.0 / k)

  // v is the mean wind speed, not the scale parameter !
  def apply(k: Double, v: Double, vr: Double): Double = {
    val c = scaleParameter(v, k)
    (Math.exp(-Math.pow(vc / c, k)) - Math.exp(-Math.pow(vr / c, k))) / (Math.pow(vr / c, k) - Math.pow(vc / c, k)) - Math.exp(-Math.pow(vf / c, k))
  }
  def apply(k: Double, v: Double): Double = Math.max(apply(k, v, 12), apply(k, v, 15))

  def linear(k: Double, v: Double, vr: Double): Double = {
    val c = scaleParameter(v, k)
    c / (vr - vc) * (Math.exp(-vc / c) - Math.exp(-vr / c)) - Math.exp(-vf / c)
  }
  def linear(k: Double, c: Double): Double = Math.max(linear(k, c, 12), linear(k, c, 15))

}

object LoadHourCalculation {
  def main(args: Array[String]): Unit = {
    val turbine2MW = new WindTurbineWithPower("5MWoffshore")
    turbine2MW.plot
    val k = List(1.5, 2.0, 3.0)
    val v = (4 until 12).map(i => (0 until 10).map(_ * 0.1 + i)).flatten.toList
    val euEstimation = v.map(i => Math.max(0, Math.min(5500, i * 626.51 - 1901))).toList
    val hoogrijkEstimation = v.map(i => Math.max(0, Math.min(4000, i * 565 - 1745))).toList
    val list = k.map(i => {
      println(i)
      (v, v.map(v => new Weibull(v, i).capacityFactor(turbine2MW) * 365 * 24).toList, "K factor = " + i)
    })
    PlotHelper.plotXY(list.toList :+ (v, euEstimation, "EEA 2009") :+ (v, hoogrijkEstimation, "Hoogwijk 2004"), xLabel = "Mean Wind Speed [m/s]", yLabel = "Load Hours", legend = true)
    val error = (0 until v.size).map(i => Math.abs(euEstimation(i) - list(0)._2(i)) / list(0)._2(i)).sum / v.size
    println("Mean Error :" + error)

  }
}