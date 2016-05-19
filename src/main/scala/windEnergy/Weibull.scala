package windEnergy

import org.apache.commons.math3.distribution.WeibullDistribution
import utils.PlotHelper
import squants.motion._
import squants.space._
import squants.energy._
import org.apache.commons.math3.special.Gamma

/**
 * Weibull distribution at 10 m height
 * 
 * Justus and Mikhail
 */
object Weibull {
  def shapeParameter(meanSpeed : Velocity, std : Double) = Math.pow(std / meanSpeed.toMetersPerSecond, -1.086)
  def scaleParameter(meanSpeed: Velocity, k: Double) = meanSpeed / Gamma.gamma(1 + 1.0 / k)
  def meanSpeed(c : Velocity, k : Double) = c * Gamma.gamma(1 + 1.0 / k)
}

case class WeibullParameters(mean : Velocity, std : Double, hHub : Length){
  val k = Weibull.shapeParameter(mean, std)
  val c = Weibull.scaleParameter(mean, k)
  
  // Extrapolation
  val hr, h = Meters(10)
  val n = (0.37 - 0.0881*Math.log(c.toMetersPerSecond)) / (1 - 0.0881*Math.log(h/hr))
  
  val cHub = c*Math.pow(hHub/h, n)
  val kHub = k*(1- 0.0881*Math.log(h/hr)) / (1- 0.0881*Math.log(hHub/hr))
  
}

/**
 * Comparative analysis on power curve models of wind turbine generator in estimating capacity factor
 * 
 * Tian-Pau Chang, Feng-Jiao Liu, Hong-Hsi Ko, Shih-Ping Cheng, Li-Chung Sun, Shye-Chorng Kuo
 */

object CapacityFactorCalculation {

  val vc = 4.0
  // Rated speed vr can be optimized given the Weibull parameter of the wind speed in a location
  val vrs = (12 to 15).toList
  val vf = 25.0

  def powerCurve(k : Double, v : Double, vr: Double) = {
    if (v < vc) 0.0
    else if (v <= vr) (Math.pow(v, k) - Math.pow(vc, k)) / (Math.pow(vr, k) - Math.pow(vc, k))
    else if (v <= vf) 1.0
    else 0.0
  }
  def apply(w : WeibullParameters, vr: Double): Double = apply(w.cHub.toMetersPerSecond,w.kHub,vr)
  
  def apply(c : Double, k : Double, vr: Double): Double = {
    (Math.exp(-Math.pow(vc / c, k)) - Math.exp(-Math.pow(vr / c, k))) / (Math.pow(vr / c, k) - Math.pow(vc / c, k)) - Math.exp(-Math.pow(vf / c, k))
  }
  
 /* def apply(c : Double, k : Double) :Double= {
    val res = vrs.map(apply(c,k, _)).zipWithIndex.max
 //   println(c + "\t" + k + "\t" + vrs(res._2))
    res._1
  }*/
  def apply(w : WeibullParameters): Double = apply(w.cHub.toMetersPerSecond,w.kHub, 15.0)

  def linear(w : WeibullParameters, vr: Double): Double = {
    val c = w.cHub.toMetersPerSecond; val k = w.kHub
    c / (vr - vc) * (Math.exp(-vc / c) - Math.exp(-vr / c)) - Math.exp(-vf / c)
  }
  def linear(w : WeibullParameters): Double = vrs.map(linear(w, _)).max

  def main(args: Array[String]): Unit = {
    val v = (0 to 19).map(i => (0 until 10).map(_ * 0.1 + i)).flatten.toList
    val k = (1 to 5).toList
    val vr = (12 to 15).map(_.toDouble).toList
    
    val list = k.map(j => (v, v.map(i => CapacityFactorCalculation(Weibull.scaleParameter(MetersPerSecond(i), j).toMetersPerSecond,j,15.0)), "k="+j.toString))
    
   PlotHelper.plotXY(list, legend=true, xLabel = "Mean Wind Speed [m/s]", yLabel = "Capacity Factor")
   
  }  
}

/**
 * Actually calculate the capacity factor by simulating a large number of samples from the weibull distribution
 */
class WeibullSimulation(meanspeed: Double, k: Double) {
  val wb = new WeibullDistribution(k, meanspeed)
  val x = wb.sample(1000000).toList
  def plot = PlotHelper.histogram(x, 100, "Weibull distribution scale :" + meanspeed + ", shape:" + k)

  def capacityFactor(turbine: WindTurbine): Double = x.map(v => turbine.power(MetersPerSecond(v))).foldLeft(Kilowatts(0))(_ + _) / x.size / turbine.ratedPower

  // Small test to plot the simulation against linear approximation
  def main(args: Array[String]): Unit = {
    val turbine2MW = new WindTurbineWithPower("5MWoffshore")
    turbine2MW.plot
    val k = List(1.5, 2.0, 3.0)
    val v = (4 until 12).map(i => (0 until 10).map(_ * 0.1 + i)).flatten.toList
    val euEstimation = v.map(i => Math.max(0, Math.min(5500, i * 626.51 - 1901))).toList
    val hoogrijkEstimation = v.map(i => Math.max(0, Math.min(4000, i * 565 - 1745))).toList
    val list = k.map(i => {
      println(i)
      (v, v.map(v => new WeibullSimulation(v, i).capacityFactor(turbine2MW) * 365 * 24).toList, "K factor = " + i)
    })
    PlotHelper.plotXY(list.toList :+ (v, euEstimation, "EEA 2009") :+ (v, hoogrijkEstimation, "Hoogwijk 2004"), xLabel = "Mean Wind Speed [m/s]", yLabel = "Load Hours", legend = true)
    val error = (0 until v.size).map(i => Math.abs(euEstimation(i) - list(0)._2(i)) / list(0)._2(i)).sum / v.size
    println("Mean Error :" + error)

  }
}