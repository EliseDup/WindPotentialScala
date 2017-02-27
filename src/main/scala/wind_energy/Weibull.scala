package wind_energy

import org.apache.commons.math3.distribution.WeibullDistribution
import squants.motion._
import squants.space._
import squants.energy._
import org.apache.commons.math3.special.Gamma
import grid._
import utils._

/**
 * Weibull distribution at 10 m height
 *
 * Justus and Mikhail
 */
object Weibull {
  def shapeParameter(meanSpeed: Velocity, std: Double) = Math.pow(std / meanSpeed.toMetersPerSecond, -1.086)
  def scaleParameter(meanSpeed: Velocity, k: Double) = meanSpeed / Gamma.gamma(1 + 1.0 / k)
  def meanSpeed(c: Velocity, k: Double) = c * Gamma.gamma(1 + 1.0 / k)
  
  // Maximum instant power = 1/2 rho A int0^infty P(v) v^3 dv
  def maxPower(cell : GridCell): Power = maxPower(cell.windSpeed, cell.weibull.k, WindPotential.diameterRotor(cell), WindPotential.hubAltitude(cell))
  def maxPower(meanSpeed: Velocity, k: Double, d: Length = Meters(80), elevation : Length = Meters(100)): Power = 
    Watts(16.0 / 27 * 0.5 * Thermodynamics.airDensity(elevation).toKilogramsPerCubicMeter * Math.PI * Math.pow(d.toMeters/2,2) * Math.pow(meanSpeed.toMetersPerSecond, 3) *  Gamma.gamma(1 + 3.0 / k) / Math.pow( Gamma.gamma(1 + 1.0 / k),3))
}

object WeibullParametersExtrapolation {
  // Extrapolation
  val z1 = Meters(10)
  val a = 0.0881
  val b = 0.37
  def zg(z2: Length) = Meters(Math.sqrt(z1.toMeters * z2.toMeters))

  def n(c1: Velocity, z2: Length, z0: Length) = 1 / Math.log(zg(z2) / z0) - (a * Math.log(c1.toMetersPerSecond / 6))

  def c2(c1: Velocity, z2: Length, z0: Length) = c1 * Math.pow(z2 / z1, n(c1, z2, z0))
  def k2(k1: Double, z2: Length, z0: Length) = k1 / (1 - a * Math.log(z2 / z1))

}

case class WeibullParameters(mean: Velocity, std: Double) {
  val k = Weibull.shapeParameter(mean, std)
  val c = Weibull.scaleParameter(mean, k)
  //val kHub = WeibullParametersExtrapolation.k2(k, hHub, z0)
  //val cHub = WeibullParametersExtrapolation.c2(c, hHub, z0)
  //val meanHub = Weibull.meanSpeed(cHub, kHub)
  override def toString() = "k :" + k + ", c: " + c
}

/**
 * Comparative analysis on power curve models of wind turbine generator in estimating capacity factor
 *
 * Tian-Pau Chang, Feng-Jiao Liu, Hong-Hsi Ko, Shih-Ping Cheng, Li-Chung Sun, Shye-Chorng Kuo
 */

object CapacityFactorCalculation {
  // Rated speed vr can be optimized given the Weibull parameter of the wind speed in a location
  val vf = 25.0
  def apply(cell: GridCell): Double = cubic(cell)
  def general(cell: GridCell): Double = {
    (12 to 14).map(i => general(cell.weibull.c.toMetersPerSecond, cell.weibull.k, 3.0, i)).max
  }
  def cubic(cell: GridCell): Double = {
    (12 to 14).map(i => cubic(cell.weibull.c.toMetersPerSecond, cell.weibull.k, 3.0, i)).max
  }
  
  // It will always be the lowest right ?!
  def optimalRatedSpeed(cell : GridCell) : Double = {
    (22 to 28).map(_*0.5).map(i => (i,cubic(cell.weibull.c.toMetersPerSecond, cell.weibull.k, 3.0, i))).maxBy(_._2)._1
  }
      
  def powerCurve(v: Double, vc: Double, vr: Double) = {
    if (v < vc) 0.0
    else if (v <= vr) (^(v, 3) - ^(vc, 3)) / (^(vr, 3) - ^(vc, 3))
    else if (v <= vf) 1.0
    else 0.0
  }

  def general(c: Double, k: Double, vc: Double, vr: Double): Double = {
    (Math.exp(-Math.pow(vc / c, k)) - Math.exp(-Math.pow(vr / c, k))) / (Math.pow(vr / c, k) - Math.pow(vc / c, k)) - Math.exp(-Math.pow(vf / c, k))
  }

  def e(d: Double) = Math.exp(d)
  def ^(a: Double, b: Double) = Math.pow(a, b)
  // Regularized = Incomplete / Gamma ?? It seems regularized Gamma = Incomplete in that case ..
  def incompleteGamma(x: Double, u: Double) = {
     Gamma.regularizedGammaP(x, u)
  }

  def cubic(c: Double, k: Double, vc: Double, vr: Double): Double = {
  -e(- ^(vf / c, k)) + (3 * ^(c, 3) * Gamma.gamma(3.0 / k)) / (k * (^(vr, 3) - ^(vc, 3))) * (incompleteGamma(3.0 / k, ^(vr / c, k)) - incompleteGamma(3.0 / k, ^(vc / c, k)))
  }

  def linear(c: Double, k: Double, vc: Double, vr: Double): Double = {
    c / (vr - vc) * (Math.exp(- ^(vc / c, k)) - Math.exp(- ^(vr / c, k))) - Math.exp(- ^(vf / c, k))
  }

  def main(args: Array[String]): Unit = {
    val v = (1 until 15).map(i => (0 until 10).map(_ * 0.1 + i)).flatten.toList
    val k = List(1, 1.5, 2, 2.5, 3.0)
    val vr = (12 to 15).map(_.toDouble).toList
    val list = k.map(j => (v, v.map(i => CapacityFactorCalculation.cubic(Weibull.scaleParameter(MetersPerSecond(i),j).toMetersPerSecond, j, 4.0, 12.0)), "k=" + j.toString))
    PlotHelper.plotXY(list, legend = true, xLabel = "Mean Wind Speed [m/s]", yLabel = "Capacity Factor", save = true)
    
    k.map(j => {
      println(j + "\t" + v.map(i => (i, CapacityFactorCalculation.cubic(i, j, 4.0, 12.0))).maxBy(_._2))
    })

  }
  
  def meanSpeed(cf : Double): Velocity = {
    var c = 0.1
    var cf1 = cubic(c, 2.0, 4.0, 12.0)
    while(Math.abs(cf1-cf) > 0.01){
      c += 0.1
      cf1 = cubic(c, 2.0, 4.0, 12.0)
    }
    Weibull.meanSpeed(MetersPerSecond(c), 2.0)
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