package windEnergy

import org.apache.commons.math3.distribution.WeibullDistribution
import utils.PlotHelper
import squants.motion.MetersPerSecond
import squants.energy.Kilowatts

object LoadHourCalculation {
  def main(args: Array[String]): Unit = {
    val turbine2MW = new WindTurbineWithPower("5MWoffshore")
  turbine2MW.plot
    val k = List(1.5,2.0,3.0)
    val v = (4 until 12).map(i => (0 until 10).map(_ * 0.1 + i)).flatten.toList
    val euEstimation = v.map(i => Math.max(0, Math.min(5500, i * 626.51 - 1901))).toList
    val hoogrijkEstimation = v.map(i => Math.max(0,Math.min(4000, i * 565 - 1745))).toList
    val list = k.map(i => {
      println(i)
      (v, v.map(v => new Weibull(v, i).capacityFactor(turbine2MW) * 365 * 24).toList, "K factor = " + i)
    })
    PlotHelper.plotXY(list.toList :+ (v, euEstimation, "EEA 2009") :+ (v, hoogrijkEstimation, "Hoogwijk 2004"), xLabel = "Mean Wind Speed [m/s]", yLabel = "Load Hours", legend = true)
    val error = (0 until v.size).map(i => Math.abs(euEstimation(i) - list(0)._2(i)) / list(0)._2(i)).sum / v.size
    println("Mean Error :" + error)

  }
}

class Weibull(meanspeed: Double, k: Double) {
  val wb = new WeibullDistribution(k, meanspeed)
  val x = wb.sample(1000000).toList
  def plot = PlotHelper.histogram(x, 100, "Weibull distribution scale :" + meanspeed + ", shape:" + k)

  def capacityFactor(turbine: WindTurbine): Double = x.map(v => turbine.power(MetersPerSecond(v))).foldLeft(Kilowatts(0))(_ + _) / x.size / turbine.ratedPower
}