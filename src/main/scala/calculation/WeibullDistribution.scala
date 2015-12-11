package calculation

import org.apache.commons.math3.distribution.WeibullDistribution
import utils.PlotHelper

object LoadHourCalculation {
  def main(args: Array[String]): Unit = {
    val turbine2MW = new Enercon82_2000(80)
    val k = List(2.0)
    val v = (5 until 12).map(i => (0 until 10).map(_ * 0.1 + i)).flatten.toList
    val euEstimation = v.map(_*626.38-2003.3).toList
    val list = k.map(i =>{
      println(i)
        (v, v.map(v => new Weibull(v, i).capacityFactor(turbine2MW)*365*24).toList, "K factor: "+i)
    })
    PlotHelper.plotXY(list.toList :+ (v,euEstimation,"2 MW UE Estimation"))
   val error = (0 until v.size).map(i => Math.abs(euEstimation(i)-list(0)._2(i))/list(0)._2(i)).sum / v.size
    println("Mean Error :" + error)
   
  }
}

class Weibull(meanspeed: Double, k: Double) {
  val wb = new WeibullDistribution(k, meanspeed)
  val x = wb.sample(1000000).toList
  def plot = PlotHelper.histogram(x, 100, "Weibull distribution scale :" + meanspeed + ", shape:" + k)

  def capacityFactor(turbine: WindTurbine): Double = x.map(turbine.power(_)).sum / x.size / turbine.ratedPower
}