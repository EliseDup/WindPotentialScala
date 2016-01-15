package operation

import org.apache.commons.math3.distribution.WeibullDistribution
import utils.PlotHelper
import squants.motion.Velocity
import squants.motion.MetersPerSecond
import squants.energy.Kilowatts

object LoadHourCalculation {
  def main(args: Array[String]): Unit = {
    val turbine2MW = new WindTurbine2MW
    val k = List(1.75,2.0,2.4)
    val v = (1 until 14).map(i => (0 until 10).map(_ * 0.1 + i)).flatten.toList
    val euEstimation = v.map(i => Math.max(0, i*626.38-2003.3)).toList
    val list = k.map(i =>{
      println(i)
        (v, v.map(v => new Weibull(v, i).capacityFactor(turbine2MW)*365*24).toList, "K factor: "+i)
    })
    PlotHelper.plotXY(list.toList :+ (v,euEstimation,"2 MW EU Report Estimation"), xLabel="Mean Wind Speed", yLabel="Load Hours", legend=true)
   val error = (0 until v.size).map(i => Math.abs(euEstimation(i)-list(0)._2(i))/list(0)._2(i)).sum / v.size
    println("Mean Error :" + error)
   
  }
}

class Weibull(meanspeed: Double, k: Double) {
  val wb = new WeibullDistribution(k, meanspeed)
  val x = wb.sample(1000000).toList
  def plot = PlotHelper.histogram(x, 100, "Weibull distribution scale :" + meanspeed + ", shape:" + k)

  def capacityFactor(turbine: WindTurbine): Double = x.map(v => turbine.power(MetersPerSecond(v))).foldLeft(Kilowatts(0))(_ + _) / x.size / turbine.ratedPower
}