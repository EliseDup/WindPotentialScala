

import gridData.WorldGrid
import squants.space.Degrees
import squants.radio.WattsPerSquareMeter
import windEnergy.CapacityFactorCalculation
import squants.energy._
import squants.time.Hours
import utils.PlotHelper
import utils.Exajoules
import windEnergy.GustavsonWakeEffect
import squants.space.Meters
import squants.radio.Irradiance

object TopDownTest {

  val world = new WorldGrid("results/worldWind5years.txt", Degrees(0.5))
  val offshore1000m = world.offshoreGrids.filter(_.elevation.toMeters >= -1000)
  val grids = (world.onshoreGrids ++ offshore1000m)
  def cells(minSpeed: Double) = grids.filter(_.windSpeed.toMetersPerSecond >= minSpeed)

  def nD(cd: Irradiance): Double = {
    Math.sqrt(Megawatts(2) / ((Meters(80) * Meters(80)) * cd))
  }
  def lambda(cd: Irradiance): Double = {
    val n = nD(cd)
    Math.PI / (4 * n * n)
  }
  def main(args: Array[String]): Unit = {

    val cd = (1 to 300).map(_ * 0.1).toList
    val minSpeed = List(5)

    val list = minSpeed.map(v => {
      (cd, cd.map(c => {
        val cs = cells(v)
        val area = world.area(cs)
        cs.map(g => {
          val e = ((Math.min(1.0, c * CapacityFactorCalculation(g) * 0.9)) / (c * CapacityFactorCalculation(g)))
          (e * g.area / area)
        }).sum
      }), "Max 1.0 W/m^2 - " + v.toString)
    })

    PlotHelper.plotXY(list :+
      (cd, cd.map(c => GustavsonWakeEffect.wakeEffect(200, lambda(WattsPerSquareMeter(c)))), "Gustavson 50x50")
      :+ (cd, cd.map(c => GustavsonWakeEffect.wakeEffect(1000, lambda(WattsPerSquareMeter(c)),inf=true)), "Gustavson Inf"), xLabel = "Installed Capacity Density [W/m2]", yLabel ="Array Efficiency", legend = true)
  }
}