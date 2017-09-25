package wind_energy

import squants.space._
import squants.motion._
import squants.mass._
import squants.radio._
import grid.GridCell
import org.apache.commons.math3.special.Gamma
import utils.PlotHelper

object WindPower {
  // Pressure = 1bar * (1 - 0.0065 z / T)^5.255
  def pressure(elevation: Length) = Pascals(101325) * Math.pow(1 - 0.0065 * elevation.toMeters / 288.15, 5.255)
  // density = P/RT
  def airDensity(elevation: Length) = KilogramsPerCubicMeter(pressure(elevation).toPascals / (287.05 * 288.15))
  // Power Density = 1/2 pho v^3
  def windPowerDensity(windSpeed: Velocity, elevation: Length) = WattsPerSquareMeter(0.5 * airDensity(elevation).toKilogramsPerCubicMeter * Math.pow(windSpeed.toMetersPerSecond, 3))
  def theoreticalWindPowerDensity(cell: GridCell, wind: WindProfile) = WattsPerSquareMeter(0.5 * airDensity(wind.height + cell.altitude).toKilogramsPerCubicMeter * Math.pow(wind.c.toMetersPerSecond, 3) * Gamma.gamma(1 + 3.0 / wind.k))
}

class WindProfile(val mean: Velocity, val std: Double, val height : Length) {
  val k = Weibull.shapeParameter(mean, std)
  val c = Weibull.scaleParameter(mean, k)
  override def toString() = "Wind Profile at "+ height + " : mean speed " + mean + ", weibull parameters = k :" + k + ", c: " + c
}

/** From Gustavson : Limits to Wind Power Density (1979)
 * We approach the values presented in the report with a negative exponential ~ y = a exp (-bx)
 * x = lambda = Area_rotor / Area_turbine = (𝜋D²/4) / nD * nD = 𝜋 / 4n²
 * Parameter a and b are calculated with 'nls' (nonlinear least squares) function in R
 *
 * Values are found for arrays of 5x5, 10x10, 50x50 and ~ Infinite size
 */
object WakeEffect {
  val a5 = 0.9943; val b5 = 5.2661;
  val a10 = 0.9871; val b10 = 11.7542;
  val a50 = 0.9838; val b50 = 42.5681;
  val aInf = 0.9619; val bInf = 88.9204;

  def exp(lambda: Double, a: Double, b: Double) = a * Math.exp(-b * lambda)
  
  def arrayEfficiency(nT: Double, lambda: Double, inf: Boolean = false) = {
    if (nT <= 25) exp(lambda, a5, b5)
    else if (nT <= 100) exp(lambda, a10, b10)
    else if (inf && nT > 2500) exp(lambda, aInf, bInf)
    else exp(lambda, a50, b50)
  }

  def plot {
    val n = (500 to 2500).map(_ * 0.01).toList
    def eff(size: Int) = n.map(i => arrayEfficiency(size, Math.PI/(4*i*i), true)*100)
    val list = List(5, 10, 50, 1000).map(s => (n, eff(s*s), if(s<100) s.toString + "x" + s.toString else "Infinite"))
    PlotHelper.plotXY(list, xLabel = "Turbine Spacing [RD]", yLabel = "Array Efficiency [%]", legend = true, tick = (true,5,20))
  }
}