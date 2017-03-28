package wind_energy

import squants.space.Length
import squants.space.Area
import squants.SquantifiedDouble
import squants.SquantifiedLong
import utils.PlotHelper
/**
 * From Gustavson : Limits to Wind Power Density (1979)
 * We approach the values presented in the report with a negative exponential ~ y = a exp (-bx)
 * x = lambda = Area_rotor / Area_turbine = (𝜋D²/4) / nD * nD = 𝜋 / 4n²
 * Parameter a and b are calculated with 'nls' (nonlinear least squares) function in R
 *
 * Values are found for arrays of 5x5, 10x10, 50x50 and ~ Infinite size
 */
object GustavsonWakeEffect {
  val a5 = 0.9943; val b5 = 5.2661;
  val a10 = 0.9871; val b10 = 11.7542;
  val a50 = 0.9838; val b50 = 42.5681;
  val aInf = 0.9619; val bInf = 88.9204;

  def exp(lambda: Double, a: Double, b: Double) = a * Math.exp(-b * lambda)
  def arrayEfficiency(nT: Double, lambda: Double, inf: Boolean = false) ={
    if (nT <= 25) exp(lambda, a5, b5)
    else if (nT <= 100) exp(lambda, a10, b10)
    else if (inf && nT > 2500) exp(lambda, aInf, bInf)
    else exp(lambda, a50, b50)
  }

  def plot {
    val n = (500 to 2500).map(_ * 0.01).toList
    def eff(size: Int) = n.map(i => arrayEfficiency(size, Math.PI/(4*i*i), true)*100)
    val list = List(5, 10, 50, 100).map(s => (n, eff(s*s), if(s<100) s.toString + "x" + s.toString else "Infinite"))
    PlotHelper.plotXY(list, xLabel = "Turbine Spacing [nD]", yLabel = "Array Efficiency [%]", legend = true, tick = (true,5,20))
  }
}
/**
 * Array sizeTurbine spacing
 * Array efficiency(%)
 * 													4D  5D  6D  7D  8D  9D
 * 2x2 											81  87  91  93  95  96
 * 4x4 											65  76  82  87  90  92
 * 6x6 											57  70  78  83  87  90
 * 8x8 											52  66  75  81  85  88
 * 10x10 										49  63  73  79  84  87
 *
 * From : A methodological review to estimate techno-economical wind energy production - Julieta Schallenberg-Rodriguez
 *
 *
 */

object WakeEffect {

  // We have to fix a spacing and then we wan evalate the number of turbines in the area and the corresponding array efficieny
  // So to put one wind turbine one need a space of nD * nD (a square surrounding the wind turbine)
  // 9D with 80m diameter is about 2 wind turbine per square kilometers

  val n = Array(4, 16, 36, 64, 100)
  // Map (nDiameters => Array(wake effect) )
  val array = Map(
    4 -> Array(0.81, 0.65, 0.57, 0.52, 0.49),
    5 -> Array(0.87, 0.76, 0.7, 0.66, 0.63),
    6 -> Array(0.91, 0.82, 0.78, 0.75, 0.73),
    7 -> Array(0.93, 0.87, 0.83, 0.81, 0.79),
    8 -> Array(0.95, 0.9, 0.87, 0.85, 0.84),
    9 -> Array(0.96, 0.92, 0.9, 0.88, 0.87))

  def nTurbines(x: Length, y: Length, d: Length, nDiameters: Int) = (x / (nDiameters * d) * y / (nDiameters * d))
  def nTurbines(area: Area, d: Length, nDiameters: Int) = area / ((d * nDiameters) * (d * nDiameters))

  def wakeEffect(area: Area, d: Length, nDiameters: Int): Double = wakeEffect(nTurbines(area, d, nDiameters), nDiameters)
  def wakeEffect(nTurbines: Double, nDiameters: Int) = {
    if (nTurbines >= 100 * 100) 0.62
    val nIndex =
      if (nTurbines <= n(0)) 0
      else if (nTurbines <= n(1)) 1
      else if (nTurbines <= n(2)) 2
      else if (nTurbines <= n(3)) 3
      else 4
    val nD =
      if (nDiameters < 4) 4
      else if (nDiameters > 9) 9
      else nDiameters
    array(nD)(nIndex)

  }
}