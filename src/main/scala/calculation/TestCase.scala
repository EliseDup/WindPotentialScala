package calculation

import utils.PlotHelper

/**
 *
 * Energy and CO2 life-cycle analysis of wind turbines - review and applications
 *
 * From M. Lenzen, J. Munksgaard, Renewable Energy 26 (2002) 339-362
 *
 */
object TestCase {
  def main(args: Array[String]): Unit = {
    val test = new Case
    PlotHelper.plotXY(List((test.t, test.powConstantRate, "Constant Rate"),
      (test.t, test.powConstantGrowth, "Constant Growth"),
      (test.t, test.noConstantRate, "Turbines Operation Constant Rate"),
      (test.t, test.noConstantGrowth, "Turbines Operation Constant Growth"),
      (test.t, test.ncConstantRate, "Turbines Construction Constant Rate"),
      (test.t, test.ncConstantGrowth, "Turbines Construction Constant Growth")))
  }
}
class Case {
  /**
   * T : Lifetime
   * C : Construction time
   * loadFactor
   * Rated power of wind turbines used [kW]
   * Epsilon_fossil : mean conversion efficiency of thermal electricity generation
   * 	=> Electricity equivalent of the primary energy requirement E for the construction of the plants = epsilon_fossil*E
   * Kappa : substituting rate of the installation
   * Tau : factor for the linear relationship between the rated power P and the energy embodied E
   *  => E = tau*P = 0.01 TJ / kW =0.336
   * Alpha : acceleration of the capacity growth
   * N_O^0 : Number of turbines operating in t0 = if 500MW installed = 1000 WT of 500kW
   *
   */
  val t = (0 until 1000).map(_ / 10.0 + 0.1).toList
  val T = 20
  val C = 1
  val loadFactor = 0.25
  val ratedPower = 500 * Math.pow(10, 3)
  val epsilon_fossil = 0.35
  val kappa = 52.6
  val tau = 0.336
  val alpha = 109
  val n_operation_0 = 1000
  /**
   * Available power when there are n wind turbines available
   */
  def turbine_construction(t: Double, acceleration: Boolean): Double = {
    if (acceleration) kappa
    else {
      if (t < C) kappa * t
      else kappa * C
    }
  }
  /**
   * With dNS/dt = kappa (constant rate) => NS(t) = kappa*t
   * With dNS/dt = alpha*t (constant acceleration) => NS(t) = alpha*t^2/2
   */
  def turbine_operation(t: Double, acceleration: Boolean): Double = {
    if (acceleration) n_operation_0 + Math.min(2000, alpha * Math.pow(t, 2) / 2)
    else {
      val factor = if (t < C) 0.0 else if (t < T) t - C else T - C
      n_operation_0 + factor * kappa
    }
  }

  /**
   * P_a = P * load * NO - epsilon_fossil * E / C * NC
   */
  def availablePower(t: Double, acceleration: Boolean): Double = {
    val no = turbine_operation(t, acceleration)
    val nc = turbine_construction(t, acceleration)
    ratedPower * (loadFactor * no - epsilon_fossil * tau / C * nc)
  }
  val powConstantRate = t.map(availablePower(_, false) / Math.pow(10, 6))
  val powConstantGrowth = t.map(availablePower(_, true) / Math.pow(10, 6))
  val noConstantRate = t.map(turbine_operation(_, false))
  val noConstantGrowth = t.map(turbine_operation(_, true))
  val ncConstantRate = t.map(turbine_construction(_, false))
  val ncConstantGrowth = t.map(turbine_construction(_, true))
}