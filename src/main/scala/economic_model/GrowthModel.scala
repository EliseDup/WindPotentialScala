package economic_model

import utils._

object GrowthModel {

  import Helper._
  import PlotHelper._

  // Constantes du modèle
  val T = 25
  // // Taux de dépréciation du capital : Kt = (1-delta)^t * K0 => delta = 1 - (Kt/Ko)^1/t
  def delta_(t: Int, ratio_left: Double = 0.1) = 1.0 - math.pow(ratio_left, 1.0 / t)
  val delta = delta_(T, 0.1)

  val alpha = 0.05 // Energy-related industry value added as a percentage of GDP
  val gpt = 0.1 / 100 // Taux de progrès technique

  def m(alpha: Double) = alpha / (1 - alpha) // 
  def rho(alpha: Double, gamma: Double) = 1 - alpha + alpha * gamma

  val (k, qy, vy, qe, ve) = calibration()
 
  def loadCalibrationData = {
    val data = getLines("data_calibration", "\t").map(i => (i(0).toInt, i(1).toDouble, MegaTonOilEquivalent(i(2).toDouble), MegaTonOilEquivalent(i(3).toDouble), MegaTonOilEquivalent(i(4).toDouble), i(5).toDouble / 100))
    val n = data.size
    val ind = (0 until n).toList

    val year = data.map(_._1)
    val pib = data.map(_._2); val a = data.map(_._3); val e = data.map(_._4); val ce = data.map(_._5); val s = data.map(_._6)
    // Calculs directs
    val u = ind.map(i => a(i) + e(i))
    val qe = ind.map(i => a(i) / u(i))
    val ey = ind.map(i => e(i) - ce(i))
    val gamma = ind.map(i => ey(i) / e(i))
    val g = List((pib(1) - pib(0)) / pib(0)) ++ (0 until pib.size - 1).map(i => (pib(i + 1) - pib(i)) / pib(i)) // On met pour les 2 premières années le même taux de croissance pour ne pas avoir de pb de dimensions
    (n, ind, year, pib, a, e, ce, s, u, qe, ey, gamma, g)
  }
  
  def calibration(new_s: Option[Double] = None, plot: Boolean = false): (Double, Double, Double, Double, Double) = {
    val (n, ind, year, pib, a, e, ce, s, u, qe, ey, gamma, g) = loadCalibrationData
    val gk = ind.map(i => g(i) - gpt) // Taux de croissance du capital

    val v = ind.map(i => new_s.getOrElse(s(i)) / (gk(i) + delta)) // Intensité en capital de l'économie
    val p = ind.map(i => alpha * pib(i) / e(i).toKilowattHours) // Prix réel de l'énergie
    val qy = ind.map(i => e(i).toKilowattHours / pib(i) * gamma(i) / (1 - alpha + alpha * gamma(i))) // Intensité énergétique de l'économie
    val vy = ind.map(i => v(i) * (1 - alpha) / rho(alpha, gamma(i))) // Intensité capitalistique de l'économie
    val ve = ind.map(i => alpha * gamma(i) / (1 - alpha + alpha * gamma(i)) * (1 - qe(i)) / qy(i) * v(i)) // Intensité capitalistique du secteur énergétique
    val eroi = ind.map(i => 1 / (qe(i) + delta * ve(i) * qy(i)))
    // Y = PIB - pCe
    val y = ind.map(i => pib(i) - p(i) * ce(i).toKilowattHours)
    val ky = ind.map(i => y(i) * vy(i))
    val ke = ind.map(i => u(i).toKilowattHours * ve(i))
    val k = ind.map(i => ke(i) + ky(i))

    val year_double = year.map(_.toDouble)

    if (plot) {
      // plotXY(List((year_double,pib,"PIB"),(year_double,y,"Y")), "[US $ 2010]",legend = true)
      plotXY(List((year_double, e.map(_.to(MegaTonOilEquivalent)), "E"), (year_double, a.map(_.to(MegaTonOilEquivalent)), "A"), (year_double, u.map(_.to(MegaTonOilEquivalent)), "U"), (year_double, ce.map(_.to(MegaTonOilEquivalent)), "Ce"), (year_double, ey.map(_.to(MegaTonOilEquivalent)), "Ey")), legend = true, yLabel = "[Mtoe]")
      plotXY(List((year_double, eroi, "")), yLabel = "EROI")
      // plotXY(List((year_double, ke, "Ke"), (year_double, ky, "Ky"), (year_double, k, "K")), legend = true)
      plotXY(List((year_double, qe, "")), yLabel = "qe")
      combinedPlots(year_double, List((s, "s"), (g, "g"), (qe, "qe"), (qy, "qy"), (ve, "ve"), (vy, "vy"), (v, "v")))

    }

    println("EROI 2017 " + " \t" + eroi(n - 1))
    (k(n - 1), qy(n - 1), vy(n - 1), qe(n - 1), ve(n - 1))
  }

  def main(args: Array[String]): Unit = {

    val (k, qy, vy, qe, ve) = calibration(None, plot = true)
    println(k, qy, vy, qe, ve)
    println(qe + "\t" + delta * qy * ve)
    val (pib, eroi) = resolution(k, qy, vy, qe, ve)
    println(pib + "\t" + eroi)
    //plotXY(List(calibration(None), calibration(Some(0.5)), calibration(Some(0.8))), yLabel = "EROI", legend = true)

  }
  // For a given set of parameters (K, qy, vy, qe, ve) and fixed parameters s (saving rate) and n (consumption "structure" of house holds = pCe / Cy), solves the model
  def resolution(K: Double, qy: Double, vy: Double, qe: Double, ve: Double): (Double, Double) = {

    val s = .5; val n = 0.1;

    // Prices calculation
    // r = rental price of capital
    val r = 1 / (vy + qy * ve / (1 - qe))
    // p = purchase price of energy
    val p = 1 / qy * (1 - r * vy)

    val pib = r * K
    val i = s * pib
    val y = (1 + s * n) / (1 + n) * pib

    // Variables macro
    val cy = y - i
    val ce = n * cy / p
    val ky = vy * y
    val ey = qy * y
    val e = ey + ce
    val u = e / (1 - qe)
    val ke = ve * u
    // Variables de résultat
    val gamma = ey / e
    val m = ke / ky
    val v = 1 / r

    val eroi = 1 / (qe + delta * ve * qy)
    (pib, eroi)
  }

  /**
   * OLD CALIBRATION MODEL
   *
   * // Données observées
   * val pib2017 = 8.02501E+13; val pib2016 = 7.77968E+13; val pib2015 = 7.58342E+13; // US $ 2010
   * val U = MegaTonOilEquivalent(10537); val E = MegaTonOilEquivalent(9717);
   * val A = MegaTonOilEquivalent(817); val Ey = MegaTonOilEquivalent(6316); val Ce = E - Ey; // Données IEA pour 2017, en Mtoe
   * // val tfc2017 = 113012 * 1E9 // kWh
   * val g0 = (pib2017 - pib2016) / pib2016
   * val gpt0 = 0.00534
   * val gk0 = g0 - gpt0
   * val alpha0 = 0.08
   * val gamma0 = Ey / E
   * val s0 = 0.25
   * // Estimations
   * val m0 = m(alpha0) //280.0/4000 // GEMBA
   * val qe0 = A / U
   * val v0 = s0 / (gk0 + delta)
   * val p0 = alpha0 * pib2017 / E.toKilowattHours
   * val qy0 = E.toKilowattHours / pib2017 * gamma0 / (1 - alpha0 + alpha0 * gamma0) // 1/p0 * alpha0*gamma0 / (1-alpha0+alpha0*gamma0)// tfc2017/pib2017 // kWh / US $ 2010
   * val rho0 = 1 - alpha0 + alpha0 * gamma0
   *
   * val vy = v0 * (1 - alpha0) / rho0 // / (1 + m0)
   * val ve0 = alpha0 * gamma0 / (1 - alpha0 + alpha0 * gamma0) * (1 - qe0) / qy0 * v0
   *
   * val e0 = 1 / (qe0 + delta * ve0 * qy0)
   *
   * def ve(eroi: Double, qe: Double, qy: Double = qy0): Double = (1 - eroi * qe) / (eroi * delta * qy)
   * def v(eroi: Double, qe: Double, qy: Double = qy0): Double = vy + ve(eroi, qe) * qy / (1 - qe)
   * def delta_v(e0: Double, et: Double, qe0: Double, qet: Double, qy0: Double, qyt: Double) = v(et, qet, qyt) - v(e0, qe0, qy0)
   * def gk(e: Double, qe: Double): Double = s0 / v(e, qe) - delta
   * def g(e0: Double, et: Double, qe0: Double, qet: Double, qyt: Double = qy0) = (s0 - delta_v(e0, et, qe0, qet, qy0, qyt)) / (vy + ve(et, qet, qyt) * qyt / (1 - qet)) - delta
   */
}