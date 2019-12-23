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

  val alpha = 0.08 // part de l'énergie dans le PIB
  val gpt = 0.000355 // Taux de progrès technique

  def m(alpha: Double) = alpha / (1 - alpha) // 
  def rho(alpha: Double, gamma: Double) = 1 - alpha + alpha * gamma

  def main(args: Array[String]): Unit = {
    calibration
  }

  def calibration {
    val data = getLines("data_calibration", "\t").map(i => (i(0).toInt, i(1).toDouble, MegaTonOilEquivalent(i(2).toDouble), MegaTonOilEquivalent(i(3).toDouble), MegaTonOilEquivalent(i(4).toDouble), i(5).toDouble / 100))
    val ind = (0 until data.size).toList

    val year = data.map(_._1)
    val year_double = year.map(_.toDouble)
    val pib = data.map(_._2); val a = data.map(_._3); val e = data.map(_._4); val ce = data.map(_._5); val s = data.map(_._6)
    // Calculs directs
    val u = ind.map(i => a(i) + e(i))
    val qe = ind.map(i => a(i) / u(i))
    val ey = ind.map(i => e(i) - ce(i))
    val gamma = ind.map(i => ey(i) / e(i))

    val g = List((pib(1) - pib(0)) / pib(0)) ++ (0 until pib.size - 1).map(i => (pib(i + 1) - pib(i)) / pib(i)) // On met pour les 2 premières années le même taux de croissance pour ne pas avoir de pb de dimensions
    val gk = ind.map(i => g(i) - gpt) // Taux de croissance du capital

    val v = ind.map(i => s(i) / (gk(i) + delta)) // Intensité en capital de l'économie
    val p = ind.map(i => alpha * pib(i) / e(i).toKilowattHours) // Prix réel de l'énergie
    val qy = ind.map(i => e(i).toKilowattHours / pib(i) * gamma(i) / (1 - alpha + alpha * gamma(i))) // Intensité énergétique de l'économie
    val vy = ind.map(i => v(i) * (1 - alpha) / rho(alpha, gamma(i))) // Intensité capitalistique de l'économie
    val ve = ind.map(i => alpha * gamma(i) / (1 - alpha + alpha * gamma(i)) * (1 - qe(i)) / qy(i) * v(i)) // Intensité capitalistique du secteur énergétique

    val eroi = ind.map(i => 1 / (qe(i) + delta * ve(i) * qy(i)))

    plotXY(List((year_double, e.map(_.to(MegaTonOilEquivalent)), "E"), (year_double, a.map(_.to(MegaTonOilEquivalent)), "A"), (year_double, u.map(_.to(MegaTonOilEquivalent)), "U"), (year_double, ce.map(_.to(MegaTonOilEquivalent)), "Ce"), (year_double, ey.map(_.to(MegaTonOilEquivalent)), "Ey")), legend = true, yLabel = "[Mtoe]")
    plotXY(List((year_double, eroi, "")), yLabel = "EROI")

  }
 
  def simulation(K: Double, qy: Double, vy: Double, qe: Double, ve: Double) {
    // Constantes ?
    val s = .25; val n = 0.1
    // Calcul des prix
    val r = 1/(vy + qy*ve/(1-qe))
    val p = 1/qy*(1-r*vy)
    val pib = r*K
    val i = s*pib
    val y = (1+s*n)/(1+n)*pib
  }

  /**
   * OLD CALIBRATION MODEL
   */
  // Données observées
  val pib2017 = 8.02501E+13; val pib2016 = 7.77968E+13; val pib2015 = 7.58342E+13; // US $ 2010
  val U = MegaTonOilEquivalent(10537); val E = MegaTonOilEquivalent(9717);
  val A = MegaTonOilEquivalent(817); val Ey = MegaTonOilEquivalent(6316); val Ce = E - Ey; // Données IEA pour 2017, en Mtoe
  // val tfc2017 = 113012 * 1E9 // kWh
  val g0 = (pib2017 - pib2016) / pib2016
  val gpt0 = 0.00534
  val gk0 = g0 - gpt0
  val alpha0 = 0.08
  val gamma0 = Ey / E
  val s0 = 0.25
  // Estimations
  val m0 = m(alpha0) //280.0/4000 // GEMBA
  val qe0 = A / U
  val v0 = s0 / (gk0 + delta)
  val p0 = alpha0 * pib2017 / E.toKilowattHours
  val qy0 = E.toKilowattHours / pib2017 * gamma0 / (1 - alpha0 + alpha0 * gamma0) // 1/p0 * alpha0*gamma0 / (1-alpha0+alpha0*gamma0)// tfc2017/pib2017 // kWh / US $ 2010
  val rho0 = 1 - alpha0 + alpha0 * gamma0

  val vy = v0 * (1 - alpha0) / rho0 // / (1 + m0)
  val ve0 = alpha0 * gamma0 / (1 - alpha0 + alpha0 * gamma0) * (1 - qe0) / qy0 * v0

  val e0 = 1 / (qe0 + delta * ve0 * qy0)

  def ve(eroi: Double, qe: Double, qy: Double = qy0): Double = (1 - eroi * qe) / (eroi * delta * qy)
  def v(eroi: Double, qe: Double, qy: Double = qy0): Double = vy + ve(eroi, qe) * qy / (1 - qe)
  def delta_v(e0: Double, et: Double, qe0: Double, qet: Double, qy0: Double, qyt: Double) = v(et, qet, qyt) - v(e0, qe0, qy0)
  def gk(e: Double, qe: Double): Double = s0 / v(e, qe) - delta
  def g(e0: Double, et: Double, qe0: Double, qet: Double, qyt: Double = qy0) = (s0 - delta_v(e0, et, qe0, qet, qy0, qyt)) / (vy + ve(et, qet, qyt) * qyt / (1 - qet)) - delta

}