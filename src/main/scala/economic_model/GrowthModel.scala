package economic_model

import utils._
import squants.energy.Energy
import squants.energy.Joules

object GrowthModel {

  import Helper._
  import PlotHelper._
  def round(x: Double) = {
    math.round(100 * x) / 100.0
  }
  def main(args: Array[String]): Unit = {
    calibration(Some(0.5), plot = true)

    println("Hello World")
    //calibration(plot = true, rolling = (true, 5))
    // qe_by_sources
  }

  // Constantes du modèle
  val T = 25
  // // Taux de dépréciation du capital : Kt = (1-delta)^t * K0 => delta = 1 - (Kt/Ko)^1/t
  def delta_(t: Int, ratio_left: Double = 0.1) = 1.0 - math.pow(ratio_left, 1.0 / t)
  val delta = delta_(T, 0.1)

  val alpha = 0.08 // Energy-related industry value added as a percentage of GDP
  val gpt2017 = -0.2 / 100 // 0.1 / 100 // Taux de progrès technique

  def m(alpha: Double) = alpha / (1 - alpha) // 
  def rho(alpha: Double, gamma: Double) = 1 - alpha + alpha * gamma

  def loadCalibrationData(smoothing: (Boolean, Int)) = {
    val data = getLines("data_calibration", "\t").map(i => (i(0).toInt, i(1).toDouble, MegaTonOilEquivalent(i(2).toDouble), MegaTonOilEquivalent(i(3).toDouble), MegaTonOilEquivalent(i(4).toDouble), i(5).toDouble / 100, i(6).toDouble / 100))
    val n = data.size
    val ind = (0 until n).toList

    val year = data.map(_._1); val pib = data.map(_._2); val a = data.map(_._3); val e = data.map(_._4); val ce = data.map(_._5)

    val s = smooth_double(data.map(_._6), smoothing); val gpt = smooth_double(data.map(_._7), smoothing);

    // Calculs directs
    val u = ind.map(i => a(i) + e(i))
    val qe = smooth_double(ind.map(i => a(i) / u(i)), smoothing)
    val ey = ind.map(i => e(i) - ce(i))
    val gamma = smooth_double(ind.map(i => ey(i) / e(i)), smoothing)
    val g = smooth_double(List((pib(1) - pib(0)) / pib(0)) ++ (0 until pib.size - 1).map(i => (pib(i + 1) - pib(i)) / pib(i)), smoothing) // On met pour les 2 premières années le même taux de croissance pour ne pas avoir de pb de dimensions
    (n, ind, year, pib, a, e, ce, s, u, qe, ey, gamma, g, gpt)
  }

  def calibration(new_s: Option[Double] = None, plot: Boolean = false, smoothing: (Boolean, Int) = (false, 1), plotData: Boolean = false): (Double, Double, Double, Double, Double, Double) = {
    val (n, ind, year, pib, a, e, ce, s_bm, u, qe, ey, gamma, g, gpt) = loadCalibrationData(smoothing)

    val s = ind.map(i => new_s.getOrElse(s_bm(i)))
    val gk = ind.map(i => g(i) - gpt(i)) // Taux de croissance du capital

    val v = ind.map(i => s(i) / (gk(i) + delta)) // Intensité en capital de l'économie
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
      plotXY(List((year_double, eroi, "")), yLabel = "EROI", title = "eroi_calib")
      plotXY(List((year_double, eroi, "EROI"), (year_double, smooth_double(eroi, (true, 5)), "EROI_smoothed")), yLabel = "EROI", title = "eroi_calib_smoothed")
      // plotXY(List((year_double, ke, "Ke"), (year_double, ky, "Ky"), (year_double, k, "K")), legend = true)
      combinedPlots(year_double, List((s, "s"), (g, "g"), (qe, "qe"), (qy, "qy"), (ve, "ve"), (vy, "vy"), (v, "v")))
    }

    println("EROI 2017 (s" + 100 * s(n - 1) + " %)" + " \t" + eroi(n - 1))

    if (plotData) {
      plotXY(List((year_double, g.map(_ * 100), "g"), (year_double, gpt.map(_ * 100), "gpt"), (year_double, gk.map(_ * 100), "gk")), yLabel = "Growth Rate [%]", legend = true, title = "g_gk_gpt")
      plotXY(List((year_double, smooth_double(g.map(_ * 100), (true, 5)), "g"),
        (year_double, smooth_double(gpt.map(_ * 100), (true, 5)), "gpt"), (year_double, smooth_double(gk.map(_ * 100), (true, 5)), "gk")), yLabel = "Growth Rate [%]", legend = true, title = "g_gk_gpt_smoothed")
      plotXY(List((year_double, qe.map(_ * 100), ""), (year_double, smooth_double(qe.map(_ * 100), (true, 5)), "smoothed")), yLabel = "qe [%]", title = "qe_smoothed")

      plotXY(List((year_double, pib.map(_ / 1E9), "PIB")), yLabel = "World GDP [GUS $ 2010]", title = "gdp_data")
      plotXY(List((year_double, e.map(_.to(MegaTonOilEquivalent)), "E"), (year_double, a.map(_.to(MegaTonOilEquivalent)), "A"), (year_double, u.map(_.to(MegaTonOilEquivalent)), "U"), (year_double, ce.map(_.to(MegaTonOilEquivalent)), "Ce"), (year_double, ey.map(_.to(MegaTonOilEquivalent)), "Ey")), legend = true, yLabel = "[Mtoe]", title = "iea_data")
    }
    (eroi(n - 1), qy(n - 1), vy(n - 1), qe(n - 1), ve(n - 1), v(n - 1))
  }

  // Calcule la moyenne roulante sur n années
  def smooth_double(x: List[Double], n: (Boolean, Int)): List[Double] = {
    if (n._1) {
      List(x(0)) ++ ((1 until x.size).map(i => {
        (math.max(i - n._2 + 1, 0) to i).map(x(_)).sum / (math.min(i + 1, n._2))
      })).toList
    } else x
  }
  def smooth_energy(x: List[Energy], n: (Boolean, Int)): List[Energy] = {
    if (n._1) {
      List(x(0)) ++ ((1 until x.size).map(i => {
        (math.max(i - n._2 + 1, 0) to i).map(x(_)).foldLeft(Joules(0))(_ + _) / (math.min(i + 1, n._2))
      })).toList
    } else x
  }
  def qe_by_sources = {
    val tfc_data = getLines("total_final_consumption", "\t")
    val years = (1 until tfc_data.size).map(i => tfc_data(i)(0).toDouble).toList
    val tfc = (1 until tfc_data(0).size).map(i =>
      (tfc_data(0)(i).toString, (1 until tfc_data.size).map(j => TonOilEquivalent(tfc_data(j)(i).toDouble * 1000)).toList)).toMap
    val own_use_data = getLines("energy_industry_own_use", "\t")
    val own_use = (1 until own_use_data(0).size).map(i =>
      (own_use_data(0)(i).toString, (1 until own_use_data.size).map(j => -TonOilEquivalent(if (own_use_data(j)(i).nonEmpty) own_use_data(j)(i).toDouble * 1000 else 0.0)).toList)).toMap

    val sources = tfc.keys.toList.filter(i => !i.contains("Crude oil"))
    val qe = sources.map(s => (s, (0 until tfc(s).size).map(i => own_use(s)(i) / (own_use(s)(i) + tfc(s)(i))).toList)).toMap
    val qe_total = (0 until tfc(sources(0)).size).map(i => sources.map(s => own_use(s)(i).toJoules).sum / (sources.map(s => own_use(s)(i).toJoules).sum + sources.map(s => tfc(s)(i).toJoules).sum)).toList
    plotXY(qe.map(q => (years, q._2, q._1)).toList ++ List((years, qe_total, "Total")), legend = true)

  }

  def printTableCalibration(s: List[Option[Double]]) {
    val cals = s.map(i => calibration(i))
    print("$v$ & "); cals.map(cal => print(round(cal._6) + " & ")); println(" \\" + "\\")
    print("$q_y$ & "); cals.map(cal => print(round(cal._2) + " & ")); println(" \\" + "\\")
    print("$v_y$ & "); cals.map(cal => print(round(cal._3) + " & ")); println(" \\" + "\\")
    print("$v_e$ & "); cals.map(cal => print(round(cal._5) + " & ")); println(" \\" + "\\")
    print("$epsilon$ & "); cals.map(cal => print(round(cal._1) + " & ")); println(" \\" + "\\")
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