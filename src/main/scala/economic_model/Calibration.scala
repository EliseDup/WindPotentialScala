package economic_model
import utils._
import squants.energy._

// Vector of technical parameters
case class Z(ve: Double, vf: Double, qe: Double, qf: Double, le: Double, lf: Double, deltae: Double, deltaf: Double) {
  override def toString() = { vf + "\t" + qf + "\t" + lf + "\t" + deltaf + "\t" + ve + "\t" + qe + "\t" + le + "\t" + deltae }
}
object ModelResolution{
  def apply(c : calibration_results_work):ModelResolution = new ModelResolution(c.z, c.data.ye(c.i), c.energy_units, c.L, c.s, c.n)
}

class ModelResolution(val z: Z, val ye: Energy, val energy_units: EnergyUnit, val L: Double, val s: Double,
    val n: Double) {
  // L = le Ye + lf Yf
  val yf = (L - z.le * ye.to(energy_units)) / z.lf
  val ke = z.ve * ye.to(energy_units)
  val kf = z.vf * yf
  // Ce = (1-qe)Ye - qf Yf
  val ce = (1 - z.qe) * ye - energy_units(z.qf * yf)
  val cf = cf_s_n(s, n)
  val p = p_s_n(s, n)
  // Cf = (1-s)/(1+sn) * yf

  def cf_s_n(s: Double, n: Double) = (1 - s) / (1 + s * n) * yf
  def p_s_n(s: Double, n: Double) = n * cf_s_n(s, n) / ce.to(energy_units)

  // Pertes / PIN
  val eroi = 1 / (z.qe + z.deltae * z.ve * z.qf)
  val p1 = 1 - ((1 - z.qe) * (1 - z.deltaf * z.vf) - (-z.qf * -z.deltae * z.ve))
  val PIN1 = ye.to(energy_units) / z.qf
  val PIN2 = PIN1 * (1 - 1.0 / eroi)
  val PIN3 = PIN1 * (1 - p1)
  val xi = ce / ye
  val p2 = (1 - z.deltaf * z.vf - p * z.qf) * xi
  val PIN4 = PIN1 * (1 - p1 - p2)
  
  // Pertes %
  val perte1 = 1.0/eroi
  val perte2 = p1
  val perte3 = p1+p2
}

class ImpactPER(val z: Z) {
  // Pertes / PIN
  val eroi = 1 / (z.qe + z.deltae * z.ve * z.qf)

  // Link between m = Ke/Kf, k = K/Ye, & mu = ve/k = Ke/K
  def m_mu(mu: Double) = mu / (1 - mu)
  def mu_m(m: Double) = m / (1 + m)
  def k_m(m: Double) = z.ve * (m + 1) / m

  def mean_std(a: (Double, Double)) = ((a._1 + a._2) / 2, math.abs(a._1 - (a._1 + a._2) / 2))

  // Conditions d'existence: intervalle admissible pour phi = 1/p
  val b1 = z.lf * (1 - z.qe) / z.le + z.qf
  val b2 = z.vf * (1 - z.qe) / z.ve + z.qf
  val interval_phi = (b2, b1)
  val interval_p = (1.0 / b1, 1.0 / b2)
  val p = mean_std(interval_p)
  // Exercice 1 : n & s fixed
  def interval_m_gk(s: Double, n: Double) = {
    val n_p = (1 - s) * n / (1 + s * n)
    val a = (1 + n_p) * z.qf / (1 - z.qe)
    (n_p + z.ve / z.vf * a, z.ve / z.vf * (n_p * z.lf / z.le + a))
  }
  def gk_m(m: Double, s: Double, n: Double) = {
    1 / (1 + m) * (1 / z.vf * (1 + n) * s / (1 + s * n) - m * z.deltae - z.deltaf)
  }
  def interval_gk(s: Double, n: Double) = {
    val (mi, ms) = interval_m_gk(s, n)
    (gk_m(mi, s, n), gk_m(ms, s, n))
  }
  def delta_m(m: Double) = 1.0 / (1 + m) * (m * z.deltae + z.deltaf)
  def interval_delta_gk(s: Double, n: Double) = {
    val (mi, ms) = interval_m_gk(s, n)
    (delta_m(mi), delta_m(ms))
  }
  // Exercice 2 : gk & n fixed
  def interval_m_s(gk: Double, n: Double) = {
    val m_inf = (b1 * n * (1 - z.vf * (gk + z.deltaf)) + z.qf) / (z.vf / z.ve * (1 - z.qe) + b1 * n * z.vf * (gk + z.deltae))
    val m_sup = (b2 * n * (1 - z.vf * (gk + z.deltaf)) + z.qf) / (z.vf / z.ve * (1 - z.qe) + b2 * n * z.vf * (gk + z.deltae))
    (List(m_inf, m_sup).min, List(m_inf, m_sup).max)
  }
  // Link between s and n : (1+s)*n/(1+s*n) = f(m,z)
  def f(m: Double, gk: Double) = z.vf * (m * (gk + z.deltae) + gk + z.deltaf)
  def s(m: Double, gk: Double, n: Double) = {
    f(m, gk) / (1 + n - n * f(m, gk))
  }
  def interval_s(gk: Double, n: Double) = {
    val m = interval_m_s(gk, n)
    (s(m._1, gk, n), s(m._2, gk, n))
  }

  // Exercice 3: gk & c = Ce/Cf fixed (impact on n & s)
  def m_c(gk: Double, c: Double) = {
    (c * (1 / z.vf - (gk + z.deltaf)) + z.qf / z.vf) / ((1 - z.qe) / z.ve + c * (gk + z.deltae))
  }
  def interval_c(c: Double) = (c / b1, c / b2)
  def interval_s_c(gk: Double, c: Double) = {
    val fm = f(m_c(gk, c), gk)
    (fm / (1 + c / b2 - c / b2 * fm), fm / (1 + c / b1 - c / b1 * fm))
  }
  /*
  // Old model with k
  def interval_gk2(s: Double, n: Double) = {
    val (ki, ks) = interval_k2(s, n)
    val sy = s * (1 + n) / (1 + s * n); // I/Y
    val x = 1 / z.vf * sy
    val a = x - z.deltaf
    val b = (z.deltaf - z.deltae - x)
    (a + b * z.ve / ki, a + b * z.ve / ks)
  }

  // Conditions d'existence: intervalle admissible pour k = K/U
  def interval_k2(s: Double, n: Double) = {
    // n'
    val m = (1 - s) * n / (1 + s * n)
    val k1 = m * z.lf / z.le + (1 + m) * z.qf / (1 - z.qe)
    val k2 = m * z.vf / z.ve + (1 + m) * z.qf / (1 - z.qe)
    val bounds = List(z.ve + z.vf / k2, z.ve + z.vf / k1)
    (bounds.min, bounds.max)
  }
  def interval_mu2_s(gk: Double, n: Double) = {
    val bounds = List(
      (n * b1 * (1 / z.vf - (gk + z.deltaf)) + z.qf / z.vf) / ((1 - z.qe) / z.ve + z.qf / z.vf + n * b1 * (1 / z.vf - (z.deltaf - z.deltae))),
      (n * b2 * (1 / z.vf - (gk + z.deltaf)) + z.qf / z.vf) / ((1 - z.qe) / z.ve + z.qf / z.vf + n * b2 * (1 / z.vf - (z.deltaf - z.deltae))))
    (bounds.min, bounds.max)
  }
  def interval_s_mu(gk: Double, n: Double) = {
    val mu = interval_mu2_s(gk, n)
    def s(mu: Double) = 1 / ((1 + n) * (1 - mu) / (z.vf * (gk + z.deltaf - (z.deltaf - z.deltae) * mu)) - n)
    (s(mu._1), s(mu._2))
  }
  def interval_m2(ki: Double, ks: Double, ve: Double) = {
    (ve / (ki - ve), ve / (ks - ve))
  }
*/
}

class calibration_results_work(val year: Int = 2017, val Tf: Int = 20, val Te: Int = 25, val alpha: Double = 6.0 / 100, val m: Double = 280.0 / 4000, val theta: Double = 0.4, val energy_units: EnergyUnit = KilowattHours, val pib_units: Int = 1,
    val pop_units: Int = 1) {

  val i = Calibration.index_year(year)
  val delta_f = 1.0 / Tf; val delta_e = 1.0 / Te;
  val delta = (m * delta_e + delta_f) / (1 + m)
  val data = Calibration.data
  // Observed data  for year i
  val pib = data.pib(i) / pib_units;
  val g = data.g(i); val s = data.s(i);
  val qe = data.qe(i)
  // val gvs = Helper.getLines("data_eco/gpt","\t").map(j => -j(0).toDouble/100) // -data.gpt(i) //-gpt  
  val gv = 0 // gvs(i)
  val gk = g + gv
  // Calculated data for year i
  val v = s / (gk + delta)
  val K = v * pib; val Ke = m / (1 + m) * K; val Kf = 1 / (1 + m) * K
  val p = alpha * pib / data.e(i).to(energy_units) // Prix réel de l'énergie
  val yf = pib - p * data.ce(i).to(energy_units)
  val qf = data.ef(i).to(energy_units) / yf // Intensité énergétique de l'économie
  val vf = Kf / yf // Intensité capitalistique de l'économie
  val ve = Ke / data.ye(i).to(energy_units) // Intensité capitalistique du secteur énergétique
  val tilde_Ke = energy_units(Ke * qf)
  val cf = yf - s * pib
  val c = data.ce(i).to(energy_units) / cf
  val n = p * c //  data.ce(i).to(energy_units) / cf

  val eroi = 1 / (qe + delta_e * ve * qf)

  val L = data.L(i) / pop_units // .toDouble /1E6 //2871.0 * 1E6; // Pop employee (10^6 personnes)
  val r = theta / v;
  val w = (1 - theta) * pib / L;
  val lf = (1 - p * qf - r * vf) / w;
  val le = ((1 - qe) * p - r * ve) / w;
  val Lf = lf * yf;
  val Le = le * data.ye(i).to(energy_units);
  val rho = yf / pib
  val z = Z(ve, vf, qe, qf, le, lf, delta_e, delta_f)

}

object Calibration {
  // import CalibrationData._
  val data = CalibrationData
  val year_double = data.year.map(_.toDouble)

  def index_year(year: Int) = data.year.zipWithIndex.find(i => i._1 == year).get._2

  def rho(alpha: Double, gamma: Double) = 1 - alpha + alpha * gamma

  val cals = data.year.map(y => new calibration_results_work(year = y))
  val (qe, qf, ve, vf, le, lf, v) = (cals.map(_.qe), cals.map(_.qf), cals.map(_.ve), cals.map(_.vf), cals.map(_.le), cals.map(_.lf), cals.map(_.v))
  def growth_rates(ind: List[Double]) = {
    (1 until ind.size).toList.map(i => (1 - ind(i) / ind(i - 1)))
  }
  
  import Helper._
  def printTableCalibration_new(year: Int = 2017, tfs: List[Int], tes: List[Int], alphas: List[Double], ms: List[Double], gpts: List[Double]) {

    val cals = tfs.map(tf => tes.map(te => alphas.map(alpha => ms.map(m => gpts.map(gpt => new calibration_results_work(year, tf, te, alpha, m, gpt))).flatten).flatten).flatten).flatten
    val ref = new calibration_results_work(year = year)

    print("begin{tabular}{c "); cals.map(i => print("c ")); println("}");

    print("$T_f$ [years]&"); print("textbf{"); print(ref.Tf); print("}"); cals.map(cal => print(" & " + cal.Tf)); println(" \\" + "\\")
    print("$T_e$ [years]&"); print("textbf{"); print(ref.Te); print("}"); cals.map(cal => print(" & " + cal.Te)); println(" \\" + "\\")

    print("$delta$ [%]&"); print("textbf{"); print(round(ref.delta * 100, 2)); print("}"); cals.map(cal => print(" & " + round(cal.delta * 100))); println(" \\" + "\\")
    print("$alpha$ [%]&"); print("textbf{"); print(ref.alpha * 100.toInt); print("}"); cals.map(cal => print(" & " + cal.alpha * 100)); println(" \\" + "\\")
    print("$m$ [%]&"); print("textbf{"); print(ref.m * 100); print("}"); cals.map(cal => print(" & " + cal.m * 100)); println(" \\" + "\\")
    // print("$g_{pt}$ [%]&"); print("textbf{"); print(ref.gpt*100); print("}"); cals.map(cal => print(" & " + cal._1._5 * 100)); println(" \\" + "\\")

    print("$v$ & "); print("textbf{"); print(round(ref.v)); print("}"); cals.map(cal => print(" & " + round(cal.v))); println(" \\" + "\\")
    print("$q_f$ & "); print("textbf{"); print(round(ref.qf)); print("}"); cals.map(cal => print(" & " + round(cal.qf))); println(" \\" + "\\")
    print("$v_f$ & "); print("textbf{"); print(round(ref.vf)); print("}"); cals.map(cal => print(" & " + round(cal.vf))); println(" \\" + "\\")
    print("$v_e$ & "); print("textbf{"); print(round(ref.ve)); print("}"); cals.map(cal => print(" & " + round(cal.ve))); println(" \\" + "\\")

    print("n [/100]& "); print("textbf{"); print(round(ref.n * 100)); print("}"); cals.map(cal => print(" & " + round(cal.n * 100))); println(" \\" + "\\")
    println
    print("textbf{"); print(round(100 * (ref.delta * ref.ve * ref.qf))); print("}");
    cals.map(cal => print(" & " + round(round(100 * (cal.delta * cal.ve * cal.qf))))); println(" \\" + "\\")
    println
    print("$epsilon$ & "); print("textbf{"); print(round(ref.eroi)); print("}"); cals.map(cal => print(" & " + round(cal.eroi))); println(" \\" + "\\")

  }
  def printTableCalibration_simple(year: Int = 2017, alphas: List[Double], ms: List[Double]) {

    // val cals = alphas.map(alpha => ms.map(m => new calibration_results_work(year,20,25, alpha,m))).flatten
    val cals = ms.map(m => alphas.map(alpha => new calibration_results_work(year,20,25, alpha,m))).flatten
    
    // val ref = new calibration_results_work(year = year)

    print("begin{tabular}{c "); cals.map(i => print("c ")); println("}");
    print("$alpha$ [%]");
    cals.map(cal => print(" & " + cal.alpha * 100)); println(" \\" + "\\")
    print("$m$ [%]"); 
    cals.map(cal => print(" & " + cal.m * 100)); println(" \\" + "\\")
   
    print("$q_f$"); cals.map(cal => print(" & " + round(cal.qf))); println(" \\" + "\\")
    print("$v_e$"); cals.map(cal => print(" & " + round(cal.ve))); println(" \\" + "\\")

    // print("n [/100]& "); print("textbf{"); print(round(ref.n * 100)); print("}"); cals.map(cal => print(" & " + round(cal.n * 100))); println(" \\" + "\\")
    cals.map(cal => print(" & " + round(round(100 * (cal.delta_e * cal.ve * cal.qf))))); println(" \\" + "\\")
    print("$epsilon$ "); cals.map(cal => print(" & " + round(cal.eroi))); println(" \\" + "\\")

  }
}

object CalibrationData {
  import Helper._
  import PlotHelper._

  val data_folder = "/Users/Elise/Model_Doctorat/WindPotentialScala/data_eco/"

  val smoothing = (false, 1)

  val data = getLines(data_folder + "data_calibration", "\t").map(i => (i(0).toInt, i(1).toDouble, MegaTonOilEquivalent(i(2).toDouble), MegaTonOilEquivalent(i(3).toDouble), MegaTonOilEquivalent(i(4).toDouble), i(5).toDouble / 100, i(6).toDouble, i(7).toDouble / 100))
  val n = data.size
  val ind = (0 until n).toList

  val year = data.map(_._1); val pib = data.map(_._2); val ee = data.map(_._3); val e = data.map(_._4); val ce = data.map(_._5);

  val s = smooth_double(data.map(_._6), smoothing); // val gpt = smooth_double(data.map(_._7), smoothing);
  val PA = data.map(_._7); val ch = data.map(_._8);
  val L = ind.map(i => PA(i) * (1 - ch(i)))
  // Calculs directs
  val ye = ind.map(i => ee(i) + e(i))
  val qe = smooth_double(ind.map(i => ee(i) / ye(i)), smoothing)
  val ef = ind.map(i => e(i) - ce(i))
  val gamma = smooth_double(ind.map(i => ef(i) / e(i)), smoothing)
  val g = smooth_double(List((pib(1) - pib(0)) / pib(0)) ++ (0 until pib.size - 1).map(i => (pib(i + 1) - pib(i)) / pib(i)), smoothing) // On met pour les 2 premières années le même taux de croissance pour ne pas avoir de pb de dimensions
  // (n, ind, year, pib, a, e, ce, s, u, qe, ey, gamma, g, gpt)

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
    val tfc_data = getLines(data_folder + "total_final_consumption", "\t")
    val years = (1 until tfc_data.size).map(i => tfc_data(i)(0).toDouble).toList
    val tfc = (1 until tfc_data(0).size).map(i =>
      (tfc_data(0)(i).toString, (1 until tfc_data.size).map(j => TonOilEquivalent(tfc_data(j)(i).toDouble * 1000)).toList)).toMap
    val own_use_data = getLines(data_folder + "energy_industry_own_use", "\t")
    val own_use = (1 until own_use_data(0).size).map(i =>
      (own_use_data(0)(i).toString, (1 until own_use_data.size).map(j => -TonOilEquivalent(if (own_use_data(j)(i).nonEmpty) own_use_data(j)(i).toDouble * 1000 else 0.0)).toList)).toMap

    val sources = tfc.keys.toList.filter(i => !i.contains("Crude oil"))
    val qe = sources.map(s => (s, (0 until tfc(s).size).map(i => own_use(s)(i) / (own_use(s)(i) + tfc(s)(i))).toList)).toMap
    val qe_total = (0 until tfc(sources(0)).size).map(i => sources.map(s => own_use(s)(i).toJoules).sum / (sources.map(s => own_use(s)(i).toJoules).sum + sources.map(s => tfc(s)(i).toJoules).sum)).toList
    plotXY(qe.map(q => (years, q._2.map(_ * 100), q._1)).toList ++ List((years, qe_total.map(_ * 100), "Total")), yLabel = "qe [%]", legend = true, title = "qe_i")

    val coal_tfc_china = getLines(data_folder + "coal_china", "\t").map(i => TonOilEquivalent(i(2).toDouble * 1000))
    val coal_qe_china = getLines(data_folder + "coal_china", "\t").map(i => -i(1).toDouble / (-i(1).toDouble + i(2).toDouble))

    plotXY(List((years, tfc("Coal").map(_.to(MegaTonOilEquivalent)), "Total"), (years, coal_tfc_china.map(_.to(MegaTonOilEquivalent)), "China")), yLabel = "Coal - TFC [Mtoe]", legend = true)
    plotXY(List((years, qe("Coal").map(_ * 100), "Total"), (years, coal_qe_china.map(_ * 100), "China")), yLabel = "Coal - qe [%]", legend = true)

  }
}