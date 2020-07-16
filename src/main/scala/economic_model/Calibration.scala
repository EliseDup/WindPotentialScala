package economic_model
import utils._
import squants.energy._

// Vector of technical parameters
object Z {
  def apply(l: List[Double]) = new Z(l(0), l(1), l(2), l(3), l(4), l(5), l(6), l(7))
}
case class Z(ve: Double, vf: Double, qe: Double, qf: Double, le: Double, lf: Double, deltae: Double, deltaf: Double) {
  override def toString() = { ve + "\t" + vf + "\t" + qe + "\t" + qf + "\t" + le + "\t" + lf + "\t" + deltae + "\t" + deltaf }
}

class ImpactPER(val z: Z) {
  // Pertes / PIN
  val eroi = 1 / (z.qe + z.deltae * z.ve * z.qf)
  val ner = ((1 - z.qe) * (1 - z.deltaf * z.vf) - (-z.qf * -z.deltae * z.ve))

  // Link between m = Ke/Kf, k = K/Ye, & mu = ve/k = Ke/K
  def m_mu(mu: Double) = mu / (1 - mu)
  def mu_m(m: Double) = m / (1 + m)
  def k_m(m: Double) = z.ve * (m + 1) / m
  def k_mu(mu: Double) = z.ve / mu

  def mean_std(a: (Double, Double)) = ((a._1 + a._2) / 2, math.abs(a._1 - (a._1 + a._2) / 2))

  // Conditions d'existence: intervalle admissible pour phi = 1/p
  val phi1 = z.vf * (1 - z.qe) / z.ve + z.qf; val phi2 = z.lf * (1 - z.qe) / z.le + z.qf;
  val interval_phi = (phi1, phi2)
  val phi = mean_std(interval_phi)
  val interval_p = (1.0 / phi2, 1.0 / phi1)
  val p = mean_std(interval_p)

  // Exercice 1 : n & s fixed
  def interval_mu_gk(s: Double, n: Double) = {
    val n_p = (1 - s) * n / (1 + s * n)
    val a = (1 + n_p) * z.ve / z.vf * z.qf / (1 - z.qe)
    val a1 = n_p + a; val a2 = n_p * z.ve / z.vf * z.lf / z.le + a
    (a1 / (1 + a1), a2 / (1 + a2))
  }
  def mu_gk(s: Double, n: Double) = mean_std(interval_mu_gk(s, n))
  def k_gk(s: Double, n: Double) = mean_std(k_mu(interval_mu_gk(s, n)._1), k_mu(interval_mu_gk(s, n)._2))

  def gk_mu(mu: Double, s: Double, n: Double) = {
    1 / z.vf * (1 + n) * s / (1 + s * n) - z.deltaf + (z.deltaf - z.deltae - 1 / z.vf * (1 + n) * s / (1 + s * n)) * mu
  }
  // "Old" calculation with m
  def interval_m_gk(s: Double, n: Double) = {
    val n_p = (1 - s) * n / (1 + s * n)
    val a = (1 + n_p) * z.qf / (1 - z.qe)
    (n_p + z.ve / z.vf * a, z.ve / z.vf * (n_p * z.lf / z.le + a))
  }
  def gk_m(m: Double, s: Double, n: Double) = {
    1 / (1 + m) * (1 / z.vf * (1 + n) * s / (1 + s * n) - m * z.deltae - z.deltaf)
  }

  def interval_gk(s: Double, n: Double) = {
    val (mu1, mu2) = interval_mu_gk(s, n)
    (gk_mu(mu1, s, n), gk_mu(mu2, s, n))
  }
  def gk(s: Double, n: Double) = mean_std(interval_gk(s, n))
  def delta_m(m: Double) = 1.0 / (1 + m) * (m * z.deltae + z.deltaf)
  def delta_mu(mu: Double) = mu * z.deltae + (1 - mu) * z.deltaf

  def interval_delta_gk(s: Double, n: Double) = {
    val (mu1, mu2) = interval_mu_gk(s, n)
    (delta_m(mu1), delta_m(mu2))
  }
  // Exercice 2 : gk & n fixed
  def interval_m_s(gk: Double, n: Double) = {
    val m_inf = (phi2 * n * (1 - z.vf * (gk + z.deltaf)) + z.qf) / (z.vf / z.ve * (1 - z.qe) + phi2 * n * z.vf * (gk + z.deltae))
    val m_sup = (phi1 * n * (1 - z.vf * (gk + z.deltaf)) + z.qf) / (z.vf / z.ve * (1 - z.qe) + phi1 * n * z.vf * (gk + z.deltae))
    (List(m_inf, m_sup).min, List(m_inf, m_sup).max)
  }
  def interval_mu_s(gk: Double, n: Double) = {
    def mu_(phi: Double) = (phi * n * (1 - z.vf * (gk + z.deltaf)) + z.qf) / (z.vf / z.ve * (1 - z.qe) + phi * n * (1 - z.vf * (z.deltaf - z.deltae)) + z.qf)
    (mu_(phi1), mu_(phi2))
  }
  def mu_s(gk: Double, n: Double) = mean_std(interval_mu_s(gk, n))
  def k_s(gk: Double, n: Double) = mean_std(k_mu(interval_mu_s(gk, n)._1), k_mu(interval_mu_s(gk, n)._2))
  def s_s(gk: Double, n: Double) = mean_std(interval_s(gk, n))

  // v calculation
  // v = s/(gk + delta)
  def v(s: Double, gk: Double, mu: Double) = s / (gk + delta_mu(mu))
  // Evolution de la consommation total Cf + pCe
  // Puisque Ye est constant : C/Ye = (Cf+pCe)/Ye = (1-s)k/v
  def c_tot(s: Double, gk: Double, mu: Double) = (1 - s) * k_mu(mu) / v(s, gk, mu)
  def c_tot_gk(s: Double, n: Double) = {
    val mu = interval_mu_gk(s, n);
    mean_std(c_tot(s, gk_mu(mu._1, s, n), mu._1), c_tot(s, gk_mu(mu._2, s, n), mu._2))
  }
  def c_tot_s(gk: Double, n: Double) = {
    val mu = interval_mu_s(gk, n);
    mean_std(c_tot(s_mu(mu._1, gk, n), gk, mu._1), c_tot(s_mu(mu._2, gk, n), gk, mu._2))
  }
  def c_tot_c(gk: Double, c: Double) = {
    val mu = mu_c(gk, c);
    val s = interval_s_c(gk, c)
    mean_std(c_tot(s._1, gk, mu), c_tot(s._2, gk, mu))
  }
  def v_gk(s: Double, n: Double) = {
    val mu = interval_mu_gk(s, n);
    val v1 = v(s, gk_mu(mu._1, s, n), mu._1)
    val v2 = v(s, gk_mu(mu._2, s, n), mu._2)
    mean_std(v1, v2)
  }
  def v_s(gk: Double, n: Double) = {
    val mu = interval_mu_s(gk, n)
    val v1 = v(s_mu(mu._1, gk, n), gk, mu._1)
    val v2 = v(s_mu(mu._2, gk, n), gk, mu._2)
    mean_std(v1, v2)
  }
  def v_c(gk: Double, c: Double) = {
    val mu = mu_c(gk, c)
    val s = interval_s_c(gk, c)
    val v1 = v(s._1, gk, mu)
    val v2 = v(s._2, gk, mu)
    mean_std(v1, v2)
  }
  // Link between s and n : (1+s)*n/(1+s*n) = f(m,z)
  def f_m(m: Double, gk: Double) = z.vf * (m * (gk + z.deltae) + gk + z.deltaf)
  def f_mu(mu: Double, gk: Double) = z.vf * (gk + z.deltaf - mu * (z.deltaf - z.deltae)) / (1 - mu)

  def s_m(m: Double, gk: Double, n: Double) = {
    f_m(m, gk) / (1 + n - n * f_m(m, gk))
  }
  def s_mu(mu: Double, gk: Double, n: Double) = {
    f_mu(mu, gk) / (1 + n - n * f_mu(mu, gk))
  }
  def interval_s_m(gk: Double, n: Double) = {
    val m = interval_m_s(gk, n)
    (s_m(m._1, gk, n), s_m(m._2, gk, n))
  }
  def interval_s(gk: Double, n: Double) = {
    val mu = interval_mu_s(gk, n)
    (s_mu(mu._1, gk, n), s_mu(mu._2, gk, n))
  }

  // Exercice 3: gk & c = Ce/Cf fixed (impact on n & s)
  def n_c(c: Double) = mean_std(interval_n_c(c))
  def s_c(gk: Double, c: Double) = mean_std(interval_s_c(gk, c))
  def m_c(gk: Double, c: Double) = {
    (c * (1 / z.vf - (gk + z.deltaf)) + z.qf / z.vf) / ((1 - z.qe) / z.ve + c * (gk + z.deltae))
  }
  def mu_c(gk: Double, c: Double) = {
    (c * (1 - z.vf * (gk + z.deltaf)) + z.qf) / (z.vf / z.ve * (1 - z.qe) + c * (1 - z.vf * (z.deltaf - z.deltae)) + z.qf)
  }

  def interval_n_c(c: Double) = (c / phi2, c / phi1)
  def interval_s_c(gk: Double, c: Double) = {
    val fmu = f_mu(mu_c(gk, c), gk)
    (fmu / (1 + c / phi1 - c / phi1 * fmu), fmu / (1 + c / phi2 - c / phi2 * fmu))
  }
}

class calibration_results_CI(
    val alpha: Double = 0.05, val eta: Double = 4.3 / 100, val zf: Double = 0.49,
    val year: Int = 2017, val Tf: Int = 20, val Te: Int = 25, val mu: Double = 280.0 / 4280, val theta: Double = 0.4,
    val energy_units: EnergyUnit = MegaTonOilEquivalent, val pib_units: Int = 1E9.toInt,
    val pop_units: Int = 1E6.toInt) {
  val i = Calibration.index_year(year)

  val delta_f = 1.0 / Tf; val delta_e = 1.0 / Te;
  val delta = mu * delta_e + (1 - mu) * delta_f
  val data = Calibration.data
  // Observed data  for year i
  val pib = data.pib(i) / pib_units;
  val va_e = alpha * pib; val va_f = (1 - alpha) * pib
  val g = data.g(i); val s = data.s(i);
  val qe = data.qe(i)
  val gk = g // + gv
  // Calculated data for year i
  val v = s / (gk + delta)
  val K = v * pib; val Ke = mu * K; val Kf = (1 - mu) * K
  val I = s * pib; val C = (1 - s) * pib
  val p_min = va_e / data.e(i).to(energy_units)
  val eta_min = p_min * data.ce(i).to(energy_units) / C
  val p = eta * C / data.ce(i).to(energy_units) //va_e/((1-ze)*data.ye(i).to(energy_units)) //alpha * pib / data.e(i).to(energy_units) // Prix réel de l'énergie

  val Cf = (1 - eta) * C
  val Xe = p * data.e(i).to(energy_units) - va_e
  val xe = Xe / data.ye(i).to(energy_units)

  val yf = va_f / (1 - zf)
  val qf = data.ef(i).to(energy_units) / yf // Intensité énergétique de l'économie
  val xf = zf - p * qf
  val vf = Kf / yf // Intensité capitalistique de l'économie
  val ve = Ke / data.ye(i).to(energy_units) // Intensité capitalistique du secteur énergétique
  val tilde_Ke = energy_units(Ke * qf)
  def tilde_Ke(qf: Double) = energy_units(Ke * qf)
  // val cf = yf - s * pib

  val eroi = 1 / (qe + (xe + delta_e * ve) * qf)
 
  val L = data.L(i) / pop_units // .toDouble /1E6 //2871.0 * 1E6; // Pop employee (10^6 personnes)
  val r = theta / v;
  val w = (1 - theta) * pib / L;
  val lf = (1 - p * qf - r * vf) / w;
  val le = ((1 - qe) * p - r * ve) / w;
  val Lf = lf * yf;
  val Le = le * data.ye(i).to(energy_units);
  val rho = yf / pib
  val z = Z(ve, vf, qe, qf, le, lf, delta_e, delta_f)

  // val ner = (1 - z.qe)*(1 - (xf + z.deltaf * z.vf)) - z.qf*(z.deltae * z.ve + xe)
  val ner = (1 - (xf + z.deltaf * z.vf)) * (1 - z.qe) - z.qf * (xe + z.deltae * z.ve)
  val k = K / data.ye(i).to(energy_units)

  val p1 = 1.0 - ner
  val xi = data.ce(i).to(energy_units) / data.ye(i).to(energy_units)
  val p2 = (1 - (xf + z.deltaf * z.vf) - p * z.qf) * xi
  // Pertes %
  val perte1 = 1.0 / eroi
  val perte2 = p1
  val perte3 = p1 + p2
}

class calibration_results_work(val year: Int = 2017, val Tf: Int = 20, val Te: Int = 25, val alpha: Double = 6.0 / 100, val mu: Double = 280.0 / 4280, val theta: Double = 0.4,
    val energy_units: EnergyUnit = MegaTonOilEquivalent, val pib_units: Int = 1E9.toInt,
    val pop_units: Int = 1E6.toInt) {

  val i = Calibration.index_year(year)
  val delta_f = 1.0 / Tf; val delta_e = 1.0 / Te;
  val delta = mu * delta_e + (1 - mu) * delta_f
  val data = Calibration.data
  // Observed data  for year i
  val pib = data.pib(i) / pib_units;
  val g = data.g(i); val s = data.s(i);
  val qe = data.qe(i)
  // val gvs = Helper.getLines("data_eco/gpt","\t").map(j => -j(0).toDouble/100) // -data.gpt(i) //-gpt  
  //val gv = 0 // gvs(i)
  val gk = g // + gv
  // Calculated data for year i
  val v = s / (gk + delta)
  val K = v * pib; val Ke = mu * K; val Kf = (1 - mu) * K
  val p = alpha * pib / data.e(i).to(energy_units) // Prix réel de l'énergie
  val yf = pib - p * data.ce(i).to(energy_units)

  val qf = data.ef(i).to(energy_units) / yf // Intensité énergétique de l'économie
  val vf = Kf / yf // Intensité capitalistique de l'économie
  val ve = Ke / data.ye(i).to(energy_units) // Intensité capitalistique du secteur énergétique
  val tilde_Ke = energy_units(Ke * qf)
  def tilde_Ke(qf: Double) = energy_units(Ke * qf)
  val cf = yf - s * pib
  val c = data.ce(i).to(energy_units) / cf
  val n = p * c //  data.ce(i).to(energy_units) / cf
  val ctot = cf + p * data.ce(i).to(energy_units)
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

  val ner = ((1 - z.qe) * (1 - z.deltaf * z.vf) - (-z.qf * -z.deltae * z.ve))
  val k = K / data.ye(i).to(energy_units)

  val detA = ((1 - z.qe) * (1 - z.deltaf * z.vf) - (-z.qf * -z.deltae * z.ve))
  val p1 = 1.0 - detA
  val xi = data.ce(i).to(energy_units) / data.ye(i).to(energy_units)
  val p2 = (1 - z.deltaf * z.vf - p * z.qf) * xi
  // Pertes %
  val perte1 = 1.0 / eroi
  val perte2 = p1
  val perte3 = p1 + p2
}

object Calibration {
  val data = CalibrationData
  val year_double = data.year.map(_.toDouble)

  def index_year(year: Int) = data.year.zipWithIndex.find(i => i._1 == year).get._2

  def rho(alpha: Double, gamma: Double) = 1 - alpha + alpha * gamma

  val cals = data.year.map(y => new calibration_results_CI(year = y))
  val (qe, qf, ve, vf, xe, xf, le, lf, v, eroi, ner, p, perte1, perte2) = (cals.map(_.qe), cals.map(_.qf), cals.map(_.ve), cals.map(_.vf),cals.map(_.xe), cals.map(_.xf), cals.map(_.le), cals.map(_.lf), cals.map(_.v), cals.map(_.eroi), cals.map(_.ner), cals.map(_.p),
    cals.map(_.perte1), cals.map(_.perte2))
  def growth_rates(ind: List[Double]) = {
    (1 until ind.size).toList.map(i => (1 - ind(i) / ind(i - 1)))
  }

  import Helper._
  def printTableCalibration_simple(year: Int = 2017, alphas: List[Double], mus: List[Double]) {
    val cals = mus.map(mu => alphas.map(alpha => new calibration_results_work(year, 20, 25, alpha, mu))).flatten
    print("begin{tabular}{c "); cals.map(i => print("c ")); println("}");
    print("$alpha$ [/100]"); cals.map(cal => print(" & " + cal.alpha * 100)); println(" \\" + "\\")
    print("$mu$ [/100]"); cals.map(cal => print(" & " + cal.mu * 100)); println(" \\" + "\\")
    print("$q_f$"); cals.map(cal => print(" & " + round(cal.qf, 4))); println(" \\" + "\\")
    print("$v$"); cals.map(cal => print(" & " + round(cal.v))); println(" \\" + "\\")
    print("$v_f$"); cals.map(cal => print(" & " + round(cal.vf))); println(" \\" + "\\")
    print("$v_e$"); cals.map(cal => print(" & " + round(cal.ve))); println(" \\" + "\\")
    print("$q_f delta_e v_e$ [/100]"); cals.map(cal => print(" & " + round(round(100 * (cal.delta_e * cal.ve * cal.qf))))); println(" \\" + "\\")
    print("$epsilon$ "); cals.map(cal => print(" & " + round(cal.eroi))); println(" \\" + "\\")
    print("$NER$ "); cals.map(cal => print(" & " + round(cal.ner * 100))); println(" \\" + "\\")
    print("P_1=CIK SE+CIK SF"); cals.map(cal => print(" & " + round((1.0 - cal.ner) * 100))); println(" \\" + "\\")
    print("CIK SE"); cals.map(cal => print(" & " + round(1.0 / cal.eroi * 100))); println(" \\" + "\\")
    print("CIK SF"); cals.map(cal => print(" & " + round((1.0 - cal.ner - 1.0 / cal.eroi) * 100))); println(" \\" + "\\")
    print("$P_2$ = Pertes d'$ub$ - $pC_e$"); cals.map(cal => print(" & " + round(cal.p2 * 100))); println(" \\" + "\\")
    print("=Pertes d'$ub$"); cals.map(cal => print(" & " + round(cal.xi * (1 - cal.delta_f * cal.vf) * 100))); println(" \\" + "\\")
    print("-Compensation $pC_e$"); cals.map(cal => print(" & " + round((cal.xi * cal.p * cal.qf) * 100))); println(" \\" + "\\")
    print("$P_1$ + $P_2$"); cals.map(cal => print(" & " + round((cal.p1 + cal.p2) * 100))); println(" \\" + "\\")

  }
  def printTableCalibrationCI(year: Int = 2017, alphas: List[Double], etas: List[Double], zfs: List[Double], mus: List[Double]) {
    val cals = alphas.map(alpha => etas.map(eta => (mus.map(mu => zfs.map(zf => new calibration_results_CI(alpha = alpha, eta = eta, zf = zf, mu = mu)))).flatten).flatten).flatten
    print("begin{tabular}{c "); cals.map(i => print("c ")); println("}");
    print("$alpha$ [/100]"); cals.map(cal => print(" & " + round(cal.alpha * 100, 0))); println(" \\" + "\\")
    print("$eta$ [/100]"); cals.map(cal => print(" & " + round(cal.eta * 100,1))); println(" \\" + "\\")
    print("$zf$ [/100]"); cals.map(cal => print(" & " + round(cal.zf * 100,0))); println(" \\" + "\\")
    print("$mu$ [/100]"); cals.map(cal => print(" & " + round(cal.mu * 100,1))); println(" \\" + "\\")
    print("$p$"); cals.map(cal => print(" & " + round(cal.p))); println(" \\" + "\\")
    print("$q_f$"); cals.map(cal => print(" & " + round(cal.qf))); println(" \\" + "\\")
    print("$x_f$"); cals.map(cal => print(" & " + round(cal.xf))); println(" \\" + "\\")
    print("$x_e$"); cals.map(cal => print(" & " + round(cal.xe))); println(" \\" + "\\")
    print("$v$"); cals.map(cal => print(" & " + round(cal.v))); println(" \\" + "\\")
    print("$v_f$"); cals.map(cal => print(" & " + round(cal.vf))); println(" \\" + "\\")
    print("$v_e$"); cals.map(cal => print(" & " + round(cal.ve))); println(" \\" + "\\")
    print("$q_f (x_e + delta_e v_e)$ [/100]"); cals.map(cal => print(" & " + round(round(100 * ((cal.xe + cal.delta_e * cal.ve) * cal.qf))))); println(" \\" + "\\")
    print("$EROI$ "); cals.map(cal => print(" & " + round(cal.eroi))); println(" \\" + "\\")
    print("$NER$ "); cals.map(cal => print(" & " + round(cal.ner * 100))); println(" \\" + "\\")
    print("P_1=CIK SE+CIK SF"); cals.map(cal => print(" & " + round((1.0 - cal.ner) * 100))); println(" \\" + "\\")
    print("CIK SE"); cals.map(cal => print(" & " + round(1.0 / cal.eroi * 100))); println(" \\" + "\\")
    print("CIK SF"); cals.map(cal => print(" & " + round((1.0 - cal.ner - 1.0 / cal.eroi) * 100))); println(" \\" + "\\")
    print("$P_2$ = Pertes d'$ub$ - $pC_e$"); cals.map(cal => print(" & " + round(cal.p2 * 100))); println(" \\" + "\\")
    print("=Pertes d'$ub$"); cals.map(cal => print(" & " + round(cal.xi * (1 - cal.delta_f * cal.vf) * 100))); println(" \\" + "\\")
    print("-Compensation $pC_e$"); cals.map(cal => print(" & " + round((cal.xi * cal.p * cal.qf) * 100))); println(" \\" + "\\")
    print("$P_1$ + $P_2$"); cals.map(cal => print(" & " + round((cal.p1 + cal.p2) * 100))); println(" \\" + "\\")

  }
  def printTableCalibration_full(year: Int = 2017, tfs: List[Int], tes: List[Int], alphas: List[Double], mus: List[Double]) {

    val cals = tfs.map(tf => tes.map(te => alphas.map(alpha => mus.map(mu => new calibration_results_work(year, tf, te, alpha, mu))).flatten).flatten).flatten
    val ref = new calibration_results_work(year = year)

    print("begin{tabular}{c "); cals.map(i => print("c ")); println("}");

    print("$T_f$ [years]&"); print("textbf{"); print(ref.Tf); print("}"); cals.map(cal => print(" & " + cal.Tf)); println(" \\" + "\\")
    print("$T_e$ [years]&"); print("textbf{"); print(ref.Te); print("}"); cals.map(cal => print(" & " + cal.Te)); println(" \\" + "\\")

    print("$delta$ [%]&"); print("textbf{"); print(round(ref.delta * 100, 2)); print("}"); cals.map(cal => print(" & " + round(cal.delta * 100))); println(" \\" + "\\")
    print("$alpha$ [%]&"); print("textbf{"); print(ref.alpha * 100.toInt); print("}"); cals.map(cal => print(" & " + cal.alpha * 100)); println(" \\" + "\\")
    print("$mu$ [%]&"); print("textbf{"); print(ref.mu * 100); print("}"); cals.map(cal => print(" & " + cal.mu * 100)); println(" \\" + "\\")

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
}

object ModelResolution {
  def apply(c: calibration_results_work): ModelResolution = new ModelResolution(c.z, c.data.ye(c.i), c.energy_units, c.L, c.s, c.n)
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
  val detA = ((1 - z.qe) * (1 - z.deltaf * z.vf) - (-z.qf * -z.deltae * z.ve))
  val p1 = 1.0 - detA
  val PIN1 = ye.to(energy_units) / z.qf
  val PIN2 = PIN1 * (1 - 1.0 / eroi)
  val PIN3 = PIN1 * (1 - p1)
  val xi = ce / ye
  val p2 = (1 - z.deltaf * z.vf - p * z.qf) * xi
  val PIN4 = PIN1 * (1 - p1 - p2)
  val ner = detA
  // Pertes %
  val perte1 = 1.0 / eroi
  val perte2 = p1
  val perte3 = p1 + p2
}
object CalibrationData {
  import Helper._
  import PlotHelper._

  val data_folder = "/Users/Elise/Model_Doctorat/WindPotentialScala/data_eco/"

  val smoothing = (false, 1)

  val data = getLines(data_folder + "data_calibration", "\t").map(i => (i(0).toInt, i(1).toDouble, MegaTonOilEquivalent(i(2).toDouble), MegaTonOilEquivalent(i(3).toDouble), MegaTonOilEquivalent(i(4).toDouble), MegaTonOilEquivalent(i(5).toDouble), i(6).toDouble / 100, i(7).toDouble, i(8).toDouble / 100))
  val n = data.size
  val ind = (0 until n).toList

  val year = data.map(_._1); val pib = data.map(_._2);
  val neu = data.map(_._4); val e_neu = data.map(_._5); val ce = data.map(_._6);
  val oeu = data.map(_._3)
  val frac_neu = ind.map(i => neu(i) / e_neu(i))

  // val e = ind.map(i => e_neu(i) - neu(i))
  val ee = ind.map(i => oeu(i) * (1 - frac_neu(i)))
  val ye = ind.map(i => e_neu(i) - neu(i) + oeu(i))

  val s = smooth_double(data.map(_._7), smoothing); // val gpt = smooth_double(data.map(_._7), smoothing);
  val PA = data.map(_._8); val ch = data.map(_._9);
  val L = ind.map(i => PA(i) * (1 - ch(i)))
  // Calculs directs

  val qe = smooth_double(ind.map(i => ee(i) / ye(i)), smoothing)
  val ef = ind.map(i => ye(i) - ee(i) - ce(i))
  val e = ind.map(i => ye(i) - ee(i))
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

    val sources = tfc.keys.toList.filter(i => !i.contains("Crude oil") && !i.contains("Heat") && !i.contains("Wind")
      && !i.contains("brut") && !i.contains("Chaleur") && !i.contains("Eolien"))

    val qe = sources.map(s => (s, (0 until tfc(s).size).map(i => own_use(s)(i) / (own_use(s)(i) + tfc(s)(i))).toList)).toMap
    val qe_total = (0 until tfc(sources(0)).size).map(i => sources.map(s => own_use(s)(i).toJoules).sum / (sources.map(s => own_use(s)(i).toJoules).sum + sources.map(s => tfc(s)(i).toJoules).sum)).toList
    plotXY(qe.map(q => (years, q._2.map(_ * 100), q._1)).toList ++ List((years, qe_total.map(_ * 100), "Total")), yLabel = "qe [%]", legend = true, title = "qe_i")

    val coal_tfc_china = getLines(data_folder + "coal_china", "\t").map(i => TonOilEquivalent(i(2).toDouble * 1000))
    val coal_qe_china = getLines(data_folder + "coal_china", "\t").map(i => -i(1).toDouble / (-i(1).toDouble + i(2).toDouble))

    plotXY(List((years, tfc("Coal").map(_.to(MegaTonOilEquivalent)), "Total"), (years, coal_tfc_china.map(_.to(MegaTonOilEquivalent)), "China")), yLabel = "Coal - TFC [Mtoe]", legend = true)
    plotXY(List((years, qe("Coal").map(_ * 100), "Total"), (years, coal_qe_china.map(_ * 100), "China")), yLabel = "Coal - qe [%]", legend = true)

  }

  def alpha_ze_zf(pib_units: Int): (Double, Double, Double) = {
    // Calcul de alpha
    val ci_va_y = Helper.getLines("CI_VA_Y", "\t").map(i => (i(0).toString, (i(1).toDouble / pib_units, i(2).toDouble / pib_units, i(3).toDouble / pib_units), (i(4).toDouble / pib_units, i(5).toDouble / pib_units, i(6).toDouble / pib_units)))
    val total_e = (ci_va_y.map(_._2._1).sum, ci_va_y.map(_._2._2).sum, ci_va_y.map(_._2._3).sum)
    val total_f = (ci_va_y.map(_._3._1).sum, ci_va_y.map(_._3._2).sum, ci_va_y.map(_._3._3).sum)

    // = CIe / pYe
    val ze = total_e._1 / total_e._3
    // = CIf / Yf
    val zf = total_f._1 / total_f._3
    // VAe / (VAe+Vaf)
    val alpha = 0.06 ///  total_e._2 / (total_e._2 + total_f._2)
    (alpha, ze, zf)
  }
}