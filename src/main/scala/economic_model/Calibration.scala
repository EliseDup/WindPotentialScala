package economic_model
import utils._
import squants.energy._

// gpt = -gv
class calibration_results_work(val year: Int = 2017, val Tf: Int = 20, val Te: Int = 25, val alpha: Double = 6.0 / 100, val m: Double = 6.5 / 100, val gpt: Double = 0.0, val theta: Double = 0.4, val energy_units: EnergyUnit = KilowattHours, val pib_units: Int = 1) {
  val i = Calibration.index_year(year)
  val delta_f = 1.0 / Tf; val delta_e = 1.0 / Te;
  val delta = (m * delta_e + delta_f) / (1 + m)

  val data = Calibration.data
  val pib = data.pib(i) / pib_units;
  val g = data.g(i); val s = 0.25 //data.s(i); 
  val qe = data.qe(i)
  val gv = -gpt
  val gk = g + gv
  val v = s / (gk + delta)
  val K = v * pib; val Ke = m / (1 + m) * K; val Kf = 1 / (1 + m) * K
  val p = alpha * pib / data.e(i).to(energy_units) // Prix réel de l'énergie
  val yf = pib - p * data.ce(i).to(energy_units)
  val qf = data.ef(i).to(energy_units) / yf // Intensité énergétique de l'économie
  val vf = Kf / yf // Intensité capitalistique de l'économie
  val ve = Ke / data.ye(i).to(energy_units) // Intensité capitalistique du secteur énergétique

  val cf = yf - s * pib
  val n = p * data.ce(i).to(energy_units) / cf

  val eroi = 1 / (qe + delta * ve * qf)
  // println(year + "\t" + eroi + "\t" + qf + "\t" + vf + "\t" + data.qe(i) + "\t" + ve + "\t" + v + "\t" + k / data.ye(i).to(energy_units))
  // val L=3422; % Pop active (10^6 personnes) 
  val L = data.L(i) // .toDouble /1E6 //2871.0 * 1E6; // Pop employee (10^6 personnes)

  val r = theta / v;
  val w = (1 - theta) * pib / L;
  val lf = (1 - p * qf - r * vf) / w;
  val le = ((1 - qe) * p - r * ve) / w;
  val Lf = lf * yf;
  val Le = le * data.ye(i).to(energy_units);
  val rho = yf / pib

  val k = K / data.ye(i).to(energy_units);
  val (ki, ks) = interval_k(s, n, ve, vf, qe, qf, le, lf)
  val (gki, gks) = interval_gk(s, n, ve, vf, qe, qf, le, lf)
  val (gi, gs) = (gki + gpt, gks + gpt)

  // Conditions d'existence: intervalle admissible pour k = K/U
  def interval_k(s: Double, n: Double, ve: Double, vf: Double, qe: Double, qf: Double, le: Double, lf: Double) = {
    // n'
    val m = (1 - s) * n / (1 + s * n)
    val k1 = m * lf / le + (1 + m) * qf / (1 - qe)
    val k2 = m * vf / ve + (1 + m) * qf / (1 - qe)
    val bounds = List(ve + vf / k2, ve + vf / k1)
    (bounds.min, bounds.max)
  }

  // Conditions d'existence: intervalle admissible pour le taux de croissance du capital gk
  def interval_gk(s: Double, n: Double, ve: Double, vf: Double, qe: Double, qf: Double, le: Double, lf: Double) = {
    val (ki, ks) = interval_k(s, n, ve, vf, qe, qf, le, lf)
    val sy = s * (1 + n) / (1 + s * n); // I/Y
    val x = 1 / vf * sy
    val a = x - delta_f
    val b = (delta_f - delta_e - x)
    (a + b * ve / ki, a + b * ve / ks)
  }

  def interval_m(ki: Double, ks: Double, ve: Double) = {
    (ve / (ki - ve), ve / (ks - ve))
  }
  def interval_delta(ki: Double, ks: Double, ve: Double) = {
    (delta_f - (delta_f - delta_e) * ve / ki, delta_f - (delta_f - delta_e) * ve / ks)
  }
}

object Calibration {
  // import CalibrationData._
  val data = CalibrationData
  val year_double = data.year.map(_.toDouble)

  def index_year(year: Int) = data.year.zipWithIndex.find(i => i._1 == year).get._2

  def rho(alpha: Double, gamma: Double) = 1 - alpha + alpha * gamma

  val cals = data.year.map(y => new calibration_results_work(year = y))
  val (qe, qf, ve, vf, le, lf) = (cals.map(_.qe), cals.map(_.qf), cals.map(_.ve), cals.map(_.vf), cals.map(_.le), cals.map(_.lf))
  def growth_rates(ind: List[Double]) = {
    (1 until ind.size).toList.map(i => (1 - ind(i) / ind(i - 1)))
  }

  import Helper._
  def printTableCalibration_new(year: Int = 2017, tfs: List[Int], alphas: List[Double], ms: List[Double], gpts: List[Double]) {

    val cals = tfs.map(tf => alphas.map(alpha => ms.map(m => gpts.map(gpt => new calibration_results_work(year, tf, 25, alpha, m, gpt))).flatten).flatten).flatten
    val ref = new calibration_results_work(year = year)

    print("begin{tabular}{c "); cals.map(i => print("c ")); println("}");

    print("$T_f$ [years]&"); print("textbf{"); print(ref.Tf); print("}"); cals.map(cal => print(" & " + cal.Tf)); println(" \\" + "\\")

    print("$delta$ [%]&"); print("textbf{"); print(round(ref.delta * 100, 2)); print("}"); cals.map(cal => print(" & " + round(cal.delta * 100))); println(" \\" + "\\")
    print("$alpha$ [%]&"); print("textbf{"); print(ref.alpha * 100.toInt); print("}"); cals.map(cal => print(" & " + cal.alpha * 100)); println(" \\" + "\\")
    print("$m$ [%]&"); print("textbf{"); print(ref.m * 100); print("}"); cals.map(cal => print(" & " + cal.m * 100)); println(" \\" + "\\")
    // print("$g_{pt}$ [%]&"); print("textbf{"); print(ref.gpt*100); print("}"); cals.map(cal => print(" & " + cal._1._5 * 100)); println(" \\" + "\\")

    print("$v$ & "); print("textbf{"); print(round(ref.v)); print("}"); cals.map(cal => print(" & " + round(cal.v))); println(" \\" + "\\")
    print("$q_f$ & "); print("textbf{"); print(round(ref.qf)); print("}"); cals.map(cal => print(" & " + round(cal.qf))); println(" \\" + "\\")
    print("$v_f$ & "); print("textbf{"); print(round(ref.vf)); print("}"); cals.map(cal => print(" & " + round(cal.vf))); println(" \\" + "\\")
    print("$v_e$ & "); print("textbf{"); print(round(ref.ve)); print("}"); cals.map(cal => print(" & " + round(cal.ve))); println(" \\" + "\\")

    print("n [%]& "); print("textbf{"); print(round(ref.n * 100)); print("}"); cals.map(cal => print(" & " + round(cal.n * 100))); println(" \\" + "\\")
    println
    print("textbf{"); print(round(100 * (ref.delta * ref.ve * ref.qf))); print("}");
    cals.map(cal => print(" & " + round(round(100 * (cal.delta * cal.ve * cal.qf))))); println(" \\" + "\\")
    println
    print("$epsilon$ & "); print("textbf{"); print(round(ref.eroi)); print("}"); cals.map(cal => print(" & " + round(cal.eroi))); println(" \\" + "\\")

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