package economic_model
import utils._
import squants.energy._

// Vector of technical parameters

case class Z_xi(ve: Double, vf: Double, qe: Double, qf: Double, xe: Double, xf: Double, deltae: Double, deltaf: Double) {
  override def toString() = { ve + "\t" + vf + "\t" + qe + "\t" + qf + "\t" + xe + "\t" + xf + "\t" + deltae + "\t" + deltaf }
}

class calibration_results_CI(
    val alpha: Double = 0.06, val eta: Double = 4.0 / 100, val zf: Double = 0.5,
    val year: Int = 2017, val Tf: Int = 25, val Te: Int = 25, val mu: Double = 280.0 / 4280,
    val energy_units: EnergyUnit = MegaTonOilEquivalent, val pib_units: Int = 1E9.toInt,
    val pop_units: Int = 1E6.toInt) {

  val i = CalibrationXi.index_year(year)

  val delta_f = 1.0 / Tf; val delta_e = 1.0 / Te;
  val delta = mu * delta_e + (1 - mu) * delta_f
  val data = CalibrationXi.data
  def ce(i: Int) = data.ce(i).to(energy_units)

  // Observed data  for year i
  val pib = data.pib(i) / pib_units;

  val va_e = alpha * pib; val va_f = (1 - alpha) * pib
  val g = data.g(i);
  val s = data.s(i);

  val qe = data.qe(i)
  val gk = g // + gv
  // Calculated data for year i
  val v = s / (gk + delta)
  val K = v * pib; val Ke = mu * K; val Kf = (1 - mu) * K
  val I = s * pib; val C = (1 - s) * pib
  val p_min = va_e / data.e(i).to(energy_units)
  val eta_min = p_min * ce(i) / C
  val p = eta * C / ce(i) //va_e/((1-ze)*data.ye(i).to(energy_units)) //alpha * pib / data.e(i).to(energy_units) // Prix réel de l'énergie
  val gamma = ce(i) / C

  val Cf = (1 - eta) * C
  val gammab = ce(i) / Cf
  val Xe = p * data.e(i).to(energy_units) - va_e
  val xe = Xe / data.ye(i).to(energy_units)
  // val xe = (ze-qe)*p
  val ze = qe + xe / p
  val yf = va_f / (1 - zf)
  val qf = data.ef(i).to(energy_units) / yf // Intensité énergétique de l'économie
  val xf = zf - p * qf
  val Xf = xf * yf
  val vf = Kf / yf // Intensité capitalistique de l'économie
  val ve = Ke / data.ye(i).to(energy_units) // Intensité capitalistique du secteur énergétique
  val tilde_Ke = energy_units(Ke * qf)
  val tilde_Xe = energy_units(Xe * qf)
  def tilde_Ke(qf: Double) = energy_units(Ke * qf)
  def tilde_Xe(qf: Double) = energy_units(Xe * qf)
  val y = yf / data.ye(i).to(energy_units)
  // val cf = yf - s * pib

  val eroi = 1 / (qe + (xe + delta_e * ve) * qf)

  val z = Z_xi(ve, vf, qe, qf, xe, xf, delta_e, delta_f)

  // val ner = (1 - z.qe)*(1 - (xf + z.deltaf * z.vf)) - z.qf*(z.deltae * z.ve + xe)
  val ner = 1 - (z.qe + z.qf * (z.xe + z.deltae * z.ve) + (1 - z.qe) * (z.xf + z.deltaf * z.vf)) //( //(1 - (z.xf + z.deltaf * z.vf)) * (1 - z.qe) - z.qf * (z.xe + z.deltae * z.ve)
  val k = K / data.ye(i).to(energy_units)

  val p1 = 1.0 - ner
  val xi = ce(i) / data.ye(i).to(energy_units)
  val p2 = (1 - (z.xf + z.deltaf * z.vf) - p * z.qf) * xi
  // Pertes %
  val perte1 = 1.0 / eroi
  val perte2 = p1
  val perte3 = p1 + p2

  def printCalib {
    println("Ye" + "\t" + data.ye(i).to(energy_units) + "\n" + "Ee" + "\t" + data.ee(i).to(energy_units) + "\n" + "Ef" + "\t" + data.ef(i).to(energy_units))
    println("pib" + "\t" + pib + "\n" + "pibm" + "\t" + data.pib(i - 1) / pib_units + "\n" + "s" + "\t" + s + "\n" + "mu" + "\t" + mu)
    println("delta" + "\t" + delta + "\n" + "qe" + "\t" + qe + "\n" + "gk" + "\t" + gk + "\n" + "v" + "\t" + v + "\n" + "ve" + "\t" + ve + "\n" + "xe" + "\t" + xe + "\n" + "ze" + "\t" + ze + "\n" + "qf" + "\t" + qf + "\n" + "vf" + "\t" + vf + "\n" + "xf" + "\t" + xf + "\n" + "eps" + "\t" + eroi + "\n" + "gamma" + "\t" + gammab)
    println("K" + "\t" + K + "\n" + "Ke" + "\t" + Ke + "\n" + "Kf" + "\t" + Kf + "\n" + "Xe" + "\t" + Xe + "\n" + "Xf" + "\t" + Xf + "\n" + "Yf" + "\t" + yf + "\n" + "Ce" + "\t" + ce(i) + "\n" + "Cf" + "\t" + Cf + "\n" + "C" + "\t" + C)
    println("I" + "\t" + (s * pib) + "\n" + "VAe" + "\t" + (alpha * pib) +"\n"+ "VAf" + "\t" + ((1 - alpha) * pib) + "\n" + "p" + "\t" + p + "\n" + "k" + "\t" + k + "\n" + "y" + "\t" + y + "\n" + "p1" + "\t" + p1 + "\n" + "p2" + "\t" + p2)
  }
}

object CalibrationXi {
  val data = CalibrationDataXi
  val year_double = data.year.map(_.toDouble)
  // Test: linear extrapolation for alpha & eta

  /*  val a_alpha = (5.3 - 5.0) / (2005 - 2015) / 100.0
  val b_alpha = 5.0 / 100 - a_alpha * 2015
  val a_eta = (4.68 - 4.3) / (2005 - 2015) / 100.0
  val b_eta = 4.3 / 100 - a_eta * 2015
  def alpha(year: Int) = a_alpha * year + b_alpha
  def eta(year: Int) = a_eta * year + b_eta*/

  def index_year(year: Int) = data.year.zipWithIndex.find(i => i._1 == year).get._2

  val cals = data.year.map(y => new calibration_results_CI(year = y, alpha = data.alpha(index_year(y)), eta = data.eta(index_year(y)), zf = data.zf(index_year(y))))
  val (qe, qf, ve, vf, xe, xf, v, eroi, ner, p, perte1, perte2, alphas, etas, zfs) = (cals.map(_.qe), cals.map(_.qf), cals.map(_.ve), cals.map(_.vf), cals.map(_.xe), cals.map(_.xf), cals.map(_.v), cals.map(_.eroi), cals.map(_.ner), cals.map(_.p),
    cals.map(_.perte1), cals.map(_.perte2), cals.map(_.alpha), cals.map(_.eta), cals.map(_.zf))
  def growth_rates(ind: List[Double]) = {
    (1 until ind.size).toList.map(i => (1.0 - ind(i) / ind(i - 1)))
  }

  import Helper._

  def printTableCalibrationCI(year: Int = 2017, alphas: List[Double], etas: List[Double], zfs: List[Double], mus: List[Double]) {
    val cals = alphas.map(alpha => etas.map(eta => (mus.map(mu => zfs.map(zf => new calibration_results_CI(alpha = alpha, eta = eta, zf = zf, mu = mu)))).flatten).flatten).flatten
    print("begin{tabular}{c "); cals.map(i => print("c ")); println("}");
    print("$alpha$ [/100]"); cals.map(cal => print(" & " + round(cal.alpha * 100, 0))); println(" \\" + "\\")
    print("$eta$ [/100]"); cals.map(cal => print(" & " + round(cal.eta * 100, 1))); println(" \\" + "\\")
    print("$zf$ [/100]"); cals.map(cal => print(" & " + round(cal.zf * 100, 0))); println(" \\" + "\\")
    print("$mu$ [/100]"); cals.map(cal => print(" & " + round(cal.mu * 100, 1))); println(" \\" + "\\")
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
}

object CalibrationDataXi {
  import Helper._
  import PlotHelper._

  val data_folder = "../model_data/"
  val data = getLines(data_folder + "data_calibration_xi", "\t").map(i => (i(0).toInt, i(1).toDouble, MegaTonOilEquivalent(i(2).toDouble), MegaTonOilEquivalent(i(3).toDouble), MegaTonOilEquivalent(i(4).toDouble), MegaTonOilEquivalent(i(5).toDouble), i(6).toDouble / 100, i(7).toDouble, i(8).toDouble, i(9).toDouble))
  val n = data.size
  val ind = (0 until n).toList

  val year = data.map(_._1); val pib = data.map(_._2);
  val neu = data.map(_._4); val e_neu = data.map(_._5); val ce = data.map(_._6);
  val oeu = data.map(_._3)
  val frac_neu = ind.map(i => neu(i) / e_neu(i))
  val ee = ind.map(i => oeu(i) * (1 - frac_neu(i)))
  val ye = ind.map(i => e_neu(i) - neu(i) + oeu(i))
  val s = data.map(_._7);
  val eta = data.map(_._8);
  val alpha = data.map(_._9);
  val zf = data.map(_._10);
  val qe = ind.map(i => ee(i) / ye(i))
  val ef = ind.map(i => ye(i) - ee(i) - ce(i))
  val e = ind.map(i => ye(i) - ee(i))
  val gamma = ind.map(i => ef(i) / e(i))
  val g = List((pib(1) - pib(0)) / pib(0)) ++ (0 until pib.size - 1).map(i => (pib(i + 1) - pib(i)) / pib(i))
  val qtot = ind.map(i => ye(i) / pib(i))
}
