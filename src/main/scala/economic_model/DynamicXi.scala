package economic_model

import wind_energy._
import solar_energy._
import utils._
import squants.energy._
import squants.time.Hours
import java.io.FileOutputStream
import scala.collection.mutable.ArrayBuffer

object DynamicXi {

  import CalibrationDataXi._
  import PlotHelper._
  import Helper._

  val calib = new calibration_results_CI()
  val dyn_1 = new Dynamic_s_eta(calib.s, calib.eta)
  val dyn_2b = new Dynamic_s_gamma_b(calib.s, calib.gammab)
  val dyn_3 = new Dynamic_gk_eta(calib.gk, calib.eta)

  val ye_0 = calib.data.ye(calib.i).to(calib.energy_units)
  val qf_0 = calib.qf; val qf_f = calib.qf * 0.5; val ye_f = 3 * ye_0
  // Based on history : Ye +2% / year, qf - 0.7 % / year

  val t_lim = 2040 - 2017
  val bau = new Scenario(new ParamsScenario(qf_0, qf_0 * 0.85, qf_f, t_lim), new ParamsScenario(ye_0, ye_0 * math.pow(1 + 1.62433159 / 100, t_lim), ye_f, t_lim), "BAU")
  val new_policies = new Scenario(new ParamsScenario(qf_0, qf_0 * 0.58 * 0.85 / 0.64, qf_f, t_lim), new ParamsScenario(ye_0, 12581, ye_f, t_lim), "New Policies")
  val sustainable_dev = new Scenario(new ParamsScenario(qf_0, qf_0 * 0.45 * 0.85 / 0.64, qf_f, t_lim), new ParamsScenario(ye_0, ye_0, ye_0, t_lim), "Sustainable Dev")
  printScenario(bau, "BAU"); printScenario(new_policies, "NP"); printScenario(sustainable_dev, "SD")

  def printScenario(scn: Scenario, name: String) {
    println(name + "\t" + scn.ye.r1 + "\t" + scn.ye.r2 + "\t" + scn.qf.r1 + "\t" + scn.qf.r2 + "\t" + scn.ye.r_old + "\t" + scn.qf.r_old)
  }

  /*
  val t = (0 until 100).toList
  plotXY(List(
    (t.map(i => (i + 2017).toDouble), t.map(i => bau.ye_t(i)), "BAU"),
    (t.map(i => (i + 2017).toDouble), t.map(i => new_policies.ye_t(i)), "New Policies"),
    (t.map(i => (i + 2017).toDouble), t.map(i => sustainable_dev.ye_t(i)), "Sustainable Dev")), yLabel = "Ye [Mtoe]", legend = true, title = "Ye_scenarios")
  plotXY(List(
    (t.map(i => (i + 2017).toDouble), t.map(i => bau.qf_t(i)), "BAU"),
    (t.map(i => (i + 2017).toDouble), t.map(i => new_policies.qf_t(i)), "New Policies"),
    (t.map(i => (i + 2017).toDouble), t.map(i => sustainable_dev.qf_t(i)), "Sustainable Dev")), yLabel = "qf [toe/kUS$2010]", legend = true, title = "qf_scenarios")
  plotXY(List(
    (t.map(i => (i + 2017).toDouble), t.map(i => bau.qf.old_x(i)), "BAU"),
    (t.map(i => (i + 2017).toDouble), t.map(i => new_policies.qf.old_x(i)), "New Policies"),
    (t.map(i => (i + 2017).toDouble), t.map(i => sustainable_dev.qf.old_x(i)), "Sustainable Dev")), yLabel = "qf [toe/kUS$2010]", legend = true, title = "qf_scenarios_old")
*/

  def main(args: Array[String]): Unit = {
    printTable(dyn_1, 50)
    printTable(dyn_2b, 50)
    printTable(dyn_3, 50)
  }
  def plotScenarios(dyn: Dynamic_Params, ny: Int) {
    val res = List((dyn.simulate_int(calib, bau, ny, false), "BAU"),
      (dyn.simulate_int(calib, new_policies, ny, false), "NP"),
      (dyn.simulate_int(calib, sustainable_dev, ny, false), "SD"))

    def years_pib(res: DynamicResult) = (1 until res.years.size).toList.map(res.years(_).toDouble)
    def years(res: DynamicResult) = res.years.map(_.toDouble)
    val gk_list = res.map(i => (years(i._1), i._1.gk.map(_ * 100), i._2))
    plotXY(gk_list, legend = true, yLabel = "Capital Growth rate [%]", title = "gk")
    val gpib_list = res.map(i => (years_pib(i._1), i._1.model.g_pib.map(_ * 100), i._2))
    plotXY(gpib_list, legend = true, yLabel = "PIB Growth rate [%]", title = "gpib")
    val x_list = res.map(i => (years_pib(i._1), i._1.x.map(_ * 100), i._2))
    plotXY(x_list, legend = true, yLabel = "Renewable Penetration [%]", title = "x")
    val mu_list = res.map(i => (years_pib(i._1), i._1.model.mu.toList.map(_ * 100), i._2))
    plotXY(mu_list, legend = true, yLabel = "Relative Size of the energy sector [%]", title = "mu")
    val s_list = res.map(i => (years_pib(i._1), i._1.model.mu.toList.map(_ * 100), i._2))
    plotXY(mu_list, legend = true, yLabel = "Relative Size of the energy sector [%]", title = "mu")

    // Verif !
    val ye_list = res.map(i => (years_pib(i._1), i._1.ye.toList.map(_ * 100), i._2))
    plotXY(ye_list, legend = true, yLabel = "Ye [Mtoe]", title = "ye")
    val qf_list = res.map(i => (years_pib(i._1), i._1.z.toList.map(_.qf), i._2))
    plotXY(qf_list, legend = true, yLabel = "qf [toe/kUS$]", title = "qf")
  }

  def printTablesScenarios(nys: List[Int]) {
    nys.map(ny => {
      println("BAU Scenario after " + ny + " years ")
      printTable(bau, ny)
      println("New policies Scenario after " + ny + " years ")
      printTable(new_policies, ny)
      println("Sustainable development Scenario after " + ny + " years ")
      printTable(sustainable_dev, ny)
    })
  }

  def printTable(scenario: Scenario, ny: Int) {
    val res = List(
      ("Ex1", dyn_1.simulate_int(calib, scenario, nyears = ny, plot = false)),
      ("Ex2b", dyn_2b.simulate_int(calib, scenario, nyears = ny, plot = false)),
      ("Ex3", dyn_3.simulate_int(calib, scenario, nyears = ny, plot = false)))
    println("--Results--")
    println("\t" + "x" + "\t" + "k" + "\t" + "gk" + "\t" + "s" + "\t" + "eroi" + "\t" + "theta" + "\t" + "#years_transition")
    res.map(r => println(r._1 + "\t" + r._2))
  }
  def printTable(dyn: Dynamic_Params, ny: Int) {
    val res = List(
      ("BAU", dyn.simulate_int(calib, bau, nyears = ny, plot = false)),
      ("NP", dyn.simulate_int(calib, new_policies, nyears = ny, plot = false)),
      ("SD", dyn.simulate_int(calib, sustainable_dev, nyears = ny, plot = false)))
    println("--Results--")
    println("\t" + "x" + "\t" + "gk" + "\t" + "gpib" + "\t" + "mu" + "\t" + "alpha")
    res.map(r => println(r._1 + " & " + round(r._2.x.last * 100, 2) + " & " + round(r._2.gk.last * 100, 2) + " & " + round(r._2.model.g_pib(r._2.model.g_pib.size - 1) * 100, 2) + " & " +
      round(r._2.model.mu.last * 100, 2) + " & " + round(r._2.model.alpha.last * 100, 2) + "\\" + "\\"))
    println("If ex 3 :")
    res.map(r => println(r._1 + " & " + round(r._2.x.last * 100, 2) + " & " +
      round(r._2.s.max * 100, 2) + " & " +
      round(r._2.model.mu.last * 100, 2) + " & " + round(r._2.model.alpha.last * 100, 2) + "\\" + "\\"))

  }

  def qf_detailed_results(step_qf: Int, scenario: Scenario) {
    val qfs = (0 until step_qf).toList.map(i => {
      val ratio_qf = (1 - i / step_qf.toDouble)
      val ratio_qt = scenario.qf.xt / scenario.qf.x0
      // Si la nouvelle limite est plus petite que la valeur en 2040, on hausse la valeur en 2040 pour dire qu'on a fait 1/3 du chemin d'ici 2040.
      val ratio_qt_corr = 1 - 1.0 / 3 * (1 - ratio_qf) // else ratio_qt
      new Scenario(new ParamsScenario(scenario.qf.x0, scenario.qf.x0 * ratio_qt_corr, scenario.qf.x0 * ratio_qf, scenario.qf.t), scenario.ye)
    })
    // val t = (0 until 100).toList
    // val list = qfs.map(qf => (t.map(_.toDouble), t.map(qf.qf_t(_)), (qf.qf.xf / qf.qf.x0).toString))
    // plotXY(list, legend = true)

    //  detailedResults(qfs, dyn_1, calib, scenario.name + "1", false)
    //   detailedResults(qfs, dyn_2b, calib, scenario.name + "2b", false)
    detailedResults(qfs, dyn_3, calib, scenario.name + "3", false)
  }

  def detailedResults(qfs: List[Scenario], dyn: Dynamic_Params, calib: calibration_results_CI, label: String, max: Boolean) {
    val out = new java.io.PrintStream(new java.io.FileOutputStream("res_" + label))
    val res = qfs.map(qf => {
      val r = dyn.simulate_int(calib, qf, 500, plot = false, max = max)
      out.print(qf.qf.xf + "\t" + r.x.last + "\t" + r.k.last + "\t" + r.gk.last + "\t" + r.end_year.getOrElse(0.0) + "\t" + r.eroi.last + "\t" + r.s.toList.max + "\t" + r.model.mu.last + "\t" + r.model.alpha.last + "\n")
      r
    })
    out.close()
    println("--- Exercice " + label + " Ended --- ")
    //val qf_rel = qfs.map(q => q.qf.xf / calib.qf)
    //plotXY(List((qf_rel, res.map(r => r.x(r.last)), "")), xLabel = "qf_min/qf_0", yLabel = "x", title = "qf_min_x_" + label + max)
    //plotXY(List((qf_max, res.map(_.k), "")), xLabel = "qf_min", yLabel = "k", title = "qf_min_k_" + label + max)
    //plotXY(List((qf_max, res.map(_.gk), "")), xLabel = "qf_min", yLabel = "gk", title = "qf_min_gk_" + label + max)
    //plotXY(List((qf_rel, res.map(r => r.s.max), "")), xLabel = "qf_min/qf_0", yLabel = "s_max", title = "qf_min_s_max_" + label + max)
    //plotXY(List((qf_rel, res.map(_.end_year).map(_.getOrElse(0).toDouble), "")), xLabel = "qf_min/qf_0", yLabel = "years to finish transition", title = "qf_min_y_" + label + max)
  }

  def plotYeCurve {
    val lines = getLines("potential_reduced", "\t")
    val lines2 = getLines("potential_reduced_avg", "\t")

    val ye = lines.map(_(0).toDouble)
    val xe = lines.map(_(1).toDouble)
    val ke = lines.map(_(2).toDouble)
    val ye2 = lines2.map(_(0).toDouble)
    val xe2 = lines2.map(_(1).toDouble)
    val ke2 = lines2.map(_(2).toDouble)
    val is = (1 until ye.size).toList
    val is2 = (1 until ye2.size).toList
    val ye_i = is.map(i => ye(i))
    val ye_i2 = is2.map(i => ye2(i))

    val cout_i = is.map(i => xe(i) + ke(i) / 25.0)
    val cout_i2 = is2.map(i => xe2(i) + ke2(i) / 25.0)
    val delta_xe = is.map(i => (xe(i) - xe(i - 1)) / (ye(i) - ye(i - 1)))
    val delta_ke = is.map(i => (ke(i) - ke(i - 1)) / (ye(i) - ye(i - 1)))
    plotXY(List((ye_i, cout_i, "1 point sur 20"), (ye_i2, cout_i2, "moyenne sur 20 points")), legend = true, xLabel = "Ye", yLabel = "Coûts Totaux")

    val cout_margi = (1 until cout_i.size).toList.map(i => (cout_i(i) - cout_i(i - 1)) / (ye_i(i) - ye_i(i - 1)))
    val cout_margi2 = (1 until cout_i2.size).toList.map(i => (cout_i2(i) - cout_i2(i - 1)) / (ye_i2(i) - ye_i2(i - 1)))

    plotXY(List(((1 until cout_i.size).toList.map(ye_i(_)), cout_margi, "1 point sur 20"), ((1 until cout_i2.size).toList.map(ye_i2(_)), cout_margi2, "moyenne sur 20 points")), legend = true, xLabel = "Ye", yLabel = "Coûts Marginaux")
  }
}

class Scenario(val qf: ParamsScenario, val ye: ParamsScenario, val name: String = "") {
  def qf_t(T: Int) = qf.old_x(T)
  def ye_t(T: Int) = ye.old_x(T)
}

class ParamsScenario(val x0: Double, val xt: Double, val xf: Double, val t: Int) {
  val r1 = if (x0 == xt) 0.0
  else math.pow(xt / x0, 1.0 / t) - 1
  val r2 = if (x0 == xf) 0.0
  else 1 - (xt * (1 + r1) - xf) / (xt - xf)
  def x(T: Int) = {
    if (T < t) x0 * math.pow(1 + r1, T)
    else xf + (xt - xf) * Math.pow(1 - r2, (T - t))
  }
  val r_old = if (x0 == xf) 0.0 else 1 - math.pow((xt - xf) / (x0 - xf), 1.0 / t)
  def old_x(T: Int) = {
    xf + (x0 - xf) * Math.pow(1 - r_old, T)
  }
  override def toString() = "x0 = " + x0 + " , xt = " + xt + " , xf = " + xf + ", rate before " + (2017 + t) + " = " + r1 + " , rate after " + r2
}

case class DynamicResult(val years: List[Int], val x: List[Double], val k: List[Double], val gk: List[Double], val s: List[Double], val eroi: List[Double], val ye: List[Double], val z: List[Z_xi], val theta: Double, val end_year: Option[Int],
    val model: Model) { //val mu: List[Double], val p_ratio: List[Double], val ce_ratio: List[Double], val cf_ratio: List[Double]) {
  val last = x.size - 1
  override def toString() = x(last) + "\t" + k(last) + "\t" + gk(last) + "\t" + s(last) + "\t" + eroi(last) + "\t" + theta + "\t" + end_year + "\t" + model.mu(last) // + "\t" + p_ratio(last) + "\t" + ce_ratio(last) + "\t" + cf_ratio(last)
}

class Model(calib: calibration_results_CI, params: Dynamic_Params) {
  val C, Ce, Df, Cf, Ke, Kf, Yf, pib, I, mu, p, eta, gamma, alpha, VAe, VAf = scala.collection.mutable.ArrayBuffer.empty[Double];
  C += calib.C; Ce += calib.ce(calib.i); Cf += calib.Cf; Df += (calib.yf - calib.Xe - calib.Xf); Ke += calib.Ke; Kf += calib.Kf; Yf += calib.yf; pib += calib.pib;
  mu += calib.mu; p += calib.p; eta += calib.eta; gamma += calib.gammab;
  alpha += calib.alpha; VAe += calib.va_e; VAf += calib.va_f

  def update(k: Double, K: Double, z: Z_xi, s: Double, ye: Double, gK: Double) {
    Ke += ye * z.ve; Kf += K - Ke.last; Yf += Kf.last / calib.vf;
    Ce += ye * (1 - z.qe) - z.qf * Yf.last; mu += Ke.last / K;
    Df += Yf.last * (1 - z.xf) - ye * z.xe
    // delta += (z.deltae * Ke.last + z.deltaf * Kf.last) / K
    p += params.p(this)
    Cf += C.last - p.last * Ce.last
    eta += p.last * Ce.last / C.last; gamma += Ce.last / Cf.last
    pib += (gK + z.delta) * K / params.s_k(k, z); C += (1 - s) * pib.last; I += s * pib.last
    VAe += (p.last * (1 - z.qe) - z.xe) * ye
    VAf += (1 - z.xf - p.last * z.qf) * Yf.last
    alpha += VAe.last / pib.last
  }

  def g_pib = (1 until pib.toList.size).toList.map(i => (pib(i) - pib(i - 1)) / pib(i - 1))
  def pCe = (0 until p.toList.size).toList.map(i => p.toList(i) * Ce.toList(i))
}

case class Interval(val min: Double, val max: Double, val beta: Double = 0.5) {
  val mean = (1 - beta) * min + beta * max
  override def toString() = mean + "," + min + "," + max
  // assert(min < max, "Interval with min > max " + toString())
}
// Exercice 1 : s et eta constants
class Dynamic_s_eta(s: Double, eta: Double) extends Dynamic_Params(s, eta) {
  override def toString() = "Ex1"
  def y_int(z: Z_xi) = {
    val eta_prim = eta * (1 - s) / (1 - eta * (1 - s))
    val y1 = (A(z) * (1 - z.qe) + eta_prim * z.xe) / (A(z) * z.qf + eta_prim * (1 - z.xf)) // (1 + eta_prim) * z.xe / (z.xe * z.qf / (1 - z.qe) + eta_prim * (1 - z.xf))
    val y2 = (B(z) * (1 - z.qe) + eta_prim * z.xe) / (B(z) * z.qf + eta_prim * (1 - z.xf)) // ((1 - z.xf) * (1 - z.qe) / z.qf + eta_prim * z.xe) / ((1 + eta_prim) * (1 - z.xf))
    Interval(y1, y2)
  }
  def ratio_spib_K(k: Double, z: Z_xi) = {
    (1 / z.vf * s / (1 - eta * (1 - s))) * ((1 - z.xf) * (1 - z.ve / k) - z.xe * z.vf / k)
  }
  def p(model: Model) = eta * model.C.last / model.Ce.last
}

// Gamma = Ce/Cf constant
class Dynamic_s_gamma_b(s: Double, gamma: Double) extends Dynamic_Params(s, gamma) {
  override def toString() = "Ex2b"
  def y_int(z: Z_xi) = {
    val A1 = s / (1.0 - s) * (A(z) + 1.0 / (gamma * s))
    val B1 = s / (1.0 - s) * (B(z) + 1.0 / (gamma * s))
    val y1 = (A1 * (1 - z.qe) + z.xe) / (1 - z.xf + A1 * z.qf)
    val y2 = (B1 * (1 - z.qe) + z.xe) / (1 - z.xf + B1 * z.qf)
    Interval(y1, y2)
  }
  def ratio_spib_K(k: Double, z: Z_xi) = {
    (1 - z.xf + z.qf / gamma) / z.vf - ((1 - z.xf + z.qf / gamma) * z.ve / z.vf + z.xe + (1 - z.qe) / gamma) / k
  }
  // vp=vDy./vCe/ss-sg;
  def p(model: Model) = (1 - s) / s * model.Df.last / model.Ce.last - 1 / (s * gamma)
}

class Dynamic_gk_eta(gk: Double, eta: Double) extends Dynamic_Params(gk, eta) {
  override def toString() = "Ex3"
  def y_int(z: Z_xi) = {
    val (k1, k2) = k_bounds(z)
    Interval((k1 - z.ve) / z.vf, (k2 - z.ve) / z.vf)
  }
  override def k_bounds(z: Z_xi): (Double, Double) = {
    val A1 = (1 - eta) / eta * A(z)
    val B1 = (1 - eta) / eta * B(z)
    def coef(a: Double) = (a * (1 - z.qe + z.qf * z.ve / z.vf) + (1 - z.xf) * z.ve / z.vf + z.xe) / (a * z.qf / z.vf + (1 - z.xf) / z.vf - (gk + z.delta))
    (coef(A1), coef(B1))
  }

  def ratio_spib_K(k: Double, z: Z_xi) = gk + z.delta
  override def s_k(k: Double, z: Z_xi) = (1 - eta) * (gk + z.delta) / ((1 - z.xf) / z.vf - eta * (gk + z.delta) - ((1 - z.xf) / z.vf * z.ve + z.xe) / k)
  def p(model: Model) = eta * model.C.last / model.Ce.last
}

abstract class Dynamic_Params(val param1: Double, val param2: Double) {
  import Helper._
  import PlotHelper._
  import GrowthModel._
  def beta(a0: Double, a1: Double, a2: Double) = (a0 - a1) / (a2 - a1)
  // Intervalles sur y = Ye/Yf
  def y_int(z: Z_xi): Interval
  // sPIB/K (in order to calculate gk = sPIB/K-delta  
  def ratio_spib_K(k: Double, z: Z_xi): Double
  def p(model: Model): Double
  def s_k(k: Double, z: Z_xi) = param1

  def k_bounds(z: Z_xi): (Double, Double) = {
    val y = y_int(z)
    val k1 = z.vf * y.min + z.ve
    val k2 = z.vf * y.max + z.ve
    (k1, k2)
  }
  def k_int(z: Z_xi, beta: Double): Interval = {
    val bounds = k_bounds(z)
    Interval(bounds._1, bounds._2, beta)
  }
  def gk_int(z: Z_xi, beta: Double) = {
    val g_list = List(gK_k(k_int(z, beta).min, z), gK_k(k_int(z, beta).max, z))
    Interval(g_list.min, g_list.max)
  }
  def gK_k(k: Double, z: Z_xi): Double = ratio_spib_K(k, z) - z.delta

  /*def x_fun = {
    val lines = getLines("x_qe_xe_vefinal", "\t")
    (lines.map(i => (i(0).toDouble)),
      lines.map(i => (i(1).toDouble)),
      lines.map(i => (i(2).toDouble)),
      lines.map(i => (i(3).toDouble)))
  }*/

  def ye_fun(ye: Double) = {
    val lines = getLines("potential_reduced", "\t").filter(i => (i(0).toDouble <= ye))
    (lines.map(i => (i(0).toDouble)),
      lines.map(i => (i(1).toDouble)),
      lines.map(i => (i(2).toDouble)),
      lines.map(i => (i(3).toDouble)))
  }

  // def x_fun_ye(ye: Double) = GrowthModel.x_qe_xe_ve(MegaTonOilEquivalent(ye))
  def x_fun_ye(ye: Double, z0: Z_xi) = {
    val (ye_re, tilde_xe, tilde_ke, delta_e) = ye_fun(ye)
    val res = (0 until ye_re.size).toList.map(i => {
      val x = ye_re(i) / ye
      val ve = if (x == 0) z0.ve else x * tilde_ke(i) / (z0.qf * ye_re(i)) + (1 - x) * z0.ve
      val qe = if (x == 0) z0.qe else (1 - x) * z0.qe
      val xe = if (x == 0) z0.xe else x * tilde_xe(i) / (z0.qf * ye_re(i)) + (1 - x) * z0.xe
      val de = if (x == 0) z0.deltae else x * delta_e(i) + (1 - x) * z0.deltae
      (x, qe, xe, ve, de)
    })
    (res.map(_._1), res.map(_._2), res.map(_._3), res.map(_._4), res.map(_._5))
  }
  def A(z: Z_xi) = z.we / (1 - z.qe)
  def B(z: Z_xi) = (1 - z.wf) / z.qf;
  def C(z: Z_xi, k: Double) = {
    val y = (k - z.ve) / z.vf
    ((1 - z.xf) * y - z.xe) / (1 - z.qe - z.qf * y)
  }
  def qf_fun(qf_0: Double, qf_max: Double, qf_rate: Double, T: Int) = {
    qf_max + (qf_0 - qf_max) * Math.pow(1 - qf_rate, T)
  }

  def p_int(z: Z_xi) = Interval(A(z), B(z))
  def eroi_z(z: Z_xi) = 1 / (z.qe + (z.deltae * z.ve + z.xe) * z.qf)
  def ner_z(z: Z_xi) = 1 - (z.qe + z.qf * (z.xe + z.deltae * z.ve) + (1 - z.qe) * (z.xf + z.deltaf * z.vf)) //( //(1 - (z.xf + z.deltaf * z.vf)) * (1 - z.qe) - z.qf * (z.xe + z.deltae * z.ve)

  def find_indexes(k: Double, k_fun: List[(Double, Interval)], max: Boolean): (Int, Int) = {
    val index = {
      if (k < mean(k_fun(0)._2, max)) 0
      else if (k > mean(k_fun(k_fun.size - 1)._2, max)) k_fun.size - 1
      else (0 until k_fun.size - 1).toList.find(i => mean(k_fun(i)._2, max) <= k && mean(k_fun(i + 1)._2, max) >= k).get
    }
    (index, math.min(k_fun.size - 1, index + 1))
  }
  // y = ax + b => a = (y2-y1)/(x2-x1); b = y1-a*x1; y* = a x* + b
  def interpolation(x: Double, x1: (Double, Double), x2: (Double, Double)) = {
    if (x2._1 == x1._1) x1._2
    else {
      val a = (x2._2 - x1._2) / (x2._1 - x1._1)
      val b = x1._2 - a * x1._1
      a * x + b
    }
  }
  def find_index(k: Double, k_fun: List[(Double, Interval)], max: Boolean): Int = {
    val res = {
      if (k < mean(k_fun(0)._2, max)) 0
      else if (k > mean(k_fun(k_fun.size - 1)._2, max)) k_fun.size - 1
      else (0 until k_fun.size - 1).toList.find(i => mean(k_fun(i)._2, max) <= k && mean(k_fun(i + 1)._2, max) >= k).get
    }
    res
  }
  def find_x(x: Double, k_fun: List[(Double, Interval)]): Int = {
    if (x < k_fun(0)._1) 0
    else if (x > k_fun(k_fun.size - 1)._1) k_fun.size - 1
    else (0 until k_fun.size - 1).toList.find(i => k_fun(i)._1 <= x && k_fun(i + 1)._1 >= x).get
  }
  def find_index_list(x: Double, x_x: List[Double]) = {
    if (x < x_x(0)) 0
    else if (x > x_x(x_x.size - 1)) x_x.size - 1
    else (0 until x_x.size - 1).toList.find(i => x_x(i) <= x && x_x(i + 1) >= x).get
  }

  // Function that for a given x and qf gives de corresponding k
  /*def k_x(qf: Double, z0: Z_xi, beta: Double = 0.5): List[(Double, Interval)] = {
    val (x_x, qe_x, xe_x, ve_x) = x_fun
    (0 until x_x.size).toList.map(i => {
      val z = new Z_xi(ve_x(i), z0.vf, qe_x(i), qf, xe_x(i), z0.xf, z0.deltae, z0.deltaf)
      (x_x(i), k_int(z, beta))
    })
  }*/
  def k_x_ye(qf: Double, z0: Z_xi, beta: Double = 0.5, ye: Double, mu: Double): List[(Double, Interval)] = {
    val (x_x, qe_x, xe_x, ve_x, deltae_x) = x_fun_ye(ye, z0)
    (0 until x_x.size).toList.map(i => {
      val z = new Z_xi(ve_x(i), z0.vf, qe_x(i), qf, xe_x(i), z0.xf, deltae_x(i), z0.deltaf, mu * deltae_x(i) + (1 - mu) * z0.deltaf)
      (x_x(i), k_int(z, beta))
    })
  }

  def mean(int: Interval, max: Boolean) = if (max) int.max else int.mean

  def simulate_int(calib: calibration_results_CI, scenario: Scenario, nyears: Int = 100, plot: Boolean = true, max: Boolean = false): DynamicResult = {
    val z0 = calib.z; val beta_k = if (max) 1 else (calib.k - k_bounds(z0)._1) / (k_bounds(z0)._2 - k_bounds(z0)._1)
    val k_interval = scala.collection.mutable.ArrayBuffer.empty[Interval];
    val gK, s, K, k, x, qf, eroi, ner, ye = scala.collection.mutable.ArrayBuffer.empty[Double];
    val z = scala.collection.mutable.ArrayBuffer.empty[Z_xi];
    z += z0; x += 0.0; qf += z0.qf; s += calib.s; eroi += calib.eroi; ner += calib.ner;

    val ye0 = calib.data.ye(calib.i).to(calib.energy_units)
    ye += ye0

    k_interval += k_int(z0, beta_k); k += mean(k_int(z0, beta_k), max)
    // Le K de début de simulation ne correspond pas au K de la calibration de le cas où on se place sur la courbe max ! Il faut le faire correspondre avec k_max !
    K += k.last * ye.last
    gK += gK_k(k.last, z0);

    // Variables de résultats
    val model = new Model(calib, this)
    // println("Initialize " + k.last + " " + gk.last + " " + beta_k)
    val years = (1 until nyears).map(i => i + 2017).toList
    var end = false; var endYear: Option[Int] = None; var out_interval = false; var year_out: Option[Int] = None;
    for (y <- years) {

      if (!end && k.last >= mean(k_interval.last, max) && x.last > 0.9) {
        println("Transition stops after " + (y - 2017) + " years" + "(ie in " + y + ")")
        endYear = Some(y - 2017)
        end = true

        out_interval = true
        year_out = Some(y - 2017)
      }
      /* if (!out_interval && end && k.last > mean(k_interval.last, true)) {
        println("Out interval after " + (y - 2017) + " years" + "(ie in " + y + ")")
        out_interval = true
        year_out = Some(y - 2017)
      }*/

      if (!end || (end && (y - 2017) == endYear.getOrElse(0))) {
        // vqy=qys+(qy-qys)*(1-.01).^(0:T-1)
        qf += scenario.qf_t(y - 2017) // qf_fun(z0.qf, qf_max, qf_rate, y - 2017) //qf_max + (qf.last - qf_max) * (1 - qf_rate)
        ye += scenario.ye_t(y - 2017) //qf_fun(ye0, ye_max, ye_rate, y - 2017)

        K += K.last * (1 + gK.last)
        k += K.last / ye.last
        // Transition is over, z and x are not changing anymore

        if (end) {
          z += z.last
          x += 1 // x.last
          // println("Continue during year " + y + "\t" + k.last + "\t" + k_interval.last)
        } else {
          // Mapping between x and the interval for k
          val k_x_fun = k_x_ye(qf.last, z0, beta_k, ye.last, model.mu.last)
          val (x_x, qe_x, xe_x, ve_x, deltae_x) = x_fun_ye(ye.last, z0)
          val indexes_x = find_indexes(k.last, k_x_fun, max)
          val k1 = mean(k_x_fun(indexes_x._1)._2, max); val k2 = mean(k_x_fun(indexes_x._2)._2, max);
          val x1 = k_x_fun(indexes_x._1)._1; val x2 = k_x_fun(indexes_x._2)._1
          val newX = math.max(0, interpolation(k.last, (k1, x1), (k2, x2)))

          x += newX
          val ve = interpolation(x.last, (x1, ve_x(indexes_x._1)), (x2, ve_x(indexes_x._2)))
          val qe = interpolation(x.last, (x1, qe_x(indexes_x._1)), (x2, qe_x(indexes_x._2)))
          val xe = interpolation(x.last, (x1, xe_x(indexes_x._1)), (x2, xe_x(indexes_x._2)))
          val deltae = interpolation(x.last, (x1, deltae_x(indexes_x._1)), (x2, deltae_x(indexes_x._2)))
          val mu = ye.last * ve / K.last
          z += Z_xi(ve, z0.vf, qe, qf.last, xe, z0.xf, deltae, z0.deltaf, mu * deltae + (1 - mu) * z0.deltaf)
        }

        k_interval += k_int(z.last, beta_k);
        eroi += eroi_z(z.last)
        ner += ner_z(z.last)
        if (z.last.xe / (1 - z.last.xf) > (1 - z.last.qe) / z.last.qf) println("Constraint on y violated" + "\t" + z.last)
        // Calculated model parameters
        // !! We need the new delta before calculating gK
        gK += gK_k(k.last, z.last);
        s += s_k(k.last, z.last)
        model.update(k.last, K.last, z.last, s.last, ye.last, gK.last)

        // println(y + "\t" + ye.last + "\t" + qf.last + "\t" + x.last + "\t" + k.last + "\t" + mean(k_interval.last, max) + "\t" + gk.last + "\t" + s.last + "\t" + eroi.last + "\t" + beta_k + "\t" + model.mu.last + "\t" + model.eta.last + "\t" + model.gamma.last + "\t" + model.p.last / calib.p + "\t" + "\t" + model.Ce.last / calib.ce(calib.i) + "\t" + model.Cf.last / calib.Cf)
      }
    }
    //println("x" + "\t" + "k" + "\t" + "gk" + "\t" + "s" + "\t" + "eroi" + "\t" + "theta" + "\t" + "mu" + "\t" + "eta" + "\t" + "gamma" + "\t" + "p/p0")
    //println(x.last + "\t" + mean(k_interval.last, max) + "\t" + gk.last + "\t" + s.last + "\t" + eroi.last + "\t" + beta_k + "\t" + model.mu.last + "\t" + model.eta.last + "\t" + model.gamma.last + "\t" + model.p.last / calib.p + "\t" + "\t" + model.Ce.last / calib.ce(calib.i) + "\t" + model.Cf.last / calib.Cf)

    //println("x" + "\t" + "k" + "\t" + "mu" + "\t" + "alpha")
    //println(x.last + "\t" + mean(k_interval.last, max) + "\t" + model.mu.last + "\t" + model.alpha.last) //+ "\t"+ model.alpha.last)
    val years_new = if (!out_interval) years else (2017 until year_out.get + 2017).toList

    if (plot) {
      //  plotXY(List((years.map(_.toDouble), model.pib.toList, "pib"), (years.map(_.toDouble), model.I.toList, "I"),
      //     (years.map(_.toDouble), model.C.toList, "C")), yLabel = "", title = "pib_c_i", legend = true, int_x_axis = true)
      // plotXY(List((years_new.map(_.toDouble), model.C.toList, "C"), (years_new.map(_.toDouble), model.pCe, "pCe"),
      //   (years_new.map(_.toDouble), model.Cf.toList, "Cf")), yLabel = "", title = "c", legend = true, int_x_axis = true)
      //plotXY(x.toList, s.toList)
      plot_(years_new, x, "x_" + toString());
      plot_(years_new, gK, "gk_" + toString());
      //plot_int(years, k, "k_" + toString());
      println(ye.size)
      plot_(years_new, ye, "ye_" + toString());
      plot_(years_new, model.mu, "mu_" + toString())
      plot_(years_new, model.alpha, "alpha_" + toString())
      //plot_(years_new, model.p, "p_" + toString())
      //plot_(years, model.gamma, "gamma_" + toString())
      // plot_(years, model.eta, "eta_" + toString())
      //plot_(years, s, "s_" + toString())
      // Plot interval and k mean   

      // plotXY(List((x.toList, k.toList, "k"), (x.toList, k_interval.toList.map(_.mean), "k_env"), (x.toList, k_interval.toList.map(_.min), "k_env" + "_min"), (x.toList, k_interval.toList.map(_.max), "k_env" + "_max")), yLabel = "k", title = "k", legend = true, int_x_axis = false)

    }
    DynamicResult(years_new.map(_.toInt), x.toList, k_interval.toList.map(k => mean(k, max)), gK.toList, s.toList, eroi.toList, ye.toList, z.toList, beta_k, endYear, model) // model.mu.toList, model.p.toList.map(i=> i / calib.p), model.Ce.toList.map(i => i / calib.ce(calib.i)), model.Cf.toList.map(i => i / calib.Cf))
  }
  def round(x: Double, dec: Int = 0) = {
    math.round(x * math.pow(10, dec)) / math.pow(10, dec)
  }
  def plot_list(years: List[Int], x: List[(String, ArrayBuffer[Double])], title: String) {
    val list = x.map(i => (years.map(_.toDouble), i._2.toList, i._1))
    plotXY(list, yLabel = title, title = title, int_x_axis = true, legend = true)
  }
  def plot_(years: List[Int], x: ArrayBuffer[Double], title: String) {
    plotXY(List((years.map(_.toDouble), x.toList, "")), yLabel = title, title = title, int_x_axis = true)
  }
  def plot_int(years: List[Int], x: ArrayBuffer[Interval], title: String) {
    plotXY(List((years.map(_.toDouble), x.toList.map(_.mean), title), (years.map(_.toDouble), x.toList.map(_.min), title + "_min"), (years.map(_.toDouble), x.toList.map(_.max), title + "_max")), yLabel = title, title = title, legend = true, int_x_axis = true)
  }

  /*  def simulate_x_rate(calib: calibration_results_CI, eta: Double, nyears: Int) {
    def gk_(k: Double, z: Z_xi, s: Double, delta: Double) = {
      1 / z.vf * s / (1 - eta * (1 - s)) * (1 - z.xf - ((1 - z.xf) * z.ve + z.xe * z.vf) / k) - delta
    }
    def k_(z: Z_xi, s: Double, eta: Double, beta_k: Double) = {
      val eta_prim = eta * (1 - s) / (1 - eta * (1 - s))
      val y1 = (1 + eta_prim) * z.xe / (z.xe * z.qf / (1 - z.qe) + eta_prim * (1 - z.xf))
      val y2 = ((1 - z.xf) * (1 - z.qe) / z.qf + eta_prim * z.xe) / ((1 + eta_prim) * (1 - z.xf))
      Interval(z.vf * y1 + z.ve, z.vf * y2 + z.ve, beta_k).mean
    }

    def s_bounds(z: Z_xi, k: Double, eta: Double): (Double, Double) = {
      val s1 = (-B(z) / (eta * C(z, k)) + B(z) / C(z, k) + 1) / (1 + B(z) / C(z, k))
      val s2 = (-A(z) / (eta * C(z, k)) + A(z) / C(z, k) + 1) / (1 + A(z) / C(z, k))
      (s1, s2)
    }
    def s_int(z: Z_xi, k: Double, eta: Double, beta: Double) = {
      val bounds = s_bounds(z, k, eta)
      Interval(bounds._1, bounds._2, beta)
    }
    def eta_(z: Z_xi, k: Double, s: Double): (Double, Double) = {
      val eta1 = A(z) / ((1 - s) * (C(z, k) + A(z)))
      val eta2 = B(z) / ((1 - s) * (C(z, k) + B(z)))
      (eta1, eta2)
    }
    val z0 = calib.z
    println("Limit of gk " + (1 / z0.vf * (1 - z0.xf) - calib.delta))
    val beta_k = beta(calib.k, k_bounds(z0)._1, k_bounds(z0)._2)
    val beta_s = beta(calib.s, s_bounds(calib.z, calib.k, calib.eta)._1, s_bounds(calib.z, calib.k, calib.eta)._2)

    val xrate = math.pow(1.0 / 0.01, 1.0 / nyears) - 1
    println("Growing rate of x: " + xrate * 100 + "%")
    def s_fun(x_index: Int, qf: Double) = {
      (0 to 1000).map(_.toDouble / 1000.0).toList.map(s => {
        val z = new Z_xi(ve_x(x_index), z0.vf, qe_x(x_index), qf, xe_x(x_index), z0.xf, z0.deltae, z0.deltaf)
        val eta_prim = eta * (1 - s) / (1 - eta * (1 - s))
        val y1 = (1 + eta_prim) * z.xe / (z.xe * z.qf / (1 - z.qe) + eta_prim * (1 - z.xf))
        val y2 = ((1 - z.xf) * (1 - z.qe) / z.qf + eta_prim * z.xe) / ((1 + eta_prim) * (1 - z.xf))
        (s, Interval(z.vf * y1 + z.ve, z.vf * y2 + z.ve, beta_k))
      })
    }
    val years = (1 to nyears + 50).map(i => i + 2017).toList
    val s = scala.collection.mutable.ArrayBuffer.empty[Interval];
    val gk, k, x, ve, qe, qf, xe, eroi = scala.collection.mutable.ArrayBuffer.empty[Double];
    val z = scala.collection.mutable.ArrayBuffer.empty[Z_xi];
    val sfuns = scala.collection.mutable.ArrayBuffer.empty[(Double, List[(Double, Interval)])]
    z += z0; s += s_int(calib.z, calib.k, calib.eta, beta_s);
    x += 0.0; ve += z0.ve; qf += z0.qf; xe += z0.xe
    k += k_int(z0, beta_k).mean
    gk += gk_(k.last, z0, calib.s, calib.delta)

    println(calib.k + "\t" + calib.y + "\t" + (calib.k - calib.ve) / calib.vf)
    println(" S " + "\t" + s_bounds(calib.z, calib.k, calib.eta) + "\t" + beta_s)
    println(" eta " + "\t" + eta_(calib.z, calib.k, calib.s) + "\t" + beta(calib.eta, eta_(calib.z, calib.k, calib.s)._1, eta_(calib.z, calib.k, calib.s)._2))
    println("Initialize" + "\t" + x.last + "\t" + calib.k + "," + k.last + "\t" + calib.gk + "," + gk.last)
    for (y <- years) {
      k += k.last * (1 + gk.last)
      // x is fixed -> the new vector z is directly given
      x += math.min(x.last + 1.0 / nyears, 1.0) //(if (x.last == 0) 0.01 else if (x.last > 1) x.last else x.last * (1 + xrate))
      val x_index = find_index_list(x.last, x_x)
      z += new Z_xi(ve_x(x_index), z0.vf, qe_x(x_index), qf.last, xe_x(x_index), z0.xf, z0.deltae, z0.deltaf)
      // s correponds to the couple (k,x)
      //val sfun = s_fun(x_index, qf.last)
      //s_implicit += sfun(find_index(k.last, sfun))._1
      //val res = (x.last, sfun)
      //sfuns += res
      s += s_int(z.last, k.last, eta, beta_s)
      gk += gk_(k.last, z.last, math.max(0.0, math.min(s.last.mean, 1.0)), calib.delta)
      println(y + "\t" + k.last + "\t" + gk.last + "\t" + x.last + "\t" + math.max(0.0, math.min(s.last.mean, 1.0)) + "\t" + A(z.last) + "\t" + C(z.last, k.last))

      val k_test = (20 to 100).toList.map(_.toDouble)
      val s_test_beta = k_test.map(k => s_int(z.last, k, calib.eta, beta_s))
      val s_test = k_test.map(k => s_int(z.last, k, calib.eta, 0.5))

      plotXY(List((k_test, s_test_beta.map(_.mean), "s_beta"),
        (k_test, s_test_beta.map(_.min), "s_min"),
        (k_test, s_test_beta.map(_.max), "s_max"),
        (k_test, s_test.map(_.mean), "s_mean")), legend = true, xLabel = "k", yLabel = "s_" + x.last, title = "s_" + x.last)

    }
    //  plot_(years, x, "x"); 
    plot_(years, k, "k_" + nyears); plot_(years, gk, "gk_" + nyears); plot_int(years, s, "s_" + nyears); plot_(years, x, "x_" + nyears)
    // plotXY(sfuns.toList.map(s => (s._2.map(_._1),s._2.map(_._2.max),s._1.toString)), legend = true)
  }*/
}
