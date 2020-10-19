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
  val ye_0 = calib.data.ye(calib.i).to(calib.energy_units)
  //val dyn_2a = new Dynamic_s_gamma(calib.s, calib.gamma)
  val dyn_2b = new Dynamic_s_gamma_b(calib.s, calib.gammab)
  val dyn_3 = new Dynamic_gk_eta(calib.gk, calib.eta, calib.delta)
  val qf_0 = calib.qf; val qf_f = calib.qf * 0.5; val ye_f = 3 * ye_0
  // Based on history : Ye +2% / year, qf - 0.7 % / year
  //val no_qf = Scenario(qf_0, qf_0, qf_0, ye_0,ye_0,ye_0, 2040-2017) // ye_f, ye_0 * math.pow(1 + 1.62433159 / 100, 2040 - 2017), 2040 - 2017)

  val baseline = new Scenario(qf_0, qf_0 / 2, 0.01, ye_0, 2 * ye_0, 0.01)

  val history = Scenario(qf_0, qf_f, qf_0 * 0.85, ye_0, 2 * ye_0, ye_0 * math.pow(1 + 1.62433159 / 100, 2040 - 2017), 2040 - 2017)
  val history3 = Scenario(qf_0, qf_f, qf_0 * 0.85, ye_0, 3 * ye_0, ye_0 * math.pow(1 + 1.62433159 / 100, 2040 - 2017), 2040 - 2017)

  val new_policies = Scenario(qf_0, qf_f, qf_0 * 0.58 * 0.85 / 0.64, ye_0, ye_f, 12581, 2040 - 2017)
  val sustainable_dev = Scenario(qf_0, qf_f, qf_0 * 0.45 * 0.85 / 0.64, ye_0, ye_0, ye_0, 2040 - 2017)

  println("BAU" + " & " + (100 * history.ye_rate) + " & " + 100 * history.qf_rate)
  println("New Policies" + " & " + 100 * new_policies.ye_rate + " & " + 100 * new_policies.qf_rate)
  println("DD" + " & " + 100 * sustainable_dev.ye_rate + "& " + 100 * sustainable_dev.qf_rate)

  /*
  val t = (0 until 100).toList
  plotXY(List(
    (t.map(i => (i + 2017).toDouble), t.map(i => history.ye(i)), "BAU"),
    (t.map(i => (i + 2017).toDouble), t.map(i => new_policies.ye(i)), "New Policies"),
    (t.map(i => (i + 2017).toDouble), t.map(i => sustainable_dev.ye(i)), "Sustainable Dev")), yLabel = "Ye [Mtoe]", legend = true, title = "Ye_scenarios")
  plotXY(List(
    (t.map(i => (i + 2017).toDouble), t.map(i => history.qf(i)), "BAU"),
    (t.map(i => (i + 2017).toDouble), t.map(i => new_policies.qf(i)), "New Policies"),
    (t.map(i => (i + 2017).toDouble), t.map(i => sustainable_dev.qf(i)), "Sustainable Dev")), yLabel = "qf [toe/kUS$2010]", legend = true, title = "qf_scenarios")
*/
  val t = (0 until 30).toList
  plotXY(List(
    (t.map(i => (i + 2017).toDouble), t.map(i => history.ye(i)), "2*Ye"),
    (t.map(i => (i + 2017).toDouble), t.map(i => history3.ye(i)), "3*Ye")), yLabel = "Ye", legend = true, title = "Ye_scenarios")

  def main(args: Array[String]): Unit = {
    // dyn_1.simulate_int(calib, baseline, 300, true)
    //  dyn_1.simulate_int(calib, history, 50, false)
    /*
    val res = (dyn_1.simulate_int(calib, history, 100, false),
      dyn_1.simulate_int(calib, new_policies, 100, false),
      dyn_1.simulate_int(calib, sustainable_dev, 100, false))
    plotXY(List((res._1.years.map(_.toDouble), res._1.gk.map(_ * 100), "BAU"),
      (res._2.years.map(_.toDouble), res._2.gk.map(_ * 100), "Current Policies"),
      (res._3.years.map(_.toDouble), res._3.gk.map(_ * 100), "Sustainable Dev")), legend = true, yLabel = "Economy growth rate [%]", title = "gk")
    plotXY(List((res._1.years.map(_.toDouble), res._1.x.map(_ * 100), "BAU"),
      (res._2.years.map(_.toDouble), res._2.x.map(_ * 100), "Current Policies"),
      (res._3.years.map(_.toDouble), res._3.x.map(_ * 100), "Sustainable Dev")), legend = true, yLabel = "Renewable Penetration [%]", title = "x")

    plotXY(List((res._1.years.map(_.toDouble), res._1.model.mu.toList.map(_ * 100), "BAU"),
      (res._2.years.map(_.toDouble), res._2.model.mu.toList.map(_ * 100), "Current Policies"),
      (res._3.years.map(_.toDouble), res._3.model.mu.toList.map(_ * 100), "Sustainable Dev")), legend = true, yLabel = "Relative Size of the energy sector [%]", title = "mu")
*/
    //printTable(current_policies, 50)
    //printTable(current_policies, 100)
    //printTable(current_policies, 800)
  }

  def printTablesScenarios(nys: List[Int]) {
    nys.map(ny => {
      println("Past extrapolation Scenario after " + ny + " years ")
      printTable(history, ny)
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

  def qf_detailed_results(step_qf: Int, scenario: Scenario, max: Boolean) {
    val qf_min = (0 until step_qf).map(i => Scenario(scenario, calib.qf * (1 - i / step_qf.toDouble))).toList
    detailedResults(qf_min, dyn_1, calib, "1", max)
    // detailedResults(qf_min, dyn_2a, calib, "2a")
    detailedResults(qf_min, dyn_2b, calib, "2b", max)
    detailedResults(qf_min, dyn_3, calib, "3", max)
  }

  def detailedResults(qf_max: List[Scenario], dyn: Dynamic_Params, calib: calibration_results_CI, label: String, max: Boolean) {
    val out = new java.io.PrintStream(new java.io.FileOutputStream("res_" + label))

    val res = qf_max.map(qf => dyn.simulate_int(calib, qf, 5000, plot = false, max = max))
    (0 until qf_max.size) map (i => out.print(qf_max(i) + "\t" + res(i).x + "\t" + res(i).k + "\t" + res(i).gk + "\t" + res(i).eroi + "\n"))
    out.close()
    println("--- Exercice " + label + " Ended --- ")
    val qf_rel = qf_max.map(q => q.qf_f / calib.qf)
    plotXY(List((qf_rel, res.map(r => r.x(r.last)), "")), xLabel = "qf_min/qf_0", yLabel = "x", title = "qf_min_x_" + label + max)
    //plotXY(List((qf_max, res.map(_.k), "")), xLabel = "qf_min", yLabel = "k", title = "qf_min_k_" + label + max)
    //plotXY(List((qf_max, res.map(_.gk), "")), xLabel = "qf_min", yLabel = "gk", title = "qf_min_gk_" + label + max)
    plotXY(List((qf_rel, res.map(r => r.s.max), "")), xLabel = "qf_min/qf_0", yLabel = "s_max", title = "qf_min_s_max_" + label + max)

    plotXY(List((qf_rel, res.map(_.end_year).map(_.getOrElse(0).toDouble), "")), xLabel = "qf_min/qf_0", yLabel = "years to finish transition", title = "qf_min_y_" + label + max)

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
object Scenario {
  def apply(qf_0: Double, qf_f: Double, qf_t: Double, ye_0: Double, ye_f: Double, ye_t: Double, t: Int) = {
    val ye_rate = if (ye_0 == ye_f) 0.0 else 1 - math.pow((ye_t - ye_f) / (ye_0 - ye_f), 1.0 / t)
    val qf_rate = if (qf_0 == qf_f) 0.0 else 1 - math.pow((qf_t - qf_f) / (qf_0 - qf_f), 1.0 / t)
    new Scenario(qf_0, qf_f, qf_rate, ye_0, ye_f, ye_rate)
  }
  def apply(scen: Scenario, qf_f: Double) = new Scenario(scen.qf_0, qf_f, scen.qf_rate, scen.ye_0, scen.ye_f, scen.ye_rate)
}
class Scenario(val qf_0: Double, val qf_f: Double, val qf_rate: Double, val ye_0: Double, val ye_f: Double, val ye_rate: Double) {
  def qf(T: Int) = {
    qf_f + (qf_0 - qf_f) * Math.pow(1 - qf_rate, T)
  }
  def ye(T: Int) = {
    ye_f + (ye_0 - ye_f) * Math.pow(1 - ye_rate, T)
  }
}

/*
 * Exponential variation of rate rate_0 until t_lim, then exponentiel variation of the difference between x0 and xf
 */
object ParamsScenario {
  def apply(x0: Double, xf: Double, xt: Double, t: Int) = {
    val rate0 = 1 - math.pow(xt / x0, 1.0 / t)
    val ratef = if (x0 == xf) 0.0 else 1 - math.pow((xt - xf) / (x0 - xf), 1.0 / t)
    new ParamsScenario(x0, xf, rate0, ratef, t)
  }
}
class ParamsScenario(val x0: Double, val xf: Double, val rate_0: Double, val rate_f: Double, val t_lim: Int) {
  def x(T: Int) = {
    if (T <= t_lim) x0 * math.pow(1 + rate_0, T)
    else xf + (x0 - xf) * Math.pow(1 - rate_f, T)
  }
}

case class DynamicResult(val years: List[Int], val x: List[Double], val k: List[Double], val gk: List[Double], val s: List[Double], val eroi: List[Double], val theta: Double, val end_year: Option[Int],
    val model: Model) { //val mu: List[Double], val p_ratio: List[Double], val ce_ratio: List[Double], val cf_ratio: List[Double]) {
  val last = x.size - 1
  override def toString() = x(last) + "\t" + k(last) + "\t" + gk(last) + "\t" + s(last) + "\t" + eroi(last) + "\t" + theta + "\t" + end_year + "\t" + model.mu(last) // + "\t" + p_ratio(last) + "\t" + ce_ratio(last) + "\t" + cf_ratio(last)
}
class Model(calib: calibration_results_CI, params: Dynamic_Params) {
  val C, Ce, Df, Cf, Ke, Kf, Yf, pib, I, mu, p, eta, gamma = scala.collection.mutable.ArrayBuffer.empty[Double];
  C += calib.C; Ce += calib.ce(calib.i); Cf += calib.Cf; Df += (calib.yf - calib.Xe - calib.Xf); Ke += calib.Ke; Kf += calib.Kf; Yf += calib.yf; pib += calib.pib; mu += calib.mu; p += calib.p;
  def update(k: Double, K: Double, z: Z_xi, gk: Double, s: Double, ye: Double) {
    Ke += ye * z.ve; Kf += K - Ke.last; Yf += Kf.last / calib.vf;
    pib += (gk + calib.delta) * K / params.s_k(k, z); C += (1 - s) * pib.last; I += s * pib.last
    Ce += ye * (1 - z.qe) - z.qf * Yf.last; mu += Ke.last / K;
    Df += Yf.last * (1 - z.xf) - ye * z.xe
    p += params.p(this)
    Cf += C.last - p.last * Ce.last
    eta += p.last * Ce.last / C.last; gamma += Ce.last / Cf.last
    // p += param2 * Cf.last / Ce.last; // Cf += C-p*Ce
    //if (Ce.last < 0) println("Ce becomes < 0 in year " + (2017 + C.size))
  }
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
  def ratio_spib_k(k: Double, z: Z_xi) = {
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
  def ratio_spib_k(k: Double, z: Z_xi) = {
    (1 - z.xf + z.qf / gamma) / z.vf - ((1 - z.xf + z.qf / gamma) * z.ve / z.vf + z.xe + (1 - z.qe) / gamma) / k
  }
  // vp=vDy./vCe/ss-sg;
  def p(model: Model) = (1 - s) / s * model.Df.last / model.Ce.last - 1 / (s * gamma)
}

class Dynamic_gk_eta(gk: Double, eta: Double, val delta: Double) extends Dynamic_Params(gk, eta) {
  override def toString() = "Ex3"
  def y_int(z: Z_xi) = {
    val (k1, k2) = k_bounds(z)
    Interval((k1 - z.ve) / z.vf, (k2 - z.ve) / z.vf)
  }
  override def k_bounds(z: Z_xi): (Double, Double) = {
    val A1 = (1 - eta) / eta * A(z)
    val B1 = (1 - eta) / eta * B(z)
    def coef(a: Double) = (a * (1 - z.qe + z.qf * z.ve / z.vf) + (1 - z.xf) * z.ve / z.vf + z.xe) / (a * z.qf / z.vf + (1 - z.xf) / z.vf - (gk + delta))
    (coef(A1), coef(B1))
  }

  def ratio_spib_k(k: Double, z: Z_xi) = gk + delta
  override def s_k(k: Double, z: Z_xi) = (1 - eta) * (gk + delta) / ((1 - z.xf) / z.vf - eta * (gk + delta) - ((1 - z.xf) / z.vf * z.ve + z.xe) / k)
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
  def ratio_spib_k(k: Double, z: Z_xi): Double
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
  def gk_int(z: Z_xi, beta: Double, delta: Double) = {
    val g_list = List(gk_k(k_int(z, beta).min, z, delta), gk_k(k_int(z, beta).max, z, delta))
    Interval(g_list.min, g_list.max)
  }
  def gk_k(k: Double, z: Z_xi, delta: Double): Double = ratio_spib_k(k, z) - delta

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
      lines.map(i => (i(2).toDouble)))
  }
  // def x_fun_ye(ye: Double) = GrowthModel.x_qe_xe_ve(MegaTonOilEquivalent(ye))
  def x_fun_ye(ye: Double, z0: Z_xi) = {
    val (ye_re, tilde_xe, tilde_ke) = ye_fun(ye)
    val res = (0 until ye_re.size).toList.map(i => {
      val x = ye_re(i) / ye
      val ve = if (x == 0) z0.ve else x * tilde_ke(i) / (z0.qf * ye_re(i)) + (1 - x) * z0.ve
      val qe = if (x == 0) z0.qe else (1 - x) * z0.qe
      val xe = if (x == 0) z0.xe else x * tilde_xe(i) / (z0.qf * ye_re(i)) + (1 - x) * z0.xe
      (x, qe, xe, ve)
    })
    (res.map(_._1), res.map(_._2), res.map(_._3), res.map(_._4))
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
  def k_x_ye(qf: Double, z0: Z_xi, beta: Double = 0.5, ye: Double): List[(Double, Interval)] = {
    val (x_x, qe_x, xe_x, ve_x) = x_fun_ye(ye, z0)
    (0 until x_x.size).toList.map(i => {
      val z = new Z_xi(ve_x(i), z0.vf, qe_x(i), qf, xe_x(i), z0.xf, z0.deltae, z0.deltaf)
      (x_x(i), k_int(z, beta))
    })
  }

  def mean(int: Interval, max: Boolean) = if (max) int.max else int.mean

  def simulate_int(calib: calibration_results_CI, scenario: Scenario, nyears: Int = 100, plot: Boolean = true, max: Boolean = false): DynamicResult = {
    val z0 = calib.z; val beta_k = if (max) 1 else (calib.k - k_bounds(z0)._1) / (k_bounds(z0)._2 - k_bounds(z0)._1)
    val k_interval = scala.collection.mutable.ArrayBuffer.empty[Interval];
    val gk, s, K, k, x, qf, eroi, ner, ye = scala.collection.mutable.ArrayBuffer.empty[Double];
    val z = scala.collection.mutable.ArrayBuffer.empty[Z_xi];
    z += z0; k_interval += k_int(z0, beta_k); K += calib.K
    gk += gk_k(mean(k_interval.last, max), z0, calib.delta);
    x += 0.0; qf += z0.qf; s += calib.s;
    k += mean(k_int(z0, beta_k), max)
    eroi += calib.eroi; ner += calib.ner;
    val ye0 = calib.data.ye(calib.i).to(calib.energy_units)
    ye += ye0

    // Variables de résultats
    val model = new Model(calib, this)
    println("Initialize " + k.last + " " + gk.last + " " + beta_k)
    val years = (1 until nyears).map(i => i + 2017).toList
    var end = false; var endYear: Option[Int] = None
    for (y <- years) {
      // vqy=qys+(qy-qys)*(1-.01).^(0:T-1)
      qf += scenario.qf(y - 2017) // qf_fun(z0.qf, qf_max, qf_rate, y - 2017) //qf_max + (qf.last - qf_max) * (1 - qf_rate)
      ye += scenario.ye(y - 2017) //qf_fun(ye0, ye_max, ye_rate, y - 2017)

      K += K.last * (1 + gk.last)
      k += K.last / ye.last
      // Transition is over, z and x are not changing anymore
      if (end) {
        z += z.last
        x += 1 // x.last
      } else {
        val k_x_fun = {
          k_x_ye(qf.last, z0, beta_k, ye.last)
        }
        //println("Valeur de k" + "\t" + k.last)
        //println("Borne inf" +"\t" + k_x_fun(0)._1+"\t" + k_x_fun(0)._2)
        val (x_x, qe_x, xe_x, ve_x) = /*if (ye_rate == 0) x_fun else*/ x_fun_ye(ye.last, z0)
        val indexes_x = find_indexes(k.last, k_x_fun, max)
        val k1 = mean(k_x_fun(indexes_x._1)._2, max); val k2 = mean(k_x_fun(indexes_x._2)._2, max);
        val x1 = k_x_fun(indexes_x._1)._1; val x2 = k_x_fun(indexes_x._2)._1
        val newX = math.max(0, interpolation(k.last, (k1, x1), (k2, x2)))

        x += newX
        val ve = interpolation(x.last, (x1, ve_x(indexes_x._1)), (x2, ve_x(indexes_x._2)))
        val qe = interpolation(x.last, (x1, qe_x(indexes_x._1)), (x2, qe_x(indexes_x._2)))
        val xe = interpolation(x.last, (x1, xe_x(indexes_x._1)), (x2, xe_x(indexes_x._2)))
        z += Z_xi(ve, z0.vf, qe, qf.last, xe, z0.xf, z0.deltae, z0.deltaf)
      }
      k_interval += k_int(z.last, beta_k);
      if (!end && k.last >= mean(k_interval.last, max) && x.last > 0.9) {
        println("Transition stops after " + (y - 2017) + " years")
        endYear = Some(y - 2017)
        end = true
      }
      s += s_k(k.last, z.last) //s_k(mean(k.last, max), z.last)
      gk += gk_k(k.last, z.last, calib.delta); // gk_k(mean(k.last, max), z.last, calib.delta);
      eroi += eroi_z(z.last)
      ner += ner_z(z.last)
      if (z.last.xe / (1 - z.last.xf) > (1 - z.last.qe) / z.last.qf) println("Constraint on y violated" + "\t" + z.last)
      // Calculated parameters
      model.update(k.last, K.last, z.last, gk.last, s.last, ye.last)
      //println(y + "\t" + ye.last + "\t" + qf.last + "\t" + x.last + "\t" + k.last + "\t" + mean(k_interval.last, max) + "\t" + gk.last + "\t" + s.last + "\t" + eroi.last + "\t" + beta_k + "\t" + model.mu.last + "\t" + model.eta.last + "\t" + model.gamma.last + "\t" + model.p.last / calib.p + "\t" + "\t" + model.Ce.last / calib.ce(calib.i) + "\t" + model.Cf.last / calib.Cf)
    }
    println("x" + "\t" + "k" + "\t" + "gk" + "\t" + "s" + "\t" + "eroi" + "\t" + "theta" + "\t" + "mu" + "\t" + "eta" + "\t" + "gamma" + "\t" + "p/p0")
    println(x.last + "\t" + mean(k_interval.last, max) + "\t" + gk.last + "\t" + s.last + "\t" + eroi.last + "\t" + beta_k + "\t" + model.mu.last + "\t" + model.eta.last + "\t" + model.gamma.last + "\t" + model.p.last / calib.p + "\t" + "\t" + model.Ce.last / calib.ce(calib.i) + "\t" + model.Cf.last / calib.Cf)
    if (plot) {
      val years_new = years // if (!end) years else (2017 until endYear.get + 2017).toList
      //  plotXY(List((years.map(_.toDouble), model.pib.toList, "pib"), (years.map(_.toDouble), model.I.toList, "I"),
      //     (years.map(_.toDouble), model.C.toList, "C")), yLabel = "", title = "pib_c_i", legend = true, int_x_axis = true)
      // plotXY(List((years_new.map(_.toDouble), model.C.toList, "C"), (years_new.map(_.toDouble), model.pCe, "pCe"),
      //   (years_new.map(_.toDouble), model.Cf.toList, "Cf")), yLabel = "", title = "c", legend = true, int_x_axis = true)

      //plotXY(x.toList, s.toList)
      plot_(years_new, x, "x_" + toString());
      plot_(years_new, gk, "gk_" + toString());
      //plot_int(years, k, "k_" + toString());
      plot_(years, ye, "ye_" + toString());
      plot_(years_new, model.mu, "mu_" + toString())
      //plot_(years_new, model.p, "p_" + toString())
      //plot_(years, model.gamma, "gamma_" + toString())
      // plot_(years, model.eta, "eta_" + toString())
      //plot_(years, s, "s_" + toString())
      // Plot interval and k mean   

      // plotXY(List((x.toList, k.toList, "k"), (x.toList, k_interval.toList.map(_.mean), "k_env"), (x.toList, k_interval.toList.map(_.min), "k_env" + "_min"), (x.toList, k_interval.toList.map(_.max), "k_env" + "_max")), yLabel = "k", title = "k", legend = true, int_x_axis = false)

    }
    DynamicResult(years, x.toList, k_interval.toList.map(k => mean(k, max)), gk.toList, s.toList, eroi.toList, beta_k, endYear, model) // model.mu.toList, model.p.toList.map(i=> i / calib.p), model.Ce.toList.map(i => i / calib.ce(calib.i)), model.Cf.toList.map(i => i / calib.Cf))
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
