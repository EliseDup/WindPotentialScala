package economic_model

import wind_energy._
import solar_energy._
import utils._
import squants.energy._
import squants.time.Hours
import java.io.FileOutputStream
import scala.collection.mutable.ArrayBuffer

object DynamicXi {
  val calib = new calibration_results_CI()
  val dyn_1 = new Dynamic_s_eta(calib.s, calib.eta)
  val dyn_2a = new Dynamic_s_gamma(calib.s, calib.gamma)
  val dyn_2b = new Dynamic_s_gamma_b(calib.s, calib.gammab)
  val dyn_3 = new Dynamic_gk_eta(calib.gk, calib.eta, 1 / 25.0)

  import CalibrationDataXi._
  import PlotHelper._
  def main(args: Array[String]): Unit = {
    dyn_1.simulate_int(calib, calib.qf, nyears = 200)
    dyn_1.simulate_int(calib, calib.qf, nyears = 200, max = true)
    // dyn_1.simulate_int(calib, calib.qf, nyears = 200)  
    //dyn_2b.simulate_int(calib, calib.qf, nyears = 200, max=true)
    //dyn_3.simulate_int(calib, calib.qf, nyears = 200, max=true)

  }

  def qf_detailed_results(step_qf: Int) {
    val qf_max = (0 until step_qf).map(i => calib.qf * (1 - i / step_qf.toDouble)).toList
    detailedResults(qf_max, dyn_3, calib, "3")
    detailedResults(qf_max, dyn_1, calib, "1")
    detailedResults(qf_max, dyn_2a, calib, "2a")
    detailedResults(qf_max, dyn_2b, calib, "2b")
  }

  def detailedResults(qf_max: List[Double], dyn: Dynamic_Params, calib: calibration_results_CI, label: String) {
    val out = new java.io.PrintStream(new java.io.FileOutputStream("res_" + label))

    val res = qf_max.map(qf => dyn.simulate_int(calib, qf, 1.0 / 100, 200, false))
    (0 until qf_max.size) map (i => out.print(qf_max(i) + "\t" + res(i)._1 + "\t" + res(i)._3 + "\t" + res(i)._3 + "\t" + res(i)._4 + "\n"))
    out.close()
    println("--- Exercice " + label + " Ended --- ")

    plotXY(List((qf_max, res.map(_._1), "")), xLabel = "qf_max", yLabel = "x_max", title = "qf_x_max_" + label)
    plotXY(List((qf_max, res.map(_._2), "")), xLabel = "qf_max", yLabel = "k_max", title = "qf_k_max_" + label)
    plotXY(List((qf_max, res.map(_._3), "")), xLabel = "qf_max", yLabel = "gk_max", title = "qf_gk_max_" + label)

  }
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
}
// Gamma = Ce/C constant
class Dynamic_s_gamma(s: Double, gamma: Double) extends Dynamic_Params(s, gamma) {
  override def toString() = "Ex2a"
  def y_int(z: Z_xi) = {
    val A1 = 1 / (gamma * (1 - s)) - B(z)
    val B1 = 1 / (gamma * (1 - s)) - A(z)
    val y1 = (A1 * (1 - z.qe) + z.xe) / (1 - z.xf + A1 * z.qf)
    val y2 = (B1 * (1 - z.qe) + z.xe) / (1 - z.xf + B1 * z.qf)
    Interval(y1, y2)
  }
  def ratio_spib_k(k: Double, z: Z_xi) = {
    s / (gamma * (1 - s)) * ((1 - z.qe) / k - z.qf / z.vf * (1 - z.ve / k))
  }
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
}

class Dynamic_gk_eta(gk: Double, eta: Double, val delta: Double) extends Dynamic_Params(gk, eta) {
  override def toString() = "Ex3"
  def y_int(z: Z_xi) = {
    val (k1, k2) = k_bounds(z)
    Interval((k1 - z.ve) / z.vf, (k2 - z.ve) / z.vf)
  }
  override def k_bounds(z: Z_xi): (Double, Double) = {
    val A1 = 1 / (eta * (1 - eta)) * A(z)
    val B1 = 1 / (eta * (1 - eta)) * B(z)
    def coef(a: Double) = (a * (1 - z.qe + z.qf * z.ve / z.vf) + (1 - z.xf) * z.ve / z.vf + z.xe) / (a * z.qf / z.vf + (1 - z.xf) / z.vf - (gk + delta))
    (coef(A1), coef(B1))
  }

  def ratio_spib_k(k: Double, z: Z_xi) = gk + delta
  override def s_k(k: Double, z: Z_xi) = (1 - eta) * (gk + delta) / ((1 - z.xf) / z.vf - eta * (gk + delta) - ((1 - z.xf) / z.vf * z.ve + z.xe) / k)
}

abstract class Dynamic_Params(param1: Double, param2: Double) {
  import Helper._
  import PlotHelper._
  import GrowthModel._
  def beta(a0: Double, a1: Double, a2: Double) = (a0 - a1) / (a2 - a1)
  // Intervalles sur y = Ye/Yf
  def y_int(z: Z_xi): Interval
  // sPIB/K (in order to calculate gk = sPIB/K-delta  
  def ratio_spib_k(k: Double, z: Z_xi): Double
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

  def x_fun = {
    (getLines("x_qe_xe_ve_10k", "\t").map(i => (i(0).toDouble)),
      getLines("x_qe_xe_ve_10k", "\t").map(i => (i(1).toDouble)),
      getLines("x_qe_xe_ve_10k", "\t").map(i => (i(2).toDouble)),
      getLines("x_qe_xe_ve_10k", "\t").map(i => (i(3).toDouble)))
  }
  def A(z: Z_xi) = z.xe / (1 - z.qe)
  def B(z: Z_xi) = (1 - z.xf) / z.qf;
  def C(z: Z_xi, k: Double) = {
    val y = (k - z.ve) / z.vf
    ((1 - z.xf) * y - z.xe) / (1 - z.qe - z.qf * y)
  }
  def qf_fun(qf_0: Double, qf_max: Double, qf_rate: Double, T: Int) = {
    qf_max + (qf_0 - qf_max) * Math.pow(1 - qf_rate, T - 1)
  }

  def p_int(z: Z_xi) = Interval(A(z), B(z))
  def eroi_z(z: Z_xi) = 1 / (z.qe + (z.deltae * z.ve + z.xe) * z.qf)

  def find_interpolation(k: Double, k_fun: List[(Double, Interval)], max: Boolean): (Int, Int, Double) = {
    val index = {
      if (k < mean(k_fun(0)._2, max)) 0
      else if (k > mean(k_fun(k_fun.size - 1)._2, max)) k_fun.size - 1
      else (0 until k_fun.size - 1).toList.find(i => mean(k_fun(i)._2, max) <= k && mean(k_fun(i + 1)._2, max) >= k).get
    }
    if (index == k_fun.size - 1) (index, index, 1)
    else {
      val k1 = mean(k_fun(index)._2, max); val k2 = mean(k_fun(index + 1)._2, max)
      val x1 = k_fun(index)._1; val x2 = k_fun(index + 1)._1;
      val x_star = (x2 - x1) / (k2 - k1) * (k - k2) + x2
      (index, index + 1, (x_star - x1) / (x2 - x1))
    }
  }

  def interpolate(params: (Int, Int, Double), list: List[Double]) {
    list(params._1) * (1 - params._3) + list(params._2) * params._3
  }

  def find_index(k: Double, k_fun: List[(Double, Interval)], max: Boolean): Int = {
    val res = {
      if (k < mean(k_fun(0)._2, max)) 0
      else if (k > mean(k_fun(k_fun.size - 1)._2, max)) k_fun.size - 1
      else (0 until k_fun.size - 1).toList.find(i => mean(k_fun(i)._2, max) <= k && mean(k_fun(i + 1)._2, max) >= k).get
    }
    println(k + "\t" + mean(k_fun(res)._2, max))
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
  val (x_x, qe_x, xe_x, ve_x) = x_fun
  // Function that for a given x and qf gives de corresponding k
  def k_x(qf: Double, z0: Z_xi, beta: Double = 0.5): List[(Double, Interval)] = {
    (0 until x_x.size).toList.map(i => {
      val z = new Z_xi(ve_x(i), z0.vf, qe_x(i), qf, xe_x(i), z0.xf, z0.deltae, z0.deltaf)
      (x_x(i), k_int(z, beta))
    })
  }
  def mean(int: Interval, max: Boolean) = if (max) int.max else int.mean

  def simulate_int(calib: calibration_results_CI, qf_max: Double, qf_rate: Double = 1 / 100.0, nyears: Int = 100, plot: Boolean = true, max: Boolean = false): (Double, Double, Double, Double) = {
    val z0 = calib.z
    val beta_k = if (max) 1 else (calib.k - k_bounds(z0)._1) / (k_bounds(z0)._2 - k_bounds(z0)._1)
    val k = scala.collection.mutable.ArrayBuffer.empty[Interval];
    val gk, s, k_mean, x, qf, eroi = scala.collection.mutable.ArrayBuffer.empty[Double];
    val z = scala.collection.mutable.ArrayBuffer.empty[Z_xi];
    // Variables de résultats
    z += z0; k += k_int(z0, beta_k);
    gk += gk_k(mean(k.last, max), z0, calib.delta);
    x += 0.0; qf += z0.qf; s += calib.s;
    k_mean += mean(k_int(z0, beta_k), max)

    val C, Ce, Cf, K, Ke, Kf, yf, pib = scala.collection.mutable.ArrayBuffer.empty[Double];
    val ye = calib.data.ye(calib.i).to(calib.energy_units)
    C += calib.C; Ce += calib.ce(calib.i); Cf += calib.Cf; K += calib.K; Ke += calib.Ke; Kf += calib.Kf; yf += calib.yf; pib += calib.pib
    print(C.last + "\t" + (1 - s.last) * pib.last)
    val years = (1 to nyears).map(i => i + 2017).toList
    println(calib.data.ye(calib.i).to(calib.energy_units) * (1 - calib.qe) - calib.qf * calib.yf)
    println("Initialize k = " + k.last + "(k0 = " + calib.k + "), gk = " + gk.last + "(gk_0 =" + calib.gk + ")" + "(beta_k= " + beta_k + ")")
    var end = false
    for (y <- years) {
      k_mean += mean(k.last, max) * (1 + gk.last)
      // vqy=qys+(qy-qys)*(1-.01).^(0:T-1)
      qf += qf_fun(z0.qf, qf_max, qf_rate, y - 2017) //qf_max + (qf.last - qf_max) * (1 - qf_rate)
      val k_x_fun = k_x(qf.last, z0, beta_k)
      val index_x = find_index(k_mean.last, k_x(qf.last, z0, beta_k), max)
      val test = find_interpolation(k_mean.last, k_x(qf.last, z0, beta_k), max)
      println(ve_x(index_x) + "\t" + (ve_x(test._1) * (1 - test._3) + ve_x(test._2) * test._3))
      println(qe_x(index_x) + "\t" + (qe_x(test._1) * (1 - test._3) + qe_x(test._2) * test._3))
      println(xe_x(index_x) + "\t" + (xe_x(test._1) * (1 - test._3) + xe_x(test._2) * test._3))

      if ((x.last == 1 || math.abs(x.last - k_x_fun(index_x)._1) < 0.0001) && !end) {
        println("Transition stops after " + (y - 2017) + " years")
        end = true
      }
      x += k_x_fun(index_x)._1;
      z += Z_xi(ve_x(index_x), z0.vf, qe_x(index_x), qf.last, xe_x(index_x), z0.xf, z0.deltae, z0.deltaf)

      k += k_int(z.last, beta_k);
      s += s_k(mean(k.last, max), z.last)
      gk += gk_k(mean(k.last, max), z.last, calib.delta);
      eroi += eroi_z(z.last)
      if (z.last.xe / (1 - z.last.xf) > (1 - z.last.qe) / z.last.qf) println("Constraint on y violated" + "\t" + z.last)
      // Calculated parameters
      K += mean(k.last, max) * ye; Ke += ye * z.last.ve; Ke += K.last - Ke.last; yf += Kf.last / calib.vf;
      pib += (gk.last + calib.delta) * K.last / s_k(mean(k.last, max), z.last); C += (1 - s.last) * pib.last
      Ce += ye * (1 - z.last.qe) - qf.last * yf.last; // Cf += C-p*Ce
      // if (C(z.last, k.last.mean) < 0) println("C becomes < 0 in year " + y)
      // println(y + "\t" + mean(k.last,max) + "\t" + k_mean.last + "\t" + gk.last + "\t" + x.last + "\t" + s.last + "\t" + eroi.last)  
    }
    println("x" + "\t" + "k" + "\t" + "gk" + "\t" + "s" + "\t" + "eroi" + "\t" + "theta")
    println(x.last + "\t" + mean(k.last, max) + "\t" + gk.last + "\t" + s.last + "\t" + eroi.last + "\t" + beta_k)
    if (plot) {
      plot_(years, x, "x_" + toString()); plot_(years, gk, "gk_" + toString());
      plot_int(years, k, "k_" + toString());
      plot_(years, s, "s_" + toString()); plot_(years, eroi, "eroi_" + toString());
    }
    (x.last, mean(k.last, max), gk.last, eroi.last)
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

  def simulate_x_rate(calib: calibration_results_CI, eta: Double, nyears: Int) {
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
  }
}
