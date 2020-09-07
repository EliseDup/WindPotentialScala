package economic_model

import wind_energy._
import solar_energy._
import utils._
import squants.energy._
import squants.time.Hours
import java.io.FileOutputStream
import scala.collection.mutable.ArrayBuffer

object DynamicXi {
  def main(args: Array[String]): Unit = {
    import CalibrationDataXi._
    val calib = new calibration_results_CI()
    // println("Calib : k = " + calib.k + ", gk = " + calib.gk + ", p = " + calib.p + ", y = " + calib.y)
    val s_eta = new Dynamic_s_eta(calib.s, calib.eta)
    //s_eta.simulate_int(calib.z, calib.delta, 1 / 100.0, calib.qf / 2, true)
    // s_eta.simulate(calib.k, calib.gk, calib.z, calib.delta, 1 / 100.0, calib.qf / 2, true)
    //val s_gamma = new Dynamic_s_gamma(calib.s, calib.gamma)
    //s_gamma.simulate_int(calib.z, calib.delta, 1 / 100.0, calib.qf / 2, true)
  }
}
case class Interval(val min : Double, val max : Double, val beta : Double = 0.5) {
  val mean = beta*min+(1-beta)*max
}
class Dynamic_s_eta(s: Double, eta: Double) extends Dynamic_Params(s, eta) {
  def y_int_params(z: Z_xi) = {
    val eta_prim = eta * (1 - s) / (1 - eta * (1 - s))
    val y1 = (1 + eta_prim) * z.xe / (z.xe * z.qf / (1 - z.qe) + eta_prim * (1 - z.xf))
    val y2 = ((1 - z.xf) * (1 - z.qe) / z.qf + eta_prim * z.xe) / ((1 + eta_prim) * (1 - z.xf))
    Interval(y1, y2)
  }
  def k_int(z: Z_xi) = {
    val y = y_int_params(z)
    val k1 = z.vf * y.min + z.ve
    val k2 = z.vf * y.max + z.ve
    Interval(k1, k2)
  }
  def gk_k(k: Double, z: Z_xi, delta: Double) = {
    1 / z.vf * s / (1 - eta * (1 - s)) * ((1 - z.xf) * (1 - z.ve / k) - z.xe * z.vf / k) - delta
  }
}

class Dynamic_s_gamma(s: Double, gamma: Double) extends Dynamic_Params(s, gamma) {
  def k_int(z: Z_xi) = {
    val k1 = z.vf / z.qf * ((1 - gamma * (1 - s) * (1 - z.xf) / z.qf) * (1 - z.qe) + gamma * z.xe * (1 - s)) + z.ve
    val k2 = z.vf * (1 - z.qe) / (z.qf * (1 - gamma * (1 - s) * z.xe / (1 - z.qe)) + gamma * (1 - s) * (1 - z.xf)) + z.ve
    Interval(k1, k2)
  }
  def gk_k(k: Double, z: Z_xi, delta: Double) = {
    s / (gamma * (1 - s)) * ((1 - z.qe + z.qf / z.vf * z.ve) / k - z.qf / z.vf) - delta
  }
}

abstract class Dynamic_Params(param1: Double, param2: Double) {
  import Helper._
  import PlotHelper._
  import GrowthModel._

  def k_int(z: Z_xi): Interval
  def gk_int(z: Z_xi, delta: Double)=Interval(gk_k(k_int(z).min, z, delta), gk_k(k_int(z).max, z, delta))
  def gk_k(k: Double, z: Z_xi, delta: Double): Double

  def x_fun = {
    (getLines("x_qe_xe_ve", "\t").map(i => (i(0).toDouble)),
      getLines("x_qe_xe_ve", "\t").map(i => (i(1).toDouble)),
      getLines("x_qe_xe_ve", "\t").map(i => (i(2).toDouble)),
      getLines("x_qe_xe_ve", "\t").map(i => (i(3).toDouble)))
  }

  def qf_fun(qf_0: Double, qf_max: Double, qf_rate: Double, T: Int) = {
    qf_max + (qf_0 - qf_max) * Math.pow(1 - qf_rate, T - 1)
  }

  // def bounds(a1: Double, a2: Double) = ((a1 + a2) / 2, List(a1, a2).min, List(a1, a2).max)

  def p_int(z: Z_xi) = Interval(z.xe / (1 - z.qe), (1 - z.xf) / z.qf)
  def y_int(z: Z_xi) = Interval(z.xe / (1 - z.xf), (1 - z.qe) / z.qf)
  def eroi_z(z: Z_xi) = 1 / (z.qe + (z.deltae * z.ve + z.xe) * z.qf)

  def find_index(k: Double, k_fun: List[(Double, (Double, Double, Double))]): Int = {
    if (k < k_fun(0)._2._1) 0
    else if (k > k_fun(k_fun.size - 1)._2._1) k_fun.size - 1
    else (0 until k_fun.size - 1).toList.find(i => k_fun(i)._2._1 <= k && k_fun(i + 1)._2._1 >= k).get
  }

  def simulate_int(z0: Z_xi, delta: Double, qf_rate: Double, qf_max: Double, plot: Boolean = false) {
    val (x_x, qe_x, xe_x, ve_x) = x_fun
    def k_x(qf: Double): List[(Double, Interval)] = {
      (0 until x_x.size).toList.map(i => {
        val z = new Z_xi(ve_x(i), z0.vf, qe_x(i), qf, xe_x(i), z0.xf, z0.deltae, z0.deltaf)
        (x_x(i), k_int(z))
      })
    }
    val k, gk = scala.collection.mutable.ArrayBuffer.empty[Interval];
    val k_mean, x, ve, qe, qf, xe, eroi = scala.collection.mutable.ArrayBuffer.empty[Double];
    val z = scala.collection.mutable.ArrayBuffer.empty[Z_xi];
    z += z0
    k += k_int(z0);
    gk += gk_int(z0, delta);
    x += 0.0; ve += z0.ve; qf += z0.qf; xe += z0.xe
    k_mean += k_int(z0).mean
    val years = (1 to 500).map(i => i + 2017).toList

    println("Initialize k = " + k.last + ", gk = " + gk.last + ", p = " + p_int(z0) + ", y = " + y_int(z0))

    for (y <- years) {
      k_mean += k.last.mean * (1 + gk.last.mean)
      // vqy=qys+(qy-qys)*(1-.01).^(0:T-1)
      qf += qf_fun(z0.qf, qf_max, 1 / 100.0, y - 2017) //qf_max + (qf.last - qf_max) * (1 - qf_rate)
      val k_x_fun = k_x(qf.last)
      val index_x = find_index(k_mean.last, k_x(qf.last))
      x += k_x_fun(index_x)._1;

      ve += ve_x(index_x)
      xe += xe_x(index_x)
      qe += qe_x(index_x)
      z += Z_xi(ve.last, z0.vf, qe.last, qf.last, xe.last, z0.xf, z0.deltae, z0.deltaf)
      k += k_int(z.last)
      gk += gk_int(z.last, delta); // 1/z0.vf*s/(1-eta*(1-s))*((1-z0.xf)*(1-ve.last/k.last)-xe.last*z0.vf/k.last) - delta
      eroi += eroi_z(z.last)
      if (z.last.xe / (1 - z.last.xf) > (1 - z.last.qe) / z.last.qf) println("Constraint on y violated" + "\t" + z.last)
      // println(y + "\t" + k.last._1 + "\t" + k.last._2 + "\t" + k.last._3 + "\t" + k_mean.last + "\t" + gk.last + "\t" + x.last + "\t" + qf.last + "\t" + eroi.last)

    }
    println("END - " + years.last + "\t" + " x = " + x.last + ", gk = " + gk.last.mean * 100 + " k/k0 " + k.last.mean / k.toList(0).mean)

    if (plot) {
      plot_(years, x, "x"); plot_int(years, k, "k"); plot_int(years, gk, "gk")
    }
  }
  def plot_(years: List[Int], x: ArrayBuffer[Double], title: String) {
    plotXY(List((years.map(_.toDouble), x.toList, "")), yLabel = title, title = title)
  }
  def plot_int(years: List[Int], x: ArrayBuffer[Interval], title: String) {
    plotXY(List((years.map(_.toDouble), x.toList.map(_.mean), title), (years.map(_.toDouble), x.toList.map(_.min), title + "_min"), (years.map(_.toDouble), x.toList.map(_.max), title + "_max")), yLabel = title, title = title, legend = true)
  }
  /* def simulate(k0: Double, gk0: Double, z0: Z_xi, delta: Double, qf_rate: Double, qf_max: Double, plot: Boolean = false) {
    val (x_x, qe_x, xe_x, ve_x) = x_fun
    def k_x(qf: Double): List[(Double, Interval)] = {
      (0 until x_x.size).toList.map(i => {
        val z = new Z_xi(ve_x(i), z0.vf, qe_x(i), qf, xe_x(i), z0.xf, z0.deltae, z0.deltaf)
        (x_x(i), k_int(z))
      })
    }
    val x, k, gk, qf = scala.collection.mutable.ArrayBuffer.empty[Double];
    val z = scala.collection.mutable.ArrayBuffer.empty[Z_xi];
    z += z0; k += k0; gk += gk0; qf += z0.qf;
    val years = (1 to 100).map(i => i + 2017).toList

    println("Initialize k = " + k.last + ", gk = " + gk.last + ", p = " + p_int(z0) + ", y = " + y_int(z0))

    for (y <- years) {
      k += k.last * (1 + gk.last)
      // vqy=qys+(qy-qys)*(1-.01).^(0:T-1)
      qf += qf_fun(z0.qf, qf_max, 1 / 100.0, y - 2017)
      val k_x_fun = k_x(qf.last)
      val index_x = find_index(k.last, k_x(qf.last))
      x += x_x(index_x);
      z += Z_xi(ve_x(index_x), z0.vf, qe_x(index_x), qf.last, xe_x(index_x), z0.xf, z0.deltae, z0.deltaf)
      gk += gk_k(k.last, z.last, delta); // 1/z0.vf*s/(1-eta*(1-s))*((1-z0.xf)*(1-ve.last/k.last)-xe.last*z0.vf/k.last) - delta
      // eroi += eroi_z(z.last)
      if (z.last.xe / (1 - z.last.xf) > (1 - z.last.qe) / z.last.qf) println("Constraint on y violated" + "\t" + z.last)
      if (k.last < k_int(z.last)._2 || k.last > k_int(z.last)._3) println("k not in the interval " + k.last + "\t" + k_int(z.last))
    }

    println("END - " + years.last + "\t" + " x = " + x.last + ", gk = " + gk.last * 100 + " k/k0 " + k.last / k0)

    if (plot) {
      plot_(years, x, "x"); plot_(years, k, "k"); plot_(years, gk, "gk")
    }
  }*/
}