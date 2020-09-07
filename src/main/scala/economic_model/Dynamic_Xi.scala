package economic_model

import wind_energy._
import solar_energy._
import utils._
import squants.energy._
import squants.time.Hours
import java.io.FileOutputStream

object Dynamic_Xi {
  import Helper._
  import PlotHelper._
  import GrowthModel._

  def main(args: Array[String]): Unit = {
    val calib = new calibration_results_CI
    println("Calib : k = " + calib.k + ", gk = " + calib.gk + ", p = " + calib.p + ", y = " + calib.y)
    //simulate_test(calib.k, calib.gk, calib.z, calib.s, calib.eta, calib.delta, 1 / 100.0, calib.qf / 2, true)
    //simulate_test_gamma(calib.k, calib.gk, calib.z, calib.s, calib.gamma, calib.delta, 1 / 100.0, calib.qf / 2, true)
    
    // simulate_x_s_eta(calib.z, calib.s, calib.eta, calib.delta, 1 / 100.0, calib.qf / 2, true)
    simulate_x_s_gamma(calib.z, calib.s, calib.gamma, calib.delta, 1 / 100.0, calib.qf / 2, true)

    // simulate_Ye(0.0)
  }

  def x_fun = {
    (getLines("x_qe_xe_ve", "\t").map(i => (i(0).toDouble)),
      getLines("x_qe_xe_ve", "\t").map(i => (i(1).toDouble)),
      getLines("x_qe_xe_ve", "\t").map(i => (i(2).toDouble)),
      getLines("x_qe_xe_ve", "\t").map(i => (i(3).toDouble)))
  }
  
  def qf_fun(qf_0 : Double, qf_max : Double, qf_rate : Double, T: Int) = {
    qf_max + (qf_0 - qf_max)*Math.pow(1 - qf_rate,T-1)
  }

  def bounds(a1: Double, a2: Double) = ((a1 + a2) / 2, List(a1, a2).min, List(a1, a2).max)

  def p_int(z: Z_xi) = bounds(z.xe / (1 - z.qe), (1 - z.xf) / z.qf)
  def y_int(z: Z_xi) = bounds(z.xe / (1 - z.xf), (1 - z.qe) / z.qf)
  def eroi_z(z: Z_xi) = 1 / (z.qe + (z.deltae * z.ve + z.xe) * z.qf)

  def y_int_s_eta(z: Z_xi, s: Double, eta: Double) = {
    val eta_prim = eta * (1 - s) / (1 - eta * (1 - s))
    val y1 = (1 + eta_prim) * z.xe / (z.xe * z.qf / (1 - z.qe) + eta_prim * (1 - z.xf))
    val y2 = ((1 - z.xf) * (1 - z.qe) / z.qf + eta_prim * z.xe) / ((1 + eta_prim) * (1 - z.xf))
    bounds(y1, y2)
  }
  def k_int_s_eta(z: Z_xi, s: Double, eta: Double) = {
    val y = y_int_s_eta(z, s, eta)
    val k1 = z.vf * y._2 + z.ve
    val k2 = z.vf * y._3 + z.ve
    bounds(k1, k2)
  }
  def k_int_s_gamma(z: Z_xi, s: Double, gamma: Double) = {
    val k1 = z.vf / z.qf * ((1 - gamma * (1 - s) * (1 - z.xf) / z.qf) * (1 - z.qe) + gamma * z.xe * (1 - s)) + z.ve
    val k2 = z.vf * (1 - z.qe) / (z.qf * (1 - gamma * (1 - s) * z.xe / (1 - z.qe)) + gamma * (1 - s) * (1 - z.xf)) + z.ve
    bounds(k1, k2)
  }

  def gk_k_s_eta(k: Double, z: Z_xi, s: Double, eta: Double, delta: Double) = {
    1 / z.vf * s / (1 - eta * (1 - s)) * ((1 - z.xf) * (1 - z.ve / k) - z.xe * z.vf / k) - delta
  }
  def gk_k_s_gamma(k: Double, z: Z_xi, s: Double, gamma: Double, delta: Double) = {
    s / (gamma * (1 - s)) * ((1 - z.qe + z.qf / z.vf * z.ve) / k - z.qf / z.vf) - delta
  }

  def gk_int_s_eta(z: Z_xi, s: Double, eta: Double, delta: Double) = bounds(gk_k_s_eta(k_int_s_eta(z, s, eta)._2, z, s, eta, delta), gk_k_s_eta(k_int_s_eta(z, s, eta)._3, z, s, eta, delta))
  def gk_int_s_gamma(z: Z_xi, s: Double, gamma: Double, delta: Double) = bounds(gk_k_s_gamma(k_int_s_eta(z, s, gamma)._2, z, s, gamma, delta), gk_k_s_gamma(k_int_s_eta(z, s, gamma)._3, z, s, gamma, delta))

  def simulate_test(k0: Double, gk0: Double, z0: Z_xi, s: Double, eta: Double, delta: Double, qf_rate: Double, qf_max: Double, plot: Boolean = false) {
    val (x_x, qe_x, xe_x, ve_x) = x_fun
    def k_x(qf: Double): List[(Double, (Double, Double, Double))] = {
      (0 until x_x.size).toList.map(i => {
        val z = new Z_xi(ve_x(i), z0.vf, qe_x(i), qf, xe_x(i), z0.xf, z0.deltae, z0.deltaf)
        (x_x(i), k_int_s_eta(z, s, eta))
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
      qf += qf_fun(z0.qf, qf_max, 1/100.0,y-2017)
      val k_x_fun = k_x(qf.last)
      val index_x = find_index(k.last, k_x(qf.last))
      x += x_x(index_x);
      z += Z_xi(ve_x(index_x), z0.vf, qe_x(index_x), qf.last, xe_x(index_x), z0.xf, z0.deltae, z0.deltaf)
      gk += gk_k_s_eta(k.last, z.last, s, eta, delta); // 1/z0.vf*s/(1-eta*(1-s))*((1-z0.xf)*(1-ve.last/k.last)-xe.last*z0.vf/k.last) - delta
      // eroi += eroi_z(z.last)
      if (z.last.xe / (1 - z.last.xf) > (1 - z.last.qe) / z.last.qf) println("Constraint on y violated" + "\t" + z.last)
      if(k.last < k_int_s_eta(z.last, s, eta)._2 || k.last > k_int_s_eta(z.last, s, eta)._3){
        println("k not in the interval " + k.last + "\t" + k_int_s_eta(z.last, s, eta))
      }
    }
    println("END - " + years.last + "\t" + " x = " + x.last + ", gk = " + gk.last * 100 + " k/k0 " + k.last / k0)

    if (plot) {
      plotXY(List((years.map(_.toDouble), k.toList, "k")), yLabel = "k", title = "k")
      plotXY(List((years.map(_.toDouble), x.toList, "")), yLabel = "x", title = "x")
      plotXY(List((years.map(_.toDouble), gk.toList, "gk")), yLabel = "gk", title = "gk")
      // plotXY(List((years.map(_.toDouble), qf.toList, "")), yLabel = "qf", title = "qf")

      //val p = z.toList.map(i => p_int(i))
      //plotXY(List((years.map(_.toDouble), p.map(_._1), "p"), (years.map(_.toDouble), p.map(_._2), "p_min"), (years.map(_.toDouble), p.map(_._3), "p_max")), yLabel = "p", title = "p", legend = true)
    }
  }
  def simulate_test_gamma(k0: Double, gk0: Double, z0: Z_xi, s: Double, gamma: Double, delta: Double, qf_rate: Double, qf_max: Double, plot: Boolean = false) {
    val (x_x, qe_x, xe_x, ve_x) = x_fun
    def k_x(qf: Double): List[(Double, (Double, Double, Double))] = {
      (0 until x_x.size).toList.map(i => {
        val z = new Z_xi(ve_x(i), z0.vf, qe_x(i), qf, xe_x(i), z0.xf, z0.deltae, z0.deltaf)
        (x_x(i), k_int_s_gamma(z, s, gamma))
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
      qf += qf_fun(z0.qf, qf_max, 1/100.0,y-2017)
      val k_x_fun = k_x(qf.last)
      val index_x = find_index(k.last, k_x(qf.last))
      x += x_x(index_x);
      z += Z_xi(ve_x(index_x), z0.vf, qe_x(index_x), qf.last, xe_x(index_x), z0.xf, z0.deltae, z0.deltaf)
      gk += gk_k_s_gamma(k.last, z.last, s, gamma, delta); // 1/z0.vf*s/(1-eta*(1-s))*((1-z0.xf)*(1-ve.last/k.last)-xe.last*z0.vf/k.last) - delta
      // eroi += eroi_z(z.last)
      if (z.last.xe / (1 - z.last.xf) > (1 - z.last.qe) / z.last.qf) println("Constraint on y violated" + "\t" + z.last)
      if(k.last < k_int_s_gamma(z.last, s, gamma)._2 || k.last > k_int_s_gamma(z.last, s, gamma)._3){
        println("k not in the interval " + k.last + "\t" + k_int_s_gamma(z.last, s, gamma))
      }
    }
    println("END - " + years.last + "\t" + " x = " + x.last + ", gk = " + gk.last * 100 + " k/k0 " + k.last / k0)

    if (plot) {
      plotXY(List((years.map(_.toDouble), k.toList, "k")), yLabel = "k", title = "k")
      plotXY(List((years.map(_.toDouble), x.toList, "")), yLabel = "x", title = "x")
      plotXY(List((years.map(_.toDouble), gk.toList, "gk")), yLabel = "gk", title = "gk")
      // plotXY(List((years.map(_.toDouble), qf.toList, "")), yLabel = "qf", title = "qf")

      //val p = z.toList.map(i => p_int(i))
      //plotXY(List((years.map(_.toDouble), p.map(_._1), "p"), (years.map(_.toDouble), p.map(_._2), "p_min"), (years.map(_.toDouble), p.map(_._3), "p_max")), yLabel = "p", title = "p", legend = true)
    }
  }
  def simulate_x_s_eta(z0: Z_xi, s: Double, eta: Double, delta: Double, qf_rate: Double, qf_max: Double, plot: Boolean = false) {
    val (x_x, qe_x, xe_x, ve_x) = x_fun
    def k_x(qf: Double): List[(Double, (Double, Double, Double))] = {
      (0 until x_x.size).toList.map(i => {
        val z = new Z_xi(ve_x(i), z0.vf, qe_x(i), qf, xe_x(i), z0.xf, z0.deltae, z0.deltaf)
        (x_x(i), k_int_s_eta(z, s, eta))
      })
    }
    val k, gk = scala.collection.mutable.ArrayBuffer.empty[(Double, Double, Double)];
    val k_mean, x, ve, qe, qf, xe, eroi = scala.collection.mutable.ArrayBuffer.empty[Double];
    val z = scala.collection.mutable.ArrayBuffer.empty[Z_xi];
    z += z0
    k += k_int_s_eta(z0, s, eta);
    gk += gk_int_s_eta(z0, s, eta, delta);
    x += 0.0; ve += z0.ve; qf += z0.qf; xe += z0.xe
    k_mean += k_int_s_eta(z0, s, eta)._1
    val years = (1 to 10).map(i => i + 2017).toList
    
    println("Initialize k = " + k.last + ", gk = " + gk.last + ", p = " + p_int(z0) + ", y = " + y_int(z0))

    for (y <- years) {
      k_mean += k.last._1 * (1 + gk.last._1)
      // vqy=qys+(qy-qys)*(1-.01).^(0:T-1)
      qf += qf_fun(z0.qf, qf_max, 1 / 100.0, y - 2017)  // qf_max + (qf.last - qf_max) * (1 - qf_rate)
      val k_x_fun = k_x(qf.last)
      val index_x = find_index(k_mean.last, k_x(qf.last))
      println(index_x + "\t" + k_mean.last)
      x += k_x_fun(index_x)._1;
      ve += ve_x(index_x)
      xe += xe_x(index_x)
      qe += qe_x(index_x)
      z += Z_xi(ve.last, z0.vf, qe.last, qf.last, xe.last, z0.xf, z0.deltae, z0.deltaf)
      k += k_int_s_eta(z.last, s, eta)
      gk += gk_int_s_eta(z.last, s, eta, delta); // 1/z0.vf*s/(1-eta*(1-s))*((1-z0.xf)*(1-ve.last/k.last)-xe.last*z0.vf/k.last) - delta
      eroi += eroi_z(z.last)
      if (z.last.xe / (1 - z.last.xf) > (1 - z.last.qe) / z.last.qf) println("Constraint on y violated" + "\t" + z.last)
      println(y + "\t" + k.last._1 + "\t" + k.last._2 + "\t" + k.last._3 + "\t" + k_mean.last + "\t" + gk.last + "\t" + x.last + "\t" + qf.last + "\t" + eroi.last)

    }
    println("END - " + years.last + "\t" + " x = " + x.last + ", gk = " + gk.last._1 * 100 + " k/k0 " + k.last._1 / k.toList(0)._1)

    if (plot) {
      plotXY(List((years.map(_.toDouble), k.toList.map(_._1), "k"), (years.map(_.toDouble), k.toList.map(_._2), "k_min"), (years.map(_.toDouble), k.toList.map(_._3), "k_max")), yLabel = "k", title = "k", legend = true)
      plotXY(List((years.map(_.toDouble), x.toList, "")), yLabel = "x", title = "x")
      plotXY(List((years.map(_.toDouble), gk.toList.map(_._1), "gk"), (years.map(_.toDouble), gk.toList.map(_._2), "gk_min"), (years.map(_.toDouble), gk.toList.map(_._3), "gk_max")), yLabel = "gk", title = "gk", legend = true)
      plotXY(List((years.map(_.toDouble), qf.toList, "")), yLabel = "qf", title = "qf")

      val p = z.toList.map(i => p_int(i))
      plotXY(List((years.map(_.toDouble), p.map(_._1), "p"), (years.map(_.toDouble), p.map(_._2), "p_min"), (years.map(_.toDouble), p.map(_._3), "p_max")), yLabel = "p", title = "p", legend = true)
    }
  }

  def simulate_x_s_gamma(z0: Z_xi, s: Double, gamma: Double, delta: Double, qf_rate: Double, qf_max: Double, plot: Boolean = false) {
    val (x_x, qe_x, xe_x, ve_x) = x_fun
    def k_x(qf: Double): List[(Double, (Double, Double, Double))] = {
      (0 until x_x.size).toList.map(i => {
        val z = new Z_xi(ve_x(i), z0.vf, qe_x(i), qf, xe_x(i), z0.xf, z0.deltae, z0.deltaf)
        (x_x(i), k_int_s_gamma(z, s, gamma))
      })
    }
    val k, gk = scala.collection.mutable.ArrayBuffer.empty[(Double, Double, Double)];
    val k_mean, x, ve, qe, qf, xe, eroi = scala.collection.mutable.ArrayBuffer.empty[Double];
    val z = scala.collection.mutable.ArrayBuffer.empty[Z_xi];
    z += z0
    k += k_int_s_gamma(z0, s, gamma);
    gk += gk_int_s_gamma(z0, s, gamma, delta)
    x += 0.0; ve += z0.ve; qf += z0.qf; xe += z0.xe
    k_mean += k_int_s_gamma(z0, s, gamma)._1
    val years = (1 to 10).map(i => i + 2017).toList

    println("Initialize k = " + k.last + ", gk = " + gk.last + ", p = " + p_int(z0))

    for (y <- years) {
      k_mean += k.last._1 * (1 + gk.last._1)
      // vqy=qys+(qy-qys)*(1-.01).^(0:T-1)
      qf += qf_fun(z0.qf, qf_max, 1 / 100.0, y - 2017) //qf_max + (qf.last - qf_max) * (1 - qf_rate)
      val k_x_fun = k_x(qf.last)
      val index_x = find_index(k_mean.last, k_x_fun)
      x += k_x_fun(index_x)._1;
      ve += ve_x(index_x)
      xe += xe_x(index_x)
      qe += qe_x(index_x)
      z += Z_xi(ve.last, z0.vf, qe.last, qf.last, xe.last, z0.xf, z0.deltae, z0.deltaf)
      k += k_int_s_gamma(z.last, s, gamma)
      gk += gk_int_s_gamma(z.last, s, gamma, delta) // 1/z0.vf*s/(1-eta*(1-s))*((1-z0.xf)*(1-ve.last/k.last)-xe.last*z0.vf/k.last) - delta
      eroi += eroi_z(z.last)
      if (z.last.xe / (1 - z.last.xf) > (1 - z.last.qe) / z.last.qf) println("Constraint on y violated" + "\t" + z.last)

      println(y + "\t" + k.last._1 + "\t" + k.last._2 + "\t" + k.last._3 + "\t" + k_mean.last + "\t" + gk.last + "\t" + x.last + "\t" + qf.last + "\t" + eroi.last)

    }
    println("END - " + years.last + "\t" + " x = " + x.last + ", gk = " + gk.last._1 * 100 + " k/k0 " + k.last._1 / k.toList(0)._1)
    if (plot) {
      plotXY(List((years.map(_.toDouble), k.toList.map(_._1), "k"), (years.map(_.toDouble), k.toList.map(_._2), "k_min"), (years.map(_.toDouble), k.toList.map(_._3), "k_max")), yLabel = "k", title = "k", legend = true)
      plotXY(List((years.map(_.toDouble), x.toList, "")), yLabel = "x", title = "x")
      plotXY(List((years.map(_.toDouble), gk.toList.map(_._1), "gk"), (years.map(_.toDouble), gk.toList.map(_._2), "gk_min"), (years.map(_.toDouble), gk.toList.map(_._3), "gk_max")), yLabel = "gk", title = "gk", legend = true)
      plotXY(List((years.map(_.toDouble), qf.toList, "")), yLabel = "qf", title = "qf")
      val p = z.toList.map(i => p_int(i))
      plotXY(List((years.map(_.toDouble), p.map(_._1), "p"), (years.map(_.toDouble), p.map(_._2), "p_min"), (years.map(_.toDouble), p.map(_._3), "p_max")), yLabel = "p", title = "p", legend = true)
    }
  }
  def find_index(k: Double, k_fun: List[(Double, (Double, Double, Double))]): Int = {
    if (k < k_fun(0)._2._1) 0
    else if (k > k_fun(k_fun.size - 1)._2._1) k_fun.size - 1
    else (0 until k_fun.size - 1).toList.find(i => k_fun(i)._2._1 <= k && k_fun(i + 1)._2._1 >= k).get
  }

  def _k(z: Z, s: Double, n: Double) = {
    val k1 = z.ve + z.vf / ((1 - s) * n / (1 + s * n) * z.vf / z.ve + (1 + n) / (1 + s * n) * z.qf / (1 - z.qe))
    val k2 = z.ve + z.vf / ((1 - s) * n / (1 + s * n) * z.lf / z.le + (1 + n) / (1 + s * n) * z.qf / (1 - z.qe))
    (k1 + k2) / 2
  }
  def _gk(k: Double, z: Z, s: Double, n: Double, delta: Double) = {
    1 / z.vf * (1 + n) * s / (1 + s * n) * (1 - z.ve / k) - delta
  }

  def simulate_Ye(qf_rate: Double) {

    val calib = new calibration_results_work; val s = calib.s; val n = calib.n; val z0 = calib.z
    // For a given Ye,r the corresponding ve,r and qe,r
    val ye_qe_ve = getLines("ye_qe_ve", "\t").map(i => (i(0).toDouble, i(1).toDouble, i(2).toDouble))
    // k = K/Ye => K = k*Ye = k(Ye)*Ye

    def ve_x(x: Double, ve_r: Double) = x * ve_r + (1 - x) * z0.ve
    def qe_x(x: Double, qe_r: Double) = x * qe_r + (1 - x) * z0.qe

    def K_Ye(x: Double, qf: Double) = {
      if (x == 0.005) {
        (0 until 20000).toList.map(i => (i.toDouble, z0.ve, i.toDouble * _k(z0, s, n)))
      } else {
        ye_qe_ve.map(i => {
          val z = new Z(ve_x(x, i._3), z0.vf, qe_x(x, i._2), qf, z0.le, z0.lf, z0.deltae, z0.deltaf)
          // For a given x, the total output is given by Ye,r/x
          val ye = i._1 / x
          (ye, ve_x(x, i._3), ye * _k(z, s, n))
        })
      }
    }
    def K_Ye_0(ye: Double, qf: Double) = {
      val z = new Z(z0.ve, z0.vf, z0.qe, qf, z0.le, z0.lf, z0.deltae, z0.deltaf)
      // For a given x, the total output is given by Ye,r/x 
      (ye, z0.ve, ye * _k(z, s, n))
    }

    val k, gk, K, Ye, ve, qf, x = scala.collection.mutable.ArrayBuffer.empty[Double];
    k += calib.k; K += calib.K; Ye += calib.data.ye(calib.i).to(MegaTonOilEquivalent); ve += calib.ve; x += 0.005
    gk += calib.gk; qf += calib.qf
    val years = (1 to 50).map(i => i + 2017).toList
    for (y <- years) {
      x += x.last * 1.11
      qf += qf.last * (1 - qf_rate)
      K += K.last * (1 + gk.last)
      val K_Ye_fun = K_Ye(x.last, qf.last)
      val index_ye = if (K.last < K_Ye_fun(0)._3) 0
      else if (K.last > K_Ye_fun(K_Ye_fun.size - 1)._3) K_Ye_fun.size - 1
      else (0 until K_Ye_fun.size - 1).toList.find(i => K_Ye_fun(i)._3 <= K.last && K_Ye_fun(i + 1)._3 >= K.last).get
      Ye += K_Ye_fun(index_ye)._1
      ve += K_Ye_fun(index_ye)._2
      k += K.last / Ye.last

      gk += 1 / z0.vf * (1 + n) * s / (1 + s * n) * (1 - ve.last / k.last) - calib.delta

      println(Ye.last + "\t" + index_ye)

    }

    plotXY(List((years.map(_.toDouble), Ye.toList, "")), yLabel = "ye", title = "ye")
    plotXY(List((years.map(_.toDouble), gk.toList, "")), yLabel = "gk", title = "gk")
    plotXY(List((years.map(_.toDouble), ve.toList, "")), yLabel = "ve", title = "ve")
    plotXY(List((years.map(_.toDouble), K.toList, "")), yLabel = "K", title = "K")
    plotXY(List((years.map(_.toDouble), k.toList, "")), yLabel = "k", title = "k")
    plotXY(List((years.map(_.toDouble), x.toList, "")), yLabel = "x", title = "x")

  }

}