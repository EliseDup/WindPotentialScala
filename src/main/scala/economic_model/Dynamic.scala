package economic_model

import wind_energy._
import solar_energy._
import utils._
import squants.energy._
import squants.time.Hours
import java.io.FileOutputStream

object Dynamic {
  import Helper._
  import PlotHelper._
  import GrowthModel._

  def main(args: Array[String]): Unit = {
    // val calib = new calibration_results_work
    // simulate_x(calib.z, calib.s, calib.n, calib.delta, 1 / 100.0)
    simulate_Ye(0.0)
  }

  def simulate_x(z0: Z, s: Double, n: Double, delta: Double, qf_rate: Double) {

    val x_qe_ve = getLines("x_qe_ve", "\t").map(i => (i(0).toDouble, i(1).toDouble, i(2).toDouble))

    def k_x(qf: Double) = {
      x_qe_ve.map(i => {
        val z = new Z(i._3, z0.vf, i._2, qf, z0.le, z0.lf, z0.deltae, z0.deltaf)
        (i._1, _k(z, s, n))
      })
    }
    val k, gk, x, ve, qf = scala.collection.mutable.ArrayBuffer.empty[Double];
    k += _k(z0, s, n); gk += _gk(k.last, z0, s, n, delta); x += 0.0; ve += z0.ve; qf += z0.qf;
    val years = (1 to 100).map(i => i + 2017).toList

    for (y <- years) {
      println(y + "\t" + k.last + "\t" + gk.last + "\t" + x.last + "\t" + qf.last)
      k += k.last * (1 + gk.last)
      qf += qf.last * (1 - qf_rate)
      val k_x_fun = k_x(qf.last)
      val index_x = if (k.last < k_x_fun(0)._2) 0
      else if (k.last > k_x_fun(k_x_fun.size - 1)._2) k_x_fun.size - 1
      else (0 until k_x_fun.size - 1).toList.find(i => k_x_fun(i)._2 <= k.last && k_x_fun(i + 1)._2 >= k.last).get

      x += k_x_fun(index_x)._1
      ve += x_qe_ve(index_x)._3
      gk += 1 / z0.vf * (1 + n) * s / (1 + s * n) * (1 - ve.last / k.last) - delta
    }

    plotXY(List((years.map(_.toDouble), x.toList, "")), yLabel = "x", title = "x")
    plotXY(List((years.map(_.toDouble), gk.toList, "")), yLabel = "gk", title = "gk")
    plotXY(List((years.map(_.toDouble), k.toList, "")), yLabel = "k", title = "k")
    plotXY(List((years.map(_.toDouble), qf.toList, "")), yLabel = "qf", title = "qf")

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
      x += x.last*1.11
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
  def _k(z: Z, s: Double, n: Double) = {
    val k1 = z.ve + z.vf / ((1 - s) * n / (1 + s * n) * z.vf / z.ve + (1 + n) / (1 + s * n) * z.qf / (1 - z.qe))
    val k2 = z.ve + z.vf / ((1 - s) * n / (1 + s * n) * z.lf / z.le + (1 + n) / (1 + s * n) * z.qf / (1 - z.qe))
    (k1 + k2) / 2
  }
  def _gk(k: Double, z: Z, s: Double, n: Double, delta: Double) = {
    1 / z.vf * (1 + n) * s / (1 + s * n) * (1 - z.ve / k) - delta
  }

}