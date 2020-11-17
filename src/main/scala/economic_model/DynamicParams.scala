package economic_model

import wind_energy._
import solar_energy._
import utils._
import squants.energy._
import squants.time.Hours
import java.io.FileOutputStream
import scala.collection.mutable.ArrayBuffer

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
  def p(model: ModelResults) = {
    eta * model.C.last / model.Ce.last
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
  def ratio_spib_K(k: Double, z: Z_xi) = {
    (1 - z.xf + z.qf / gamma) / z.vf - ((1 - z.xf + z.qf / gamma) * z.ve / z.vf + z.xe + (1 - z.qe) / gamma) / k
  }
  // vp=vDy./vCe/ss-sg;
  def p(model: ModelResults) = (1 - s) / s * model.Df.last / model.Ce.last - 1 / (s * gamma)
}

class Dynamic_gk_eta(gK: Double, eta: Double) extends Dynamic_Params(gK, eta) {
  override def toString() = "Ex3"
  def y_int(z: Z_xi) = {
    val (k1, k2) = k_bounds(z)
    Interval((k1 - z.ve) / z.vf, (k2 - z.ve) / z.vf)
  }

  override def k_bounds(z: Z_xi): (Double, Double) = {
    val A1 = (1 - eta) / eta * A(z)
    val B1 = (1 - eta) / eta * B(z)
    //def coef(a: Double) = (a * (1 - z.qe + z.qf * z.ve / z.vf) + (1 - z.xf) * z.ve / z.vf + z.xe) / (a * z.qf / z.vf + (1 - z.xf) / z.vf - (gK + z.delta))  
    def coef(a: Double) = (a * (1 - z.qe + z.qf * z.ve / z.vf) + ((1 - z.xf) / z.vf - (z.deltaf - z.deltae)) * z.ve + z.xe) / (a * z.qf / z.vf + (1 - z.xf) / z.vf - (gK + z.deltaf))

    (coef(A1), coef(B1))
  }

  def ratio_spib_K(k: Double, z: Z_xi) = {
    val delta = z.deltaf + z.ve / k * (z.deltae - z.deltaf)
    gK + delta
  }
  override def s_k(k: Double, z: Z_xi) = {
    val delta = z.deltaf + z.ve / k * (z.deltae - z.deltaf)
    (1 - eta) * (gK + delta) / ((1 - z.xf) / z.vf - eta * (gK + delta) - ((1 - z.xf) / z.vf * z.ve + z.xe) / k)
  }
  def p(model: ModelResults) = eta * model.C.last / model.Ce.last
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
  def p(model: ModelResults): Double
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

  def A(z: Z_xi) = z.we / (1 - z.qe)
  def B(z: Z_xi) = (1 - z.wf) / z.qf;
  def C(z: Z_xi, k: Double) = {
    val y = (k - z.ve) / z.vf
    ((1 - z.xf) * y - z.xe) / (1 - z.qe - z.qf * y)
  }

  def p_int(z: Z_xi) = Interval(A(z), B(z))
  
  def simulate_int(calib: calibration_results_CI, scenario: Scenario, nyears: Int = 100, plot: Boolean = true, theta: Option[Double]=None): DynamicResults = {
    // Les variables qui sont résultats de la simulation sont ici, toutes les autres variables calculées sont dans ModelResults.
    val z0 = calib.z; val theta_k = theta.getOrElse((calib.k - k_bounds(z0)._1) / (k_bounds(z0)._2 - k_bounds(z0)._1))
    val k_interval = scala.collection.mutable.ArrayBuffer.empty[Interval];
    val gK, s, K, k, x, qf, ye = scala.collection.mutable.ArrayBuffer.empty[Double];
    val z = scala.collection.mutable.ArrayBuffer.empty[Z_xi];
    x += 0.0; qf += z0.qf; s += calib.s;

    val ye0 = calib.data.ye(calib.i).to(calib.energy_units)
    ye += ye0

    k_interval += k_int(z0, theta_k); k += k_int(z0, theta_k).mean
    // Le K de début de simulation ne correspond pas au K de la calibration de le cas où on se place sur la courbe max ! Il faut le faire correspondre avec k_max !
    K += calib.K //k.last * ye.last
    val mu = z0.ve / k.last
    val delta = mu * z0.deltae + (1 - mu) * z0.deltaf
    z += new Z_xi(z0.ve, z0.vf, z0.qe, z0.qf, z0.xe, z0.xf, z0.deltae, z0.deltaf, delta)
    gK += gK_k(k.last, z.last);
    // Variables de résultats -> à mettre à jour chaque année
    val model = new ModelResults(calib, this)
    model.update(k.last, K.last, z.last, s.last, ye.last, gK.last)

    println("Initialize " + k.last + " " + theta_k +  "(" + k_interval.last+ ")")
    val years = (1 until nyears).map(i => i + 2017).toList
    var end = false; var endYear: Option[Int] = None; var out_interval = false; var year_out: Option[Int] = None;
    // println(x.last + "\t" + k.last + "\t" + K.last + "\t" + gK.last + "\t" + model.mu.last + "\t" + ye.last)

    for (y <- years) {

      if (!end && k.last >= k_interval.last.mean && x.last > 0.9) {
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
          val k_x_fun = k_x_ye(qf.last, z0, theta_k, ye.last)
          val (x_x, qe_x, xe_x, ve_x, deltae_x) = x_fun_ye(ye.last, z0)
          val indexes_x = find_indexes(k.last, k_x_fun)
          val k1 = k_x_fun(indexes_x._1)._2.mean; val k2 = k_x_fun(indexes_x._2)._2.mean;
          val x1 = k_x_fun(indexes_x._1)._1; val x2 = k_x_fun(indexes_x._2)._1
          val newX = math.max(0, interpolation(k.last, (k1, x1), (k2, x2)))
          x += newX
          val ve = interpolation(x.last, (x1, ve_x(indexes_x._1)), (x2, ve_x(indexes_x._2)))
          val qe = interpolation(x.last, (x1, qe_x(indexes_x._1)), (x2, qe_x(indexes_x._2)))
          val xe = interpolation(x.last, (x1, xe_x(indexes_x._1)), (x2, xe_x(indexes_x._2)))
          val deltae = interpolation(x.last, (x1, deltae_x(indexes_x._1)), (x2, deltae_x(indexes_x._2)))
          val mu = ve / k.last
          z += Z_xi(ve, z0.vf, qe, qf.last, xe, z0.xf, deltae, z0.deltaf, mu * deltae + (1 - mu) * z0.deltaf)
        }

        k_interval += k_int(z.last, theta_k);
        gK += gK_k(k.last, z.last);
        s += s_k(k.last, z.last)

        if (z.last.xe / (1 - z.last.xf) > (1 - z.last.qe) / z.last.qf) println("Constraint on y violated" + "\t" + z.last + "\t" + model.p.last)
        // Calculated model parameters
        // !! We need the new delta before calculating gK
        model.update(k.last, K.last, z.last, s.last, ye.last, gK.last)
        //println(x.last + "\t" + k.last + "\t" + K.last + "\t" + gK.last+ "\t" + model.mu.last+ "\t" +ye.last)
        //println(y + "\t" + ye.last + "\t" + qf.last + "\t" + x.last + "\t" + k.last + "\t" + mean(k_interval.last, max) + "\t" + gk.last + "\t" + s.last + "\t" + eroi.last + "\t" + beta_k + "\t" + model.mu.last + "\t" + model.eta.last + "\t" + model.gamma.last + "\t" + model.p.last / calib.p + "\t" + "\t" + model.Ce.last / calib.ce(calib.i) + "\t" + model.Cf.last / calib.Cf)
        
      }
    }
    println(x.last + "\t" + k_interval.last.mean + "\t" + k.last + "\t" + gK.last + "\t" + model.mu.last + "\t" + z.last.delta + "\t" + s.last + "\t" + model.p.last)
    val years_new = if (!out_interval) years else (2017 until year_out.get + 2017).toList

    if (plot) {
      //  plotXY(List((years.map(_.toDouble), model.pib.toList, "pib"), (years.map(_.toDouble), model.I.toList, "I"),
      //     (years.map(_.toDouble), model.C.toList, "C")), yLabel = "", title = "pib_c_i", legend = true, int_x_axis = true)
      // plotXY(List((years_new.map(_.toDouble), model.C.toList, "C"), (years_new.map(_.toDouble), model.pCe, "pCe"),
      //   (years_new.map(_.toDouble), model.Cf.toList, "Cf")), yLabel = "", title = "c", legend = true, int_x_axis = true)
      //plotXY(x.toList, s.toList)
      plot_(years_new, x, "x_" + toString());
      plot_(years_new, gK, "gk_" + toString());
      // plot_(years_new, model.mu, "mu_" + toString())

      plotXY(List((years_new.map(_.toDouble), model.VAe.toList, "VAe"), (years_new.map(_.toDouble), model.VANe.toList, "VANe"),
        (years_new.map(_.toDouble), model.VAf.toList, "VAf"), (years_new.map(_.toDouble), model.VANf.toList, "VANf"),
        (years_new.map(_.toDouble), model.pib.toList, "PIB")), yLabel = "pib", title = "pib", legend = true, int_x_axis = true)

      //plot_(years_new, model.p, "p_" + toString())
      //plot_(years, model.gamma, "gamma_" + toString())
      // plot_(years, model.eta, "eta_" + toString())
      //plot_(years, s, "s_" + toString())
      // Plot interval and k mean   

      // plotXY(List((x.toList, k.toList, "k"), (x.toList, k_interval.toList.map(_.mean), "k_env"), (x.toList, k_interval.toList.map(_.min), "k_env" + "_min"), (x.toList, k_interval.toList.map(_.max), "k_env" + "_max")), yLabel = "k", title = "k", legend = true, int_x_axis = false)

    }
    DynamicResults(years_new.map(_.toInt), x.toList, k_interval.toList.map(k => k.mean), gK.toList, s.toList, ye.toList, z.toList, theta_k, endYear, model) // model.mu.toList, model.p.toList.map(i=> i / calib.p), model.Ce.toList.map(i => i / calib.ce(calib.i)), model.Cf.toList.map(i => i / calib.Cf))
  }

  def ye_fun(ye: Double) = {
    val l = getLines("potential_reduced", "\t")
    val index = find_index_list(ye, l.map(_(0).toDouble))
    val tildeK1 = l(index)(2).toDouble; val tildeK2 = l(index + 1)(2).toDouble;
    val tildeX1 = l(index)(1).toDouble; val tildeX2 = l(index + 1)(1).toDouble;
    val Ye1 = l(index)(0).toDouble; val Ye2 = l(index + 1)(0).toDouble;
    val newTildeK = tildeK1 + (ye - Ye1) * (tildeK2 - tildeK1) / (Ye2 - Ye1)
    val newTildeX = tildeX1 + (ye - Ye1) * (tildeX2 - tildeX1) / (Ye2 - Ye1)
    val lines = l.filter(i => (i(0).toDouble <= ye))

    (lines.map(i => (i(0).toDouble)) ++ List(ye),
      lines.map(i => (i(1).toDouble)) ++ List(newTildeX),
      lines.map(i => (i(2).toDouble)) ++ List(newTildeK),
      lines.map(i => (i(3).toDouble)) ++ List(1 / 25.0))
  }

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

  def k_x_ye(qf: Double, z0: Z_xi, beta: Double = 0.5, ye: Double): List[(Double, Interval)] = {
    val (x_x, qe_x, xe_x, ve_x, deltae_x) = x_fun_ye(ye, z0)
    (0 until x_x.size).toList.map(i => {
      // We should never use delta in the following calculations !!
      val z = new Z_xi(ve_x(i), z0.vf, qe_x(i), qf, xe_x(i), z0.xf, deltae_x(i), z0.deltaf, Double.NaN)
      (x_x(i), k_int(z, beta))
    })
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

  def find_indexes(k: Double, k_fun: List[(Double, Interval)]): (Int, Int) = {
    val index = {
      if (k < k_fun(0)._2.mean) 0
      else if (k > k_fun(k_fun.size - 1)._2.mean) k_fun.size - 1
      else (0 until k_fun.size - 1).toList.find(i => k_fun(i)._2.mean <= k &&k_fun(i + 1)._2.mean >= k).get
    }
    (index, math.min(k_fun.size - 1, index + 1))
  }
  def find_index(k: Double, k_fun: List[(Double, Interval)], max: Boolean): Int = {
    val res = {
      if (k < k_fun(0)._2.mean) 0
      else if (k > k_fun(k_fun.size - 1)._2.mean) k_fun.size - 1
      else (0 until k_fun.size - 1).toList.find(i => k_fun(i)._2.mean <= k && k_fun(i + 1)._2.mean >= k).get
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

  // DEPRACTED
  // Function that for a given x and qf gives de corresponding k
  /*
   *   def x_fun = {
    val lines = getLines("x_qe_xe_vefinal", "\t")
    (lines.map(i => (i(0).toDouble)),
      lines.map(i => (i(1).toDouble)),
      lines.map(i => (i(2).toDouble)),
      lines.map(i => (i(3).toDouble)))
   * def k_x(qf: Double, z0: Z_xi, beta: Double = 0.5): List[(Double, Interval)] = {
    val (x_x, qe_x, xe_x, ve_x) = x_fun
    (0 until x_x.size).toList.map(i => {
      val z = new Z_xi(ve_x(i), z0.vf, qe_x(i), qf, xe_x(i), z0.xf, z0.deltae, z0.deltaf)
      (x_x(i), k_int(z, beta))
    })
  } 
  }*/

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
}
