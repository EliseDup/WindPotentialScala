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
  // printScenario(bau, "BAU"); printScenario(new_policies, "NP"); printScenario(sustainable_dev, "SD")

  def printScenario(scn: Scenario, name: String) {
    println(name + "\t" + scn.ye.r1 + "\t" + scn.ye.r2 + "\t" + scn.qf.r1 + "\t" + scn.qf.r2 + "\t" + scn.ye.r_old + "\t" + scn.qf.r_old)
  }

  def main(args: Array[String]): Unit = {
   dyn_3.simulate_int(calib, bau, 50, false)
    dyn_3.simulate_int(calib, bau, 100, false)
    
    //dyn_1.simulate_int(calib, bau, 600, true)
    //qf_detailed_results(100, bau)
    //qf_detailed_results(100, sustainable_dev)
    //qf_detailed_results(100, new_policies)
    // plotDetailedResults(List(("res_BAU1", "BAU"), ("res_New Policies1", "NP"), ("res_Sustainable Dev1", "SD")))
  }

  def plotScenarios(dyn: Dynamic_Params, ny: Int, name: String = "") {
    val res = List((dyn.simulate_int(calib, bau, ny, false), "BAU"),
      (dyn.simulate_int(calib, new_policies, ny, false), "NP"),
      (dyn.simulate_int(calib, sustainable_dev, ny, false), "SD"))

    def years_pib(res: DynamicResults) = (1 until res.years.size).toList.map(res.years(_).toDouble)
    def years(res: DynamicResults) = res.years.map(_.toDouble)
    val gk_list = res.map(i => (years(i._1), i._1.gK.map(_ * 100), i._2))
    plotXY(gk_list, legend = true, yLabel = "gK [%]", title = "gk_" + name)
    val gpib_list = res.map(i => (years_pib(i._1), i._1.model.g_pib.map(_ * 100), i._2))
    plotXY(gpib_list, legend = true, yLabel = "gPIB [%]", title = "gpib_" + name)
    val x_list = res.map(i => (years_pib(i._1), i._1.x.map(_ * 100), i._2))
    plotXY(x_list, legend = true, yLabel = "x [%]", title = "x_" + name)
    val mu_list = res.map(i => (years_pib(i._1), i._1.model.mu.toList.map(_ * 100), i._2))
    plotXY(mu_list, legend = true, yLabel = "mu [%]", title = "mu_" + name)
    val s_list = res.map(i => (years_pib(i._1), i._1.s.map(_ * 100), i._2))
    plotXY(s_list, legend = true, yLabel = "s [%]", title = "s_" + name)

    val delta_list = res.map(i => (years_pib(i._1), i._1.z.map(_.delta * 100), i._2))
    plotXY(delta_list, legend = true, yLabel = "delta [%]", title = "delta_" + name)

    val eroi_list = res.map(i => (years_pib(i._1), i._1.model.eroi.toList, i._2))
    plotXY(eroi_list, legend = true, yLabel = "EROI", title = "eroi_" + name)
    val ner_list = res.map(i => (years_pib(i._1), i._1.model.ner.toList.map(_ * 100), i._2))
    plotXY(ner_list, legend = true, yLabel = "NER [%]", title = "ner_" + name)

    // Verif !
    /* val ye_list = res.map(i => (years_pib(i._1), i._1.ye.toList.map(_ * 100), i._2))
    plotXY(ye_list, legend = true, yLabel = "Ye [Mtoe]", title = "ye")
    val qf_list = res.map(i => (years_pib(i._1), i._1.z.toList.map(_.qf), i._2))
    plotXY(qf_list, legend = true, yLabel = "qf [toe/kUS$]", title = "qf")*/
  }

  def plotDetailedResults(names: List[(String, String)]) {
    //out.print(qf.qf.xf + "\t" + r.x.last + "\t" + r.k.last + "\t" + r.gK.last + "\t" + r.end_year.getOrElse(0.0) + "\t" + r.s.toList.max + "\t" + r.model.mu.last + "\t" + r.model.alpha.last + "\n")
    val list = names.map(name => (name._2, getLines(name._1, "\t")))
    // val qf_ratio = list.map(l => (l._1, l._2.map(_(0).toDouble/calib.qf)))
    val qf_x = list.map(l => (l._2.map(_(0).toDouble / calib.qf), l._2.map(_(1).toDouble * 100), l._1))
    val qf_gK = list.map(l => (l._2.map(_(0).toDouble / calib.qf), l._2.map(_(3).toDouble * 100), l._1))
    val qf_endYear = list.map(l => (l._2.map(_(0).toDouble / calib.qf), l._2.map(_(4).toDouble), l._1))
    val qf_s = list.map(l => (l._2.map(_(0).toDouble / calib.qf), l._2.map(_(5).toDouble * 100), l._1))
    val qf_mu = list.map(l => (l._2.map(_(0).toDouble / calib.qf), l._2.map(_(6).toDouble * 100), l._1))
    val qf_alpha = list.map(l => (l._2.map(_(0).toDouble / calib.qf), l._2.map(_(7).toDouble * 100), l._1))

    plotXY(qf_x, xLabel = "qf_f/qf_0", yLabel = "x [%]", legend = (qf_x.size > 1), title = "x")
    plotXY(qf_gK, xLabel = "qf_f/qf_0", yLabel = "gK [%]", legend = (qf_x.size > 1), title = "gK")
    plotXY(qf_mu, xLabel = "qf_f/qf_0", yLabel = "mu [%]", legend = (qf_x.size > 1), title = "mu")
    plotXY(qf_alpha, xLabel = "qf_f/qf_0", yLabel = "alpha [%]", legend = (qf_x.size > 1), title = "alpha")
    plotXY(qf_s, xLabel = "qf_f/qf_0", yLabel = "s [%]", legend = (qf_x.size > 1), title = "s")
    plotXY(qf_endYear, xLabel = "qf_f/qf_0", yLabel = "end Year", legend = (qf_x.size > 1), title = "endYear")

    // list.map(l => (l._1, l._2.map(_(0).toDouble/calib.qf),l._2.map(_(1).toDouble),l._2.map(_(2).toDouble),l._2.map(_(3).toDouble), l._2.map(_(4).toDouble),l._2.map(_(5).toDouble),l._2.map(_(6).toDouble),l._2.map(_(7).toDouble)))

    /* plotXY(List((qf_ratio, x,"")), xLabel="qf_f/qf_0", yLabel="x_stat")
    plotXY(List((qf_ratio, mu,"")), xLabel="qf_f/qf_0", yLabel="mu_stat")
    plotXY(List((qf_ratio, alpha,"")), xLabel="qf_f/qf_0", yLabel="alpha_stat")
    plotXY(List((qf_ratio, gK,"")), xLabel="qf_f/qf_0", yLabel="gK_stat") */

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
    res.map(r => println(r._1 + " & " + round(r._2.x.last * 100, 2) + " & " + round(r._2.gK.last * 100, 2) + " & " + round(r._2.model.g_pib(r._2.model.g_pib.size - 1) * 100, 2) + " & " +
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

    detailedResults(qfs, dyn_1, calib, scenario.name + "1", false)
    detailedResults(qfs, dyn_2b, calib, scenario.name + "2b", false)
    detailedResults(qfs, dyn_3, calib, scenario.name + "3", false)
  }

  def detailedResults(qfs: List[Scenario], dyn: Dynamic_Params, calib: calibration_results_CI, label: String, max: Boolean) {
    val out = new java.io.PrintStream(new java.io.FileOutputStream("res_" + label))
    val res = qfs.map(qf => {
      val r = dyn.simulate_int(calib, qf, 500, plot = false, max = max)
      out.print(qf.qf.xf + "\t" + r.x.last + "\t" + r.k.last + "\t" + r.gK.last + "\t" + r.end_year.getOrElse(0.0) + "\t" + r.s.toList.max + "\t" + r.model.mu.last + "\t" + r.model.alpha.last + "\t" + r.z.last.toString() + "\n")
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

  def plotYeQf {
    val t = (0 until 100).toList
    plotXY(List(
      (t.map(i => (i + 2017).toDouble), t.map(i => bau.ye_t(i)), "BAU"),
      (t.map(i => (i + 2017).toDouble), t.map(i => new_policies.ye_t(i)), "New Policies"),
      (t.map(i => (i + 2017).toDouble), t.map(i => sustainable_dev.ye_t(i)), "Sustainable Dev")), yLabel = "Ye [Mtoe]", legend = true, title = "Ye_scenarios")
    plotXY(List(
      (t.map(i => (i + 2017).toDouble), t.map(i => bau.qf_t(i)), "BAU"),
      (t.map(i => (i + 2017).toDouble), t.map(i => new_policies.qf_t(i)), "New Policies"),
      (t.map(i => (i + 2017).toDouble), t.map(i => sustainable_dev.qf_t(i)), "Sustainable Dev")), yLabel = "qf [toe/kUS$2010]", legend = true, title = "qf_scenarios")
  }
}

