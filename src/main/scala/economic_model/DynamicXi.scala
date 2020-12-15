package economic_model

import wind_energy._
import solar_energy._
import utils._
import squants.energy._
import squants.time.Hours
import java.io.FileOutputStream
import scala.collection.mutable.ArrayBuffer
import wind_solar.Grid
import com.sun.org.apache.bcel.internal.generic.NEW

object DynamicXi {

  import PlotHelper._
  import Helper._

  //val calib = new calibration_results_CI(year = 2017, energy_units = MegaTonOilEquivalent)
  val calib = new calibration_results_CI(year = 2017, energy_units = MegaTonOilEquivalent)

  val dyn_1 = new Dynamic_s_eta(calib.s, calib.eta)
  val dyn_2b = new Dynamic_s_gamma_b(calib.s, calib.gammab)
  val dyn_3 = new Dynamic_gk_eta(calib.gK, calib.eta)

  val ye_0 = calib.ye.to(calib.energy_units)
  val qf_0 = calib.qf; val qf_f = calib.qf * 0.5; val ye_f = 3 * ye_0
  // Based on history : Ye +1.16% / year, qf - 0.7 % / year
  val t_lim = 2040 - calib.year
  val ye_40_bau = ye_0 * 1.3 // math.pow(1 + 0.0116, t_lim)

  val bau = new Scenario(new ParamsScenario(qf_0, qf_0 * 0.85, qf_f, t_lim), new ParamsScenarioLogistic(ye_0, ye_40_bau, ye_f, t_lim), "BAU")
  val bau_old = new Scenario(new ParamsScenario(qf_0, qf_0 * 0.85, qf_f, t_lim), new ParamsScenarioLogistic(ye_0, 13986.655116005899, ye_f, t_lim), "BAU_old")
  // val np = new Scenario(new ParamsScenario(qf_0, qf_0 * 0.58 * 0.85 / 0.64, qf_f, t_lim), new ParamsScenarioLogistic(ye_0, 12581, ye_f, t_lim), "NP")
  val sd = new Scenario(new ParamsScenario(qf_0, qf_0 * 0.85 * 0.45 / 0.64, qf_f, t_lim), new ParamsScenario(ye_0, ye_0, ye_0, t_lim), "SD")

  def printScenario(scn: Scenario, name: String) {
    println(name + "\t" + " ye " + scn.ye + ", qf " + scn.qf)
  }

  def main(args: Array[String]): Unit = {
    /* */
    // dyn_1.simulate_int(calib, bau, 600, false, Some(1.0))
    //dyn_2b.simulate_int(calib, bau, 250, false, Some(1.0))
    //dyn_3.simulate_int(calib, bau, 50, false, false)
    //dyn_3.simulate_int(calib, bau, 91, false, false)
    //  dyn_2b.simulate_int(calib, bau, 10,false)
    //  dyn_3.simulate_int(calib, bau, 10,false)

    //dyn_1.simulate_int(calib, bau, 600, true)
    //qf_detailed_results(100, bau)
    //qf_detailed_results(100, sustainable_dev)
    //qf_detailed_results(100, new_policies)
    // plotDetailedResults(List(("res_BAU1", "BAU"), ("res_New Policies1", "NP"), ("res_Sustainable Dev1", "SD")))
  }

  def plotScenarios(dyn: Dynamic_Params, ny: Int, name: String = "") {
    val res = List((dyn.simulate_int(calib, bau, ny, false), "BAU"),
      // (dyn.simulate_int(calib, np, ny, false), "NP"),
      (dyn.simulate_int(calib, sd, ny, false), "SD"))

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
      //  ("NP", dyn.simulate_int(calib, np, nyears = ny, plot = false)),
      ("SD", dyn.simulate_int(calib, sd, nyears = ny, plot = false)))
    println("--Results--")
    println("\t" + "x" + "\t" + "gk" + "\t" + "gpib" + "\t" + "mu" + "\t" + "alpha")
    res.map(r => println(r._1 + " & " + round(r._2.x.last * 100, 2) + " & " + round(r._2.gK.last * 100, 2) + " & " + round(r._2.model.g_pib(r._2.model.g_pib.size - 1) * 100, 2) + " & " +
      round(r._2.model.mu.last * 100, 2) + " & " + round(r._2.model.alpha.last * 100, 2) + "\\" + "\\"))
    println("If ex 3 :")
    res.map(r => println(r._1 + " & " + round(r._2.x.last * 100, 2) + " & " +
      round(r._2.s.max * 100, 2) + " & " +
      round(r._2.model.mu.last * 100, 2) + " & " + round(r._2.model.alpha.last * 100, 2) + "\\" + "\\"))

  }

}

