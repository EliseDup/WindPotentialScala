package economic_model

import wind_energy._
import solar_energy._
import utils._
import squants.energy._
import squants.time.Hours
import java.io.FileOutputStream

object ResultsPaper {
  import Helper._
  import PlotHelper._
  import GrowthModel._

  def main(args: Array[String]): Unit = {
    // Sensitivity analysis table
    //val as = List(0.04, 0.06, 0.08); val mus = List(0.03, 280.0 / 4280, 0.1)
    //pinAnalysis(as,mus)
    //Calibration.printTableCalibration_simple(2017, as, mus)
    // Exercice 1 to 3
    val cal = new calibration_results_work(energy_units = MegaTonOilEquivalent, pib_units = 1E9.toInt, pop_units = 1E6.toInt)

    val share = (0 to 4).map(_ * 0.25).toList
    val lf_f = cal.lf * math.pow(1 - 1.34 / 100, 33)
    val qf_f = cal.qf * math.pow(1 - 1.06 / 100, 33)
    println(lf_f + "\t" + qf_f)
    // resultsExercices(share, cal.qf,cal.lf)
    println("Results " + qf_f + "\t" + lf_f)
    resultsExercices(share, qf_f, lf_f)
    println("Results " + cal.qf + "\t" + cal.lf)
    resultsExercices(share, cal.qf, cal.lf)
    // 

  }

  def pinAnalysis(as: List[Double], mus: List[Double]) {
    // Pin analysis
    val z = mus.map(mu => as.map(a => (a, mu, ModelResolution(new calibration_results_work(2017, alpha = a, mu = mu))))).flatten
    /* z.map(i => println(i._1 + "\t" + i._2 + "\t" +
        i._3.z + "\t" + i._3.ye.to(i._3.energy_units) + "\t"  + i._3.p + "\t" + i._3.ce.to(i._3.energy_units) + "\t"
        +i._3.perte1 + "\t" + i._3.perte2 + "\t" + i._3.perte3 + "\t" +
        i._3.PIN1 + "\t" + i._3.PIN2 + "\t" + i._3.PIN3  + "\t" + i._3.PIN4))*/
    print("$mu$ [%]"); z.map(cal => print(" & " + (cal._2 * 100).toInt)); println(" \\" + "\\")
    print("$alpha$ [%]"); z.map(cal => print(" & " + (cal._1 * 100).toInt)); println(" \\" + "\\")
    print("$EROI$"); z.map(cal => print(" & " + round(cal._3.eroi))); println(" \\" + "\\")
    print("$NER$"); z.map(cal => print(" & " + round(cal._3.ner * 100))); println(" \\" + "\\")

    print("$1 - 1/epsilon$ [%]"); z.map(cal => print(" & " + round(cal._3.perte1 * 100))); println(" \\" + "\\")
    print("$1 - P_1$ [%]"); z.map(cal => print(" & " + round(cal._3.perte2 * 100))); println(" \\" + "\\")
    print("$1-P_1-P_2$ [%]"); z.map(cal => print(" & " + round(cal._3.perte3 * 100))); println(" \\" + "\\")

    print("$PIN_1$ "); z.map(cal => print(" & " + round(cal._3.PIN1 / 1E9).toInt)); println(" \\" + "\\")
    print("$PIN_2$"); z.map(cal => print(" & " + round(cal._3.PIN2 / 1E9, 0).toInt)); println(" \\" + "\\")
    print("$PIN_3$ "); z.map(cal => print(" & " + round(cal._3.PIN3 / 1E9, 0).toInt)); println(" \\" + "\\")
    print("$PIN_4$"); z.map(cal => print(" & " + round(cal._3.PIN4 / 1E9, 0).toInt)); println(" \\" + "\\")

    printPinLosses(z.map(_._3))
  }
  def printPinLosses(res: List[ModelResolution]) {
    print("Pertes CIK SE "); res.map(r => print(" & " + round(100 * ((r.PIN1 - r.PIN2) / r.PIN1)))); println(" \\" + "\\")
    print("+ Pertes CIK SF"); res.map(r => print(" & " + round(100 * ((r.PIN2 - r.PIN3) / r.PIN1)))); println(" \\" + "\\")
    print("+ Pertes $C_e$"); res.map(r => print(" & " + round(100 * ((r.PIN3 - r.PIN4) / r.PIN1)))); println(" \\" + "\\")

    println(" Pertes d'$ub$"); res.map(r => print(" & " + round(100 * (r.xi * (1 - r.z.deltaf * r.z.vf))))); println(" \\" + "\\")
    println("Compensation $pC_e$"); res.map(r => print(" & " + round(100 * (r.xi * r.p * r.z.qf)))); println(" \\" + "\\")
    print("Pertes totals"); res.map(r => print(" & " + round(100 * ((r.PIN1 - r.PIN4) / r.PIN1)))); println(" \\" + "\\")
  }

  def resultsExercices(share: List[Double] = (0 to 10).map(_ * 0.1).toList,
    qf: Double, lf: Double, net: Boolean = false, e_units: EnergyUnit = MegaTonOilEquivalent, pib_units: Int = 1E9.toInt) {

    val cal = new calibration_results_work(energy_units = e_units, pib_units = pib_units, pop_units = 1E6.toInt);
    val target = if (net) cal.data.ye(cal.i) * (1 - cal.qe - cal.delta_e * cal.qf * cal.ve) else cal.data.ye(cal.i)
    println("Results Exercices " + target.to(MegaTonOilEquivalent) + " Mtoe")

    // val share = (1 to 5).map(_ * 0.2).toList
    // Jacobson scenario for 2050: 
    // 14.89% rooftop residential + 11.58 % commercial
    // 21.36 % pv power plants  
    // 9.72% CSP
    // 23.52% Onshore wind
    // 13.62% Offshore wind
    // Total = 94.69 / 100 
    val techs = List((OnshoreWindTechnology, 23.52 / 94.69), (OffshoreWindTechnology, 13.62 / 94.69), (PVPoly, (14.89 + 11.58 + 21.36) / 94.69), (CSPParabolic, 9.72 / 94.69))
    val res = share.map(s => (s, calculate(target, net, techs, s, cal, qf, lf)))

    val out_stream = new java.io.PrintStream(new java.io.FileOutputStream("zs_" + System.currentTimeMillis()))
    res.map(r => out_stream.println(r._1 + "\t" + r._2))
    out_stream.close()
    printResultsExercices(res)
  }
  def printResultsExercices(res: List[(Double, Z)]) {
    val cal = new calibration_results_work()
    val plot_res = res.map(i => new ImpactPER(i._2))
    val zs = res.map(_._2)
    val share = res.map(_._1)
    // res.map(r => printResults(r._2, r._1, cal.s, cal.n, cal.gk, cal.c))
    printCommonResults(res);
    printExercice1(res, cal.s, cal.n)
    printExercice2(res, cal.gk, cal.n)
    printExercice3(res, cal.gk, cal.c)

    // Plot enveloppe g, s, s
    plotXY(List((share.map(_ * 100), plot_res.map(x => x.gk(cal.s, cal.n)._1).map(_ * 100), "Mean"),
      (share.map(_ * 100), plot_res.map(x => x.interval_gk(cal.s, cal.n)._1).map(_ * 100), "Min"),
      (share.map(_ * 100), plot_res.map(x => x.interval_gk(cal.s, cal.n)._2).map(_ * 100), "Max")), xLabel = "RE Share [%]", yLabel = "gk [%] (s, n fixed)", title = "gk_(s_n_fixed)")
    plotXY(List((share.map(_ * 100), plot_res.map(x => x.s_s(cal.gk, cal.n)._1).map(_ * 100), "Mean"),
      (share.map(_ * 100), plot_res.map(x => x.interval_s(cal.gk, cal.n)._1).map(_ * 100), "Min"),
      (share.map(_ * 100), plot_res.map(x => x.interval_s(cal.gk, cal.n)._2).map(_ * 100), "Max")), xLabel = "RE Share [%]", yLabel = "s [%] (gk, n fixed)", title = "s_(gk_n_fixed)")
    plotXY(List((share.map(_ * 100), plot_res.map(x => x.s_c(cal.gk, cal.c)._1).map(_ * 100), "Mean"),
      (share.map(_ * 100), plot_res.map(x => x.interval_s_c(cal.gk, cal.c)._1).map(_ * 100), "Min"),
      (share.map(_ * 100), plot_res.map(x => x.interval_s_c(cal.gk, cal.c)._2).map(_ * 100), "Max")), xLabel = "RE Share [%]", yLabel = "s [%] (gk, c fixed)", title = "s_(c_fixed)")
    // Print different pin level 
    def printPINs(z: (Z, Energy)) {
      val res = new ModelResolution(z._1, z._2, cal.energy_units, cal.L, cal.s, cal.n)
      println(round(res.eroi) + " & " +
        round(res.PIN1, 0) + " & " + round(res.PIN2, 0) + " & " + round(res.PIN3, 0) + " & " + round(res.PIN4, 0) + " & " +
        round(res.perte1 * 100) + " & " + round(res.perte2 * 100) + " & " + round(res.perte3 * 100))
    }
  }
  def printResults(z: Z, share_re: Double, s_fixed: Double, n_fixed: Double, gk_fixed: Double, c_fixed: Double) {
    val res = new ImpactPER(z)
    val mu_gk = res.mu_gk(s_fixed, n_fixed)
    val k_gk = res.k_gk(s_fixed, n_fixed)
    val gk = res.gk(s_fixed, n_fixed)
    val mu_c = res.mu_c(gk_fixed, c_fixed)
    val mu_s = res.mu_s(gk_fixed, n_fixed)
    val k_s = res.k_s(gk_fixed, n_fixed)
    val s = res.s_s(gk_fixed, n_fixed)
    val k_c = res.k_mu(mu_c)
    val n_c = res.n_c(c_fixed)
    val s_c = res.s_c(gk_fixed, c_fixed)

    println(round(share_re * 100, 0) + " & " +
      round(res.eroi) + " & " + round(100 * z.qe, 2) + " & " + round(z.ve) + " & " + round(res.p._1) + " & " + round(res.p._2) + " & " +
      round(100 * mu_gk._1) + " & " + round(100 * mu_gk._2) + " & " +
      round(100 * k_gk._1) + " & " + round(100 * k_gk._2) + " & " +
      round(100 * gk._1) + " & " + round(100 * gk._2) + " & " +
      round(100 * mu_s._1) + " & " + round(100 * mu_s._2) + " & " +
      round(100 * k_s._1) + " & " + round(100 * k_s._2) + " & " +
      round(100 * s._1) + " & " + round(100 * s._2) + " & " +
      round(100 * mu_c) + " & " + round(100 * k_c) + " & " + round(100 * n_c._1) + " & " + round(100 * n_c._2) + " & " + round(100 * s_c._1) + " & " + round(100 * s_c._2) + "\\" + "\\")
  }
  def printCommonResults(zs: List[(Double, Z)]) {
    println(" --- Common Results ---")
    println("x & epsilon & q_e & v_e & bar{p} & sigma_p & NER & Pertes CIK SE & Pertes CIK SF \\")
    zs.map(zz => {
      val z = zz._2
      val res = new ImpactPER(z)
      println(round(100 * zz._1, 0).toInt + " &  " + round(100 * z.qe) + " & " + round(z.ve) + " & " + round(res.p._1) + " & " + round(res.p._2) + " & " +
        round(res.eroi) + " & " + round(100 * res.ner) + " & " +
        round(100 * (1 / res.eroi)) + "& " +
        round(100 * (1 - res.ner - 1 / res.eroi)) + " \\" + "\\")
    })
  }
  // mu, k, gk, s, n
  def printExercice1(zs: List[(Double, Z)], s_fixed: Double, n_fixed: Double) {
    println(" --- Results Exercice 1 --- ")
    zs.map(zz => {
      val z = zz._2
      val res = new ImpactPER(z)
      println(round(100 * zz._1, 0).toInt + " &  " + round(100 * res.mu_gk(s_fixed, n_fixed)._1) + " & " + round(100 * res.mu_gk(s_fixed, n_fixed)._2) + " & " +
        round(res.k_gk(s_fixed, n_fixed)._1) + " & " + round(res.k_gk(s_fixed, n_fixed)._2) + " & " +
        round(100 * res.gk(s_fixed, n_fixed)._1) + " & " + round(100 * res.gk(s_fixed, n_fixed)._2) + " & " +
        round(100 * s_fixed) + " & - &" + round(100 * n_fixed) + " & - & " +
        round(res.v_gk(s_fixed, n_fixed)._1) + " & " + round(res.v_gk(s_fixed, n_fixed)._2) + " \\" + "\\")
    })
  }
  def printExercice2(zs: List[(Double, Z)], gk_fixed: Double, n_fixed: Double) {
    println(" --- Results Exercice 2 --- ")
    zs.map(zz => {
      val z = zz._2
      val res = new ImpactPER(z)
      println(round(100 * zz._1, 0).toInt + " &  " + round(100 * res.mu_s(gk_fixed, n_fixed)._1) + " & " + round(100 * res.mu_s(gk_fixed, n_fixed)._2) + " & " +
        round(res.k_s(gk_fixed, n_fixed)._1) + " & " + round(res.k_s(gk_fixed, n_fixed)._2) + " & " +
        round(100 * gk_fixed) + " & - & " +
        round(100 * res.s_s(gk_fixed, n_fixed)._1) + " & " + round(100 * res.s_s(gk_fixed, n_fixed)._2) + " & " + round(100 * n_fixed) + " & - & " +
        round(res.v_s(gk_fixed, n_fixed)._1) + " & " + round(res.v_s(gk_fixed, n_fixed)._2) + " \\" + "\\")

    })
  }
  def printExercice3(zs: List[(Double, Z)], gk_fixed: Double, c_fixed: Double) {
    println(" --- Results Exercice 3 --- ")
    zs.map(zz => {
      val z = zz._2
      val res = new ImpactPER(z)
      println(round(100 * zz._1, 0).toInt + " &  " + round(100 * res.mu_c(gk_fixed, c_fixed)) + " & - & " + round(res.k_mu(res.mu_c(gk_fixed, c_fixed))) + " & - & " +
        round(100 * gk_fixed) + " & - & " + round(100 * res.s_c(gk_fixed, c_fixed)._1) + " & " + round(100 * res.s_c(gk_fixed, c_fixed)._2) + " & " +
        round(100 * res.n_c(c_fixed)._1) + " & " + round(100 * res.n_c(c_fixed)._2) + " & " +
        round(res.v_c(gk_fixed, c_fixed)._1) + " & " + round(res.v_c(gk_fixed, c_fixed)._2) + " \\" + "\\")

    })
  }

  def plotParametersHistory {
    import Calibration._

    println((growth_rates(qf)).sum / (growth_rates(qf).size))
    println((growth_rates(lf)).sum / (growth_rates(lf).size))
    val gv = cals.map(_.gv)
    (0 until year_double.size).map(y => println(data.g(y) + "\t" + data.s(y) + "\t" + gv(y) + "\t" + v(y)))
    (0 until year_double.size).map(y => println(data.year(y) + "\t" + v(y) + "\t" + qe(y) + "\t" + qf(y) + "\t" + ve(y) + "\t" + vf(y) + "\t" + le(y) + "\t" + lf(y)))
    val res = List((qe, "qe"), (qf, " qf"), (ve, "ve"), (vf, "vf"), (le.map(_ * 1E6), "le"), (lf.map(_ * 1E6), "lf"), (eroi, "eroi"), (ner, "ner"))

    import CalibrationData._
    res.map(r => plotXY(List((year_double, r._1, "")), yLabel = r._2, title = r._2))
    val year_growth = (1 until year_double.size).map(year_double(_)).toList
    plotXY(List((year_growth, CalibrationData.smooth_double(growth_rates(qf), (true, 10)).map(_ * 100), "")), yLabel = "qf growth rate [%/y]")
    plotXY(List((year_growth, CalibrationData.smooth_double(growth_rates(lf), (true, 10)).map(_ * 100), "")), yLabel = "lf growth rate [%/y]")

  }

}