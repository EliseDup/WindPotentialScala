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
 EROISocietalPaper   
    
  }

  def exercice {
    // Exercice 1 to 3

    val cal = new calibration_results_work(energy_units = MegaTonOilEquivalent, pib_units = 1E9.toInt, pop_units = 1E6.toInt)

    val share = (0 to 4).map(_ * 0.25).toList
    val qf_2050 = cal.qf * math.pow(1 - 1.06 / 100, 33)
    val lf_2050 = cal.lf * math.pow(1 - 1.34 / 100, 33)

    println("qf 2050 " + qf_2050 + "\t" + " lf 2050 " + lf_2050)
    val T = (30.0 / share.size).toInt
    val share_qf = (0 until share.size).map(i => (share(i), qf_2050, lf_2050)).toList //cal.qf * math.pow(1 - 1.06 / 100, T * i), cal.lf * math.pow(1 - 1.34 / 100, T * i))).toList
    val share_qf0 = (0 until share.size).map(i => (share(i), cal.qf, cal.lf)).toList
    //resultsExercices(share_qf)
    //resultsExercices(share_qf0)
    val res2017 = getLines("zs_2017", "\t").map(l => (l(0).toDouble, Z((1 to 8).toList.map(i => l(i).toDouble))))
    val res2050 = getLines("zs_2050", "\t").map(l => (l(0).toDouble, Z((1 to 8).toList.map(i => l(i).toDouble))))

    printResultsExercices(res2017)
    printResultsExercices(res2050)
  }
  def resultsExercices(share: List[(Double, Double, Double)], net: Boolean = false, e_units: EnergyUnit = MegaTonOilEquivalent, pib_units: Int = 1E9.toInt) {

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
    val res = share.map(s => (s._1, calculate(target, net, techs, s._1, cal, s._2, s._3)))

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
    println("x & q_e & v_e & bar{p} & sigma_p & EROI & NER & Pertes CIK SE & Pertes CIK SF \\")
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
        round(res.v_gk(s_fixed, n_fixed)._1) + " & " + round(res.v_gk(s_fixed, n_fixed)._2) +
        " & " + round(res.c_tot_gk(s_fixed, n_fixed)._1) + " & " + round(res.c_tot_gk(s_fixed, n_fixed)._2) +
        " \\" + "\\")
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
        round(res.v_s(gk_fixed, n_fixed)._1) + " & " + round(res.v_s(gk_fixed, n_fixed)._2) +
        " & " + round(res.c_tot_s(gk_fixed, n_fixed)._1) + " & " + round(res.c_tot_s(gk_fixed, n_fixed)._2)
        + " \\" + "\\")

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
        round(res.v_c(gk_fixed, c_fixed)._1) + " & " + round(res.v_c(gk_fixed, c_fixed)._2) +
        " & " + round(res.c_tot_c(gk_fixed, c_fixed)._1) + " & " + round(res.c_tot_c(gk_fixed, c_fixed)._2) + " \\" + "\\")

    })
  }

  def EROISocietalPaper {
    sensitivityAnalysis
    // plotParametersHistory
  }
  def sensitivityAnalysis {
    val cal = new calibration_results_CI
    println(cal.yf + "\t" + cal.Xe + "\t" + cal.Xf + "\t" + cal.Cf + "\t" + cal.s*cal.pib)
    println("ETA min " + cal.alpha*cal.data.ce(cal.i)/((1-cal.s)*cal.data.e(cal.i)) )
    println((cal.xe+cal.delta_e*cal.ve)*cal.qf)
    println("Ee :" + 100*cal.eroi*cal.qe + ", Xe " + 100*cal.eroi*cal.xe*cal.qf + ", Ke " + 100*cal.eroi*cal.qf*cal.delta_e*cal.ve)

    println("---------- Tableau 2 ----------")
    println(round(cal.vf) + " & " + round(cal.qf) + " & " +round(cal.xf) + " & " + round(cal.ve) + " & " + round(cal.qe) + " & " + + round(cal.xe) + " & " + round(cal.v) + " & " + round(cal.eroi) + " & " + round(cal.ner))
    println("---------- Tableau 3 ----------")
    println("$P_1$" + " & " + round(cal.p1 * 100) + "\\" + "\\")
    println("CIK SE" + " & " + round((1 / cal.eroi) * 100) + "\\" + "\\")
    println("CIK SF" + " & " + round((cal.p1 - 1 / cal.eroi) * 100) + "\\" + "\\")
    println("$P_2$" + " & " + round(cal.p2 * 100) + "\\" + "\\")
    println("Pertes d'$ub$" + " & " + round(cal.xi * (1 - (cal.xf+cal.delta_f * cal.vf)) * 100) + "\\" + "\\")
    println("-Compensation $pC_e$" + " & " + round((cal.xi * cal.p * cal.qf) * 100) + "\\" + "\\")
    println("P_1 + P_2$" + " & " + round(100 * (cal.p1 + cal.p2)) + "\\" + "\\")
    
    val zfs = List(0.4,0.5,0.6); val etas = List(0.03, 0.04, 0.05)
    val as = List(0.04, 0.06, 0.08); val mus = List(0.03, 280.0 / 4280, 0.1);
    val tfs = List(15, 20, 25); val tes = List(20, 25, 30);
    println("---------- Tableau 5 ----------")
    Calibration.printTableCalibrationCI(2017,List(0.06), etas,zfs, List(280.0 / 4280))
  
    Calibration.printTableCalibrationCI(2017,as, List(0.04), List(0.5), List(280.0 / 4280))
  Calibration.printTableCalibrationCI(2017,List(0.06), List(0.04), List(0.5), mus)
  
  }

  def plotParametersHistory {
    import Calibration._
    val index = List(0, 5, 10, 15, 20, 25, 27)
    /*index.map(y => {
      println(data.year(y) + " & " + round(eroi(y), 2) + " & " + round(ner(y) * 100, 2)
        + " & " + round(perte1(y) * 100, 2) + " & " + round((perte2(y) - perte1(y)) * 100, 2) + "\\" + "\\")
    })*/
    // plotXY(List((year_double, alphas,"alpha"),(year_double, etas,"eta")))
    val res = List((qe.map(_*100), "qe [%]"), (qf, "qf"), (ve, "ve"), (vf, "vf"),(xe,"xe"),(xf,"xf"), (eroi, "EROI"), (ner.map(_*100), "NER [%]"),
      (v, "v"), (p, "p"))
      
       println("mean qf growth rate = " + round(growth_rates(qf).sum / (growth_rates(qf).size)))
    println("mean lf growth rate = " + round(growth_rates(lf).sum / (growth_rates(lf).size)))

    combinedPlots(year_double, List((qe.map(_*100), "qe [%]"), (qf, "qf"), (ve, "ve"), (vf, "vf"),(v, "v")))

    import CalibrationData._
    res.map(r => plotXY(List((year_double, r._1, "")), yLabel = r._2, title = r._2))
   
    plotXY(List((year_double, perte2.map(_ * 100), "CIK SE+SF"), (year_double, perte1.map(_ * 100), "CIK SE"), (year_double, (0 until perte2.size).toList.map(i => (perte2(i) * 100 - perte1(i) * 100)), "CIK SF")), yLabel = "Pertes [%]", legend = true, title = "pertes")

    //val year_growth = (1 until year_double.size).map(year_double(_)).toList
    //plotXY(List((year_growth, CalibrationData.smooth_double(growth_rates(qf), (true, 10)).map(_ * 100), "")), yLabel = "qf growth rate [%/y]")
    //plotXY(List((year_growth, CalibrationData.smooth_double(growth_rates(lf), (true, 10)).map(_ * 100), "")), yLabel = "lf growth rate [%/y]")
  
     }

}