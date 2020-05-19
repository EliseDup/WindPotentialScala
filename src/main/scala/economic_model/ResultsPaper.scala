package economic_model

import wind_energy._
import solar_energy._
import utils._
import squants.energy._
import squants.time.Hours

object ResultsPaper {
  import Helper._
  import PlotHelper._
  import GrowthModel._

  def main(args: Array[String]): Unit = {
    // Sensitivity analysis table
    //Calibration.printTableCalibration_new(2017, List(15, 25), List(25), List(0.04, 0.08), List(0.04, 0.1), List(0.0))
    //plotParametersHistory
    // Exercice 1 to 3
    val share = (0 to 10).map(_ * 0.1).toList
    resultsExercices(share)
  }

  def resultsExercices(share: List[Double] = (0 to 10).map(_ * 0.1).toList, e_units: EnergyUnit = MegaTonOilEquivalent, pib_units: Int = 1E9.toInt) {
    val cal = new calibration_results_work(energy_units = e_units, pib_units = pib_units, pop_units = 1E6.toInt);
    val target = cal.data.ye(cal.i) * (1 - cal.qe - cal.delta_e * cal.qf * cal.ve)
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
    val res = share.map(s => (s, calculate(target, techs, s, cal, cal.qf, cal.pib, cal.vf * cal.pib)))

    res.map(r => printResults(r._2._1, r._1, cal.s, cal.n, cal.gk, cal.c))
    res.map(r => printPINs(r._2))
    def printResults(z: Z, share_re: Double, s_fixed: Double, n_fixed: Double, gk_fixed: Double, c_fixed: Double) {
      val res = new ImpactPER(z)

      val m_gk = res.mean_std(res.interval_m_gk(s_fixed, n_fixed))
      val gk = res.mean_std(res.interval_gk(s_fixed, n_fixed))
      val m_s = res.mean_std(res.interval_m_s(gk_fixed, n_fixed))
      val s = res.mean_std(res.interval_s(gk_fixed, n_fixed))
      val m_c = res.m_c(gk_fixed, c_fixed)
      val n_c = res.mean_std(res.interval_c(c_fixed))
      val s_c = res.mean_std(res.interval_s_c(gk_fixed, c_fixed))

      println(round(share_re * 100, 0) + " & " +
        round(res.eroi) + " & " + round(100 * z.qe, 2) + " & " + round(z.ve) + " & " + round(res.p._1) + " & " + round(res.p._2) + " & " +
        round(100 * m_gk._1) + " & " + round(100 * m_gk._2) + " & " + round(100 * gk._1) + " & " + round(100 * gk._2) + " & " +
        round(100 * m_s._1) + " & " + round(100 * m_s._2) + " & " + round(100 * s._1) + " & " + round(100 * s._2) + " & " +
        round(100 * m_c) + " & " + round(100 * n_c._1) + " & " + round(100 * n_c._2) + " & " + round(100 * s_c._1) + " & " + round(100 * s_c._2))

      // Not used ?
      val m_gk_bounds = res.interval_m_gk(s_fixed, n_fixed)
      val mu_gk = res.mean_std(res.mu_m(m_gk_bounds._1), res.mu_m(m_gk_bounds._2))
      val k_gk = res.mean_std(res.k_m(m_gk_bounds._1), res.k_m(m_gk_bounds._2))
      val delta_gk = res.mean_std((res.delta_m(m_gk_bounds._1), res.delta_m(m_gk_bounds._2)))
      val m_s_bounds = res.interval_m_s(gk_fixed, n_fixed)
      val mu_s = res.mean_std(res.mu_m(m_s_bounds._1), res.mu_m(m_s_bounds._2))
      val k_s = res.mean_std(res.k_m(m_s_bounds._1), res.k_m(m_s_bounds._2))
    }
    // Print different pin level 
    def printPINs(z: (Z, Energy)) {
      val res = new ModelResolution(z._1, z._2, cal.energy_units, cal.L, cal.s, cal.n)
      println(res.eroi + " & " +
          round(res.PIN1, 0) + " & " + round(res.PIN2, 0) + " & " + round(res.PIN3, 0) + " & " + round(res.PIN4, 0) + " & " +
          round(res.perte1 * 100) + " & " + round(res.perte2 * 100) + " & " + round(res.perte3 * 100))
    }

  }

  def plotParametersHistory {
    import Calibration._

    println((growth_rates(qf)).sum / (growth_rates(qf).size))
    println((growth_rates(lf)).sum / (growth_rates(lf).size))
    
    (0 until year_double.size).map(y => println(data.year(y) + "\t" + v(y) + "\t" + qe(y) + "\t" + qf(y) + "\t" + ve(y) + "\t" + vf(y) + "\t" + le(y) + "\t" + lf(y)))
    val res = List((qe, "qe"), (qf, " qf"), (ve, "ve"), (vf, "vf"), (le.map(_ * 1E6), "le"), (lf.map(_ * 1E6), "lf"))
    
    import CalibrationData._
    res.map(r => plotXY(List((year_double, r._1, "")), yLabel = r._2, title = r._2))
    val year_growth = (1 until year_double.size).map(year_double(_)).toList
    plotXY(List((year_growth, CalibrationData.smooth_double(growth_rates(qf), (true, 10)).map(_ * 100), "")), yLabel = "qf growth rate [%/y]")
    plotXY(List((year_growth, CalibrationData.smooth_double(growth_rates(lf), (true, 10)).map(_ * 100), "")), yLabel = "lf growth rate [%/y]")

  }

}