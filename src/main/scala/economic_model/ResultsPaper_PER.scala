package economic_model

import wind_energy._
import solar_energy._
import utils._
import squants.energy._
import squants.time.Hours
import java.io.FileOutputStream
import squants.time.Megahertz

object ResultsPaper_PER {
  import Helper._
  import PlotHelper._
  import DynamicXi._

  val scenarios = List(bau,sd) // np, sd)
  val exercices = List(dyn_1, dyn_3) // dyn_2b, dyn_3)
  // val calib = new calibration_results_CI(year = 2018, energy_units = MegaTonOilEquivalent, is2018 = true)
  val calib = new calibration_results_CI(year = 2017, energy_units = MegaTonOilEquivalent)

  val res_path = "../model_data/simulations_2017/simulations_" //2100/" // simulations_test/"
 
  def main(args: Array[String]): Unit = {
    println("Verification " + "\t" + calib.eroi + "\t" + calib.year + "\t" + calib.qe + "\t" + calib.ve + "\t" + calib.xe)
    println("Verification dyn " + "\t" +DynamicXi.calib.year)
    val res_bau = getLines(res_path+"2070/gk_min_BAU").map(i => (i(0).toDouble, i(1).toDouble))
    val res_sd = getLines(res_path+"2070/gk_min_SD").map(i => (i(0).toDouble, i(1).toDouble))
    plotXY(List((res_bau.map(_._2), res_bau.map(_._1*100),"BAU"),(res_sd.map(_._2), res_sd.map(_._1*100),"SD")),xLabel="theta", yLabel="gk_min [%]", title="gk_min_2070", legend=true)
 
 /*  val gK = (307 to 350).map(_/10000.0)
   val Tf = 2070
   val t = Tf-calib.year
     gK.map(g => {
 val ex = new Dynamic_gk_eta(g,calib.eta)
    val res =  ex.simulate_int(calib, sd, t, false,Some(list_th(0.9, t)))
        println( g +  "\t" + res.x.last + "\t" + res.s(res.last) + "\t" + res.end_year.getOrElse(0) + "\t" + res.start_year.getOrElse(0))
  })*/
  }
  def compute {
    // First STEP compute results
    val y = List(2050,2100,calib.year+600)
    y.map(i => computeThetaResults(i)) 
    exercices.map(e => scenarios.map(s => qf_date(e, s, i)))
  }
  
  val pop_projections =(List(2020,2025,	2030,	2035,	2040,	2045,	2050,	2055,	2060,	2065,	2070,	2075,	2080,	2085,	2090,	2095,	2100),
      List(7794799.0,8184437.0,8548487.0,8887524.0,9198847.0,9481803.0,9735034.0,9958099.0,10151470.0,10317879.0,10459240.0,	10577288.0,	10673904.0,	10750662.0,	10809892.0,	10851860.0,10875394.0))
  
      def plot {
    //Then plot results in the order of apparition in the paper
    plotCTExample
    plotScenarios(2100)
    plotMarginalCostCurve()
    // plotThetaPaths(250)
    plotEcoVariables(dyn_1, bau, 250)
    tableScenario(dyn_1)
    plotQfResults(dyn_1)
    plotThetaResults(false)
    plotEcoVariables(dyn_3, bau, 98)
    
    // Cf comparison for ex3 - BAU and SD 
    val res_bau = dyn_3.simulate_int(calib, bau, 98, false, theta_var_ref(dyn_3))
    val res_sd = dyn_3.simulate_int(calib, sd, 61, false, theta_var_ref(dyn_3))
    plotXY(List((res_bau.years.map(_.toDouble), res_bau.model.Cf.toList.map(_ / 1E5), "BAU"), (res_sd.years.map(_.toDouble), res_sd.model.Cf.toList.map(_ / 1E5), "SD")), int_x_axis = true, yLabel = "Cf [1E5 GUS $ 2010] ", legend = true, title = "cf_ex3_bau_sd")

  }
  // Reference theta path : begin at k0 and tends towards 1 after 600 years !
  def theta_var_ref(dyn: Dynamic_Params, T: Int = 601) = {
    Some(list_th_var(calib_theta(dyn), 1, T))
  }
  def calib_theta(dyn: Dynamic_Params)={
    (calib.k - dyn.k_bounds(calib.z)._1) / (dyn.k_bounds(calib.z)._2 - dyn.k_bounds(calib.z)._1)
  }
  def list_th(th: Double, n: Int) = (0 until n).toList.map(i => th)
  def list_th_var(th0: Double, th1: Double, n: Int) = {
    (0 until n).toList.map(i => th0 + (th1 - th0) * i / n)
  }
  
      // A line of the file: theta - x - Tf - Ts - #years
  class Results(file: String, name: String, div : Double = 1.0) {
    val lines = getLines(file, "\t").map(i => (0 to 7).toList.map(j => i(j).toDouble))
    val param = lines.map(_(0)/div)
    val res = new SimulationResults(lines, param, name)
    val x = res.x; val ts = res.ts; val tf = res.tf; val gk = res.gk; val y = res.y; val s = res.s; val mu = res.mu; val p = res.p
  }
  // A line of the file: qfl - x - Tf - Ts - gK - mu - s - p/p0
  class SimulationResults(res: List[List[Double]], xs: List[Double], name: String) {
    val x = (xs, res.map(_(1)), name)
    val ts = (xs, res.map(_(3) + calib.year), name)
    val gk = (xs, res.map(_(4) * 100), name)
    val tf = tf_(res, name)
    val y = y_(res, name);
    def tf_(res: List[List[Double]], name: String) = {
      val indexes = res.map(_(2)).zipWithIndex.filter(_._1 > 0).map(_._2)
      ((indexes).map(i => xs(i)), (indexes).map(i => res(i)(2)), name)
    }
    def y_(res: List[List[Double]], name: String) = {
      val indexes = res.map(_(2)).zipWithIndex.filter(_._1 > 0).map(_._2)
      ((indexes).map(i => xs(i)), (indexes).map(i => res(i)(2) - res(i)(3)), name)
    }
    val mu = (xs, res.map(_(5) * 100), name)
    val s = (xs, res.map(_(6) * 100), name)
    val p = (xs, res.map(_(7)), name)
  }

  def computeThetaResults(Tf : Int) {
    val theta = (0 to 100).toList.map(_ / 100.0)
    exercices.map(dyn => scenarios.map(scn => thetaResults(theta, scn, dyn, Tf)))
  }
  
  def computeQfResuts(Tf : Int) {
    exercices.map(dyn => qf_results(50, dyn,Tf))
  }

  def plotCTExample {
    val scn_constant = new Scenario(new ParamsScenario(qf_0, qf_0, qf_0, 1), new ParamsScenario(ye_0, ye_0, ye_0, 1))
    // Enveloppe k_up(x) et k_inf(x)
    val k_env = dyn_1.k_x_ye(qf_0, calib.z, 0.5, ye_0)
    val res = dyn_1.simulate_int(calib, scn_constant, 20, false, Some(list_th(0.5, 20)))
    val index = (0 until res.x.size / 2).map(_ * 2).toList

    plotXY(List((index.map(res.x.toList(_)), index.map(res.k.toList(_)), "CT"), (k_env.map(_._1), k_env.map(_._2.min), "k_inf"),
      (k_env.map(_._1), k_env.map(_._2.max), "k_sup"),
      (k_env.map(_._1), k_env.map(_._2.mean), "k, theta = 1/2")), xLabel = "x", yLabel = "k", title = "CT_example")
  }

  def plotThetaPaths(nyears: Int) {
    plotXY((0 until nyears).toList.map(_.toDouble), list_th_var(0.5, 1.0, nyears))

    val thetas = List((list_th(0.5, nyears), "th=1/2"), (list_th_var(0.5, 1.0, nyears), "th var"), (list_th(1, nyears), "th=1"))

    val res = thetas.map(i => (i, dyn_1.simulate_int(calib, bau, nyears, false, Some(i._1))))
    plotXY(res.map(i => (i._2.x.toList, i._2.k.toList, i._1._2)), xLabel = "x", yLabel = "k", legend = true, title = "k_x_3CT")
    plotXY(res.map(i => (i._2.x.toList, i._2.gK.toList.map(_ * 100), i._1._2)), xLabel = "x", yLabel = "gK [%]", legend = true, title = "gk_x_3CT")
    plotXY(res.map(i => (i._2.years.map(_.toDouble), i._2.x.toList, i._1._2)), yLabel = "x", legend = true, title = "t_x_3CT", int_x_axis = true)
    plotXY(res.map(i => (i._2.years.map(_.toDouble), i._2.gK.toList, i._1._2)), yLabel = "gK [%]", legend = true, title = "t_gk_3CT", int_x_axis = true)
  }

  def plotScenarios(tf: Int = 2150) {
    val t = (0 until (tf - calib.year)).toList
    
    plotXY(scenarios.map(scn => (t.map(_.toDouble + calib.year), t.map(i => MegaTonOilEquivalent(scn.ye_t(i)).to(Exajoules)), scn.name)), yLabel = "Ye [EJ/year]", title = "ye_scenarios", legend = true, int_x_axis = true)
    plotXY(scenarios.map(scn => (t.map(_.toDouble + calib.year), t.map(scn.qf_t(_) / bau.qf.x0), scn.name)), yLabel = "qf/qf0", title = "qf_scenarios", legend = true, int_x_axis = true)
  }

  class TechnologyCost(file: String) {
    val lines = getLines(file, "\t")
    val ye = lines.map(i => MegaTonOilEquivalent(i(0).toDouble).to(Exajoules))
    val xe = lines.map(i => MegaTonOilEquivalent(i(1).toDouble).to(Exajoules))
    val ke = lines.map(i => MegaTonOilEquivalent(i(2).toDouble).to(Exajoules))
    // Repartition / technology [%]
    val onshore_wind =  (ye, lines.map(i => i(4).toDouble*100),"Onshore Wind")
    val offshore_wind = (ye, lines.map(i => i(5).toDouble*100),"Offshore Wind")
    val pv = (ye,lines.map(i => i(6).toDouble*100),"PV")
    val csp =  (ye,lines.map(i => i(7).toDouble*100),"CSP")
    
    // Iterator which starts from 0 or 1
    val i_0 = (0 until ye.size).toList
    val i_1 = (1 until ye.size).toList
    val ye_1 = i_1.map(i => ye(i))

    val cout_tot = i_0.map(i => (xe(i) + ke(i) / 25.0) / ye(i))
    val cout_xe = i_0.map(i => xe(i) / ye(i))
    val cout_ke = i_0.map(i => (ke(i) / 25.0) / ye(i))

    val delta_xe = i_1.map(i => (xe(i) - xe(i - 1)) / (ye(i) - ye(i - 1)))
    val delta_ke = i_1.map(i => (ke(i) - ke(i - 1)) / (ye(i) - ye(i - 1)))

    val cout_marg = i_1.toList.map(i => ((xe(i) + ke(i) / 25) - (xe(i - 1) + ke(i - 1) / 25)) / (ye(i) - ye(i - 1)))
    val cout_marg_xe = i_1.toList.map(i => (xe(i) - xe(i - 1)) / (ye(i) - ye(i - 1)))
    val cout_marg_ke = i_1.toList.map(i => (ke(i) - ke(i - 1)) / 25.0 / (ye(i) - ye(i - 1)))
  }
  
  def plotMarginalCostCurve(file: String = "potential_reduced_avg", plotTechParams: Boolean = false) {
    val tot = new TechnologyCost(file)
    val onshore_wind = new TechnologyCost("potential_reducedonshore_wind")
    val offshore_wind = new TechnologyCost("potential_reducedoffshore_wind")
    val pv = new TechnologyCost("potential_reducedpv")
    val csp = new TechnologyCost("potential_reducedcsp")

    plotXY(List((tot.ye, tot.cout_tot, "Total"), (tot.ye, tot.cout_ke, "deKe/Ye"), (tot.ye, tot.cout_xe, "Xe/Ye")), xLabel = "Final Energy Produced [EJ/year]", yLabel = "EJ invested/EJ produced", legend = true, title = "cost")
    //  plotXY(List((ye_i, cout_xe, "Xe/Ye")), xLabel = "Final Energy Produced [EJ/year]", yLabel = "Xe EJ/EJ produced", title = "xe_pot")
    //  plotXY(List((ye_i, cout_ke, "deKe/Ye")), xLabel = "Final Energy Produced [EJ/year]", yLabel = "deKe EJ/EJ produced", title = "ke_pot")
    // plotXY(List((ye_i, cout_i, "(Xe+deKe)/Ye")), xLabel = "Final Energy Produced [EJ/year]", yLabel = "EJ invested/EJ produced", title = "cost_pot_tot")

    plotXY(List((tot.ye_1, tot.cout_marg, "Total"), (tot.ye_1, tot.cout_marg_ke, "ddeKe/dYe"), (tot.ye_1, tot.cout_marg_xe, "dXe/dYe")), xLabel = "Final Energy Produced [EJ/year]", yLabel = "Marginal Cost [dEJ/dEJ]", legend = true, title = "marg_cost")
    // Detailed
    //   plotXY(List((ye_marge, cout_marg_xe, "dXe/dYe")), xLabel = "Final Energy Produced [EJ/year]", yLabel = "dXe/dYe", title = "dXe_dYe")
    //   plotXY(List((ye_marge, cout_marg_ke, "ddeKe/dYe")), xLabel = "Final Energy Produced [EJ/year]", yLabel = "ddeKe/dYe", title = "ddeKe_dYe")
    //   plotXY(List((ye_marge, cout_margi, "d(Xe+deKe)/Ye")), xLabel = "Final Energy Produced [EJ/year]", yLabel = "Marginal Cost [dEJ/dEJ]", title = "marg_cost_pot_tot")

    // Per technology
    plotXY(List((tot.ye_1, tot.cout_marg, "Total"), (onshore_wind.ye_1, onshore_wind.cout_marg, "Onshore Wind"),
      (offshore_wind.ye_1, offshore_wind.cout_marg, "Offshore Wind"), (pv.ye_1, pv.cout_marg, "PV"), (csp.ye_1, csp.cout_marg, "CSP")), xLabel = "Final Energy Produced [EJ/year]", yLabel = "Marginal Cost [dEJ/dEJ]", legend = true, title = "marg_cost_per_tech")
    // Plot ve, qe et xe
    if (plotTechParams) {
      val (x_x, qe_x, xe_x, ve_x, deltae_x) = dyn_1.x_fun_ye(ye_0, calib.z)
      plotXY(List((x_x, qe_x.map(_ * 100), "qe")), xLabel = "x", yLabel = "qe [%]", title = "qe_x")
      plotXY(List((x_x, xe_x, "xe")), xLabel = "x", yLabel = "xe [kUS$ 2010/toe]", title = "xe_x")
      plotXY(List((x_x, ve_x, "ve")), xLabel = "x", yLabel = "ve [kUS$ 2010/toe]", title = "ve_x")
      plotXY(List((x_x, (0 until x_x.size).toList.map(i => deltae_x(i) * ve_x(i) + xe_x(i)), "we")), xLabel = "x", yLabel = "we [kUS$ 2010/toe]", title = "we_x")

      plotXY(List((x_x, (0 until x_x.size).toList.map(i => (1.0 / (qe_x(i) + calib.qf * (ve_x(i) * deltae_x(i) + xe_x(i))))), "EROI")), xLabel = "x",
        yLabel = "EROI", title = "EROI_x")
      val f = ve_x.size - 1
      println("vef/ve0" + "\t" + ve_x(f) / calib.z.ve)
      println("xe from " + "\t" + calib.xe + " to " + xe_x(f))
      println("EROI from " + "\t" + calib.eroi + " to " + 1.0 / (qe_x(f) + calib.qf * (ve_x(f) * deltae_x(f) + xe_x(f))))
    }
    // Repartition 
    plotXY(List(tot.onshore_wind,tot.offshore_wind,tot.pv,tot.csp),xLabel = "Final Energy Produced [EJ/year]", yLabel = "Share [%]", legend = true, title = "tech_share")
  }
  def plotEcoVariables(dyn: Dynamic_Params, scn: Scenario, ny: Int) {
    val name = "_" + dyn.name + "_" + scn.name
    val res = dyn.simulate_int(calib, scn, ny, false, theta_var_ref(dyn))
    println("END - " + res.end_year.getOrElse(0))
    val y_double = res.years.map(_.toDouble)
    val indexes = (0 until res.years.size).toList
    val pib = (y_double, res.model.pib.toList.map(_ / 1E5), "PIB")

    plotXY(List((y_double, res.x, "x")), yLabel = "x", int_x_axis = true, title = "x" + name)

    plotXY(List((y_double, res.model.mu.toList.map(_ * 100), "mu")), yLabel = "mu [%]", int_x_axis = true, title = "mu" + name)
    plotXY(List((y_double, res.s.toList.map(_ * 100), "s")), yLabel = "s [%]", int_x_axis = true, title = "s" + name)

    plotXY(List((y_double, res.model.p.toList.map(i => i / calib.p), "p/p0")), yLabel = "p/p0", int_x_axis = true, title = "p_p0" + name)
    plotXY(List((y_double, res.model.eroi.toList, "eroi")), yLabel = "eroi", int_x_axis = true, title = "eroi" + name)

    plotXY(List((y_double, indexes.map(i => res.model.X(i) / res.model.Yf(i) * 100), "X/Yf"),
      (y_double, indexes.map(i => res.model.Cf(i) / res.model.Yf(i) * 100), "Cf/Yf"),
      (y_double, indexes.map(i => res.model.I(i) / res.model.Yf(i) * 100), "I/Yf")) //(y_double, indexes.map(i => res.model.Xf(i) / res.model.Yf(i) * 100), "Xf/Yf"),
      , yLabel = "% Yf", int_x_axis = true, legend = true, title = "Yf_rel" + name)

    plotXY(List((y_double, res.model.Yf.toList.map(_ / 1E5), "Yf"), (y_double, res.model.X.toList.map(_ / 1E5), "X"), (y_double, res.model.Cf.toList.map(_ / 1E5), "Cf"), (y_double, res.model.I.toList.map(_ / 1E5), "I")), yLabel = "1E5 GUS $ 2010", int_x_axis = true, legend = true, title = "Yf" + name)

    plotXY(List((y_double, res.ye.map(_ / 1E4), "Ye"), (y_double, res.Ef.map(_ / 1E4), "Ef"), (y_double, res.model.Ce.toList.map(_ / 1E4), "Ce"), (y_double, res.Ee.map(_ / 1E4), "Ee")), yLabel = "1E4 Mtoe", int_x_axis = true, legend = true, title = "Ye" + name)
    plotXY(List((y_double, indexes.map(i => res.Ef(i) / res.ye(i) * 100), "Ef/Ye"), (y_double, indexes.map(i => res.model.Ce.toList(i) / res.ye(i) * 100), "Ce/Ye"), (y_double, indexes.map(i => res.Ee(i) / res.ye(i) * 100), "Ee/Ye")), yLabel = "% Ye", int_x_axis = true, legend = true, title = "Ye_rel" + name)

    plotXY(List((y_double, indexes.map(i => res.pin(i) / res.model.pib(i) * 100), "PIN/PIB"), (y_double, indexes.map(i => res.model.pib(i) / res.model.Yf(i) * 100), "PIB/Yf")), yLabel = "%", int_x_axis = true, legend = true, title = "pin_pib_rel" + name)

    // PIB C K 
    // PIN VANe VANf
    plotXY(List(pib, (y_double, res.model.VAe.toList.map(_ / 1E5), "VAe"), (y_double, res.model.VAf.toList.map(_ / 1E5), "VAf"),
      (y_double, res.pin.toList.map(_ / 1E5), "PIN"), (y_double, res.model.VANe.toList.map(_ / 1E5), "VANe"), (y_double, res.model.VANf.toList.map(_ / 1E5), "VANf")), yLabel = "1E5 GUS $ 2010", legend = true, title = "pib_pin" + name, int_x_axis = true)
    plotXY(List((y_double, res.model.alpha.toList.map(_ * 100), "VAe/PIB [%]"), (y_double, res.alphan.map(_ * 100), "VANe/PIN [%]")), legend = true, title = "alpha" + name, int_x_axis = true)

    // NER
    plotXY(List((y_double, indexes.map(i => (res.P1(i) + res.P2(i)) * 100), "P1+P2 [%]"), (y_double, res.P1.map(_ * 100), "P1 [%]"), (y_double, res.P2.map(_ * 100), "P2 [%]")), int_x_axis = true, legend = true, title = "ner" + name)

    plotXY(List((y_double, res.gK.map(_ * 100), "gK [%]"), (y_double, res.model.g_pib.map(_ * 100), "g [%]")), int_x_axis = true, legend = true, title = "g" + name)
    plotXY(List((y_double, res.v, "v")), yLabel = "v", int_x_axis = true, legend = true, title = "v" + name)

    // PIB C 
    plotXY(List(pib, (y_double, res.model.C.toList.map(_ / 1E5), "C"), (y_double, res.model.pCe.map(_ / 1E5), "pCe"), (y_double, res.model.Cf.toList.map(_ / 1E5), "Cf")), int_x_axis = true, legend = true, yLabel = "1E5 GUS $ 2010", title = "C" + name)
    plotXY(List((y_double, indexes.map(i => res.model.I(i) / res.model.pib(i) * 100), "I/PIB"),
      (y_double, indexes.map(i => res.model.C.toList(i) / res.model.pib(i) * 100), "C/PIB"),
      (y_double, indexes.map(i => res.model.Cf.toList(i) / res.model.pib(i) * 100), "Cf/PIB"),
      (y_double, indexes.map(i => res.model.pCe(i) / res.model.pib(i) * 100), "pCe/PIB")), int_x_axis = true, legend = true, yLabel = "% PIB", title = "c_pib" + name)
   

  //  val c = (0 until pop_projections._1.size).toList.map(i => res.model.C(res.years.indexOf(pop_projections._1(i)))/pop_projections._2(i))
  //  plotXY(List((pop_projections._1.map(_.toDouble),c,"")), yLabel="Per capita consumption [US$2010/capita]", title="c_per_cap")
  }

  def tableScenario(dyn: Dynamic_Params) {
    println("Table scenarios comparison for " + dyn.name)
    val theta = theta_var_ref(dyn)
    val ny = List(2050 - calib.year, 2100 - calib.year, 600)
   
    val res = scenarios.map(s => ny.map(y => dyn.simulate_int(calib, s, y, false, theta))).flatten
    // Print table
    print("$T_s$"); res.map(i => print(" & " + i.start_year.get)); println("\\" + "\\")
    print("$T_f$"); res.map(i => print(" & " + i.end_year.getOrElse(0))); println("\\" + "\\")
    print("$varkappa$"); res.map(i => print(" & " + round(i.x.last * 100, 1))); println("\\" + "\\")
    print("$mu$"); res.map(i => print(" & " + round(i.model.mu.last * 100, 1))); println("\\" + "\\")
    print("$gK$"); res.map(i => print(" & " + round(i.gK.last * 100, 3))); println("\\" + "\\")
    print("$alpha$"); res.map(i => print(" & " + round(i.model.alpha.last * 100, 1))); println("\\" + "\\")
    print("$p$"); res.map(i => print(" & " + round(i.model.p.last / calib.p, 1))); println("\\" + "\\")
  }

  // Choix de la CT (influence de theta)
  def thetaResults(thetas: List[Double], scn: Scenario, dyn: Dynamic_Params, Tf: Int) {
    val out = new java.io.PrintStream(new java.io.FileOutputStream(res_path +Tf.toInt+"/"+ "theta_" + dyn.name + "_" + scn.name))
    val endY = (thetas, thetas.map(t => {
      val list_th = (0 until Tf-calib.year).toList.map(i => t)
      val res = dyn.simulate_int(calib, scn, Tf-calib.year, false, Some(list_th))
      printRes(out, t, res)
    }))
    out.close()
  }
  
  def plotQf(Tf : Int, scn : Scenario, dyn : Dynamic_Params){
     val path = "../model_data/simulations_"+Tf.toString+"/"
     val r0 = new Results(path+"qf_"+dyn.name+"_"+scn.name+"theta0","theta = 0", calib.qf)
     val r1 = new Results(path+"qf_"+dyn.name+"_"+scn.name+"theta1","theta = 1",calib.qf)
     plotXY(List(r0.x,r1.x), xLabel = "qf("+Tf.toString+")/qf0", yLabel="x("+Tf.toString+")", title = "qf_x_"+dyn.name+"_"+scn.name+"theta0_1",legend=true)
     if(dyn.name.contains("1")) plotXY(List(r0.gk,r1.gk),xLabel = "qf("+Tf.toString+")/qf0", yLabel="gK("+Tf.toString+")", title = "qf_gk_"+dyn.name+"_"+scn.name+"theta0_1",legend=true)
     else plotXY(List(r0.s,r1.s),xLabel = "qf("+Tf.toString+")/qf0", yLabel="s("+Tf.toString+")", title = "qf_s_"+dyn.name+"_"+scn.name+"theta0_1",legend=true)
  }
  
  def plotScenarios_theta(Tf : Int){
    val path = res_path +Tf.toString+"/"
    // theta
    if(Tf ==calib.year+600) {
           val ex1 = (scenarios.map(scn => new Results(path + "theta_ex1_"+scn.name, scn.name)),"ex1")
    val ex3 = (scenarios.map(scn => new Results(path + "theta_ex3_"+scn.name, scn.name)),"ex3")
      List(ex1,ex3).map(scn => plotXY(scn._1.map(_.x), xLabel = "theta", yLabel = "xl", legend = (scn._1.size > 1), title = "theta_xl_" + scn._2))
     List(ex1,ex3).map(scn => plotXY(scn._1.map(_.tf), xLabel = "theta", yLabel = "Tf", legend = (scn._1.size > 1), title = "theta_Tf_" + scn._2))
    }
      // Plot X
    else { 
               val ex1 = (scenarios.map(scn => new Results(path + "theta_ex1_"+scn.name, scn.name)),"ex1_"+Tf.toString)
    val ex3 = (scenarios.map(scn => new Results(path + "theta_ex3_"+scn.name, scn.name)),"ex3_"+Tf.toString)
  
   List(ex1,ex3).map(scn => plotXY(scn._1.map(_.x), xLabel = "theta", yLabel = "x("+Tf.toString+")", legend = (scn._1.size > 1), title = "theta_x_" + scn._2))
    //plotXY(ex1._1.map(_.gk), xLabel = "theta", yLabel = "gK("+Tf.toString+") [%]", legend=true, title = "theta_gk_" + ex1._2)
    //plotXY(ex3._1.map(_.s), xLabel = "theta", yLabel = "s("+Tf.toString+") [%]",legend=true, title = "theta_s_" + ex3._2)
    }
    
  }
  def plot_qf_r(Tf : Int){
     val path = res_path +Tf.toString+"/"
     val r_qf = new Results(path + "qf_r", "ex1_sd")
     if(Tf == 600+calib.year) plotXY(List(r_qf.tf), xLabel="r_qf", yLabel ="Tf", title="Tf_r_qf_ex1_DD")
     else plotXY(List(r_qf.x), xLabel="r_qf", yLabel ="x("+Tf.toString+")", title="x_r_qf_ex1_DD_"+Tf.toString)
  }
  def plotScenarios_qf(Tf : Int){
    val path = res_path +Tf.toString+"/"
   // qf
   val ex1_qf = (scenarios.map(scn => new Results(path + "qf_ex1_"+scn.name,scn.name, calib.qf)),"ex1_"+Tf.toString)
   val ex3_qf = (scenarios.map(scn => new Results(path + "qf_ex3_"+scn.name,scn.name, calib.qf)),"ex3_"+Tf.toString)
   // Plot X value in 2050 and 2100 for both scenarios
   List(ex1_qf,ex3_qf).map(scn =>{
   plotXY(scn._1.map(_.x), xLabel = "qf("+Tf.toString+")/qf0", yLabel = "x("+Tf.toString+")", legend = (scn._1.size > 1), title = "qf_x_" + scn._2)
   //plotXY(ex1_qf._1.map(_.mu), xLabel = "qf/qf0", yLabel = "mu [%]", legend = (scn._1.size > 1), title = "qf_mu_" + scn._2)
   })
   // Plot gK or S value in 2050 and 2100 for both scenarios
   plotXY(ex1_qf._1.map(_.gk), xLabel = "qf("+Tf.toString+")/qf0", yLabel = "gK("+Tf.toString+") [%]", legend=true, title = "qf_gk_" + ex1_qf._2)
   plotXY(ex3_qf._1.map(_.s), xLabel = "qf("+Tf.toString+")/qf0", yLabel = "s("+Tf.toString+")[%]", legend=true, title = "qf_s_" + ex3_qf._2)
      
  }
  
  def plotThetaResults(light: Boolean = true) {
    // val model_path = "../model_data/simulations/theta_"
    val bau_ex1 = new Results(res_path + "theta_ex1_bau", if(light) "BAU" else "ex1 - BAU")
    val np_ex1 = new Results(res_path + "theta_ex1_np", if(light) "NP" else"ex1 - NP")
    val sd_ex1 = new Results(res_path + "theta_ex1_sd", if(light) "SD" else"ex1 - SD")
    val bau_ex2 = new Results(res_path + "theta_ex2_bau", if(light) "BAU" else"ex2 - BAU")
    val np_ex2 = new Results(res_path + "theta_ex2_np", if(light) "NP" else "ex2 - NP")
    val sd_ex2 = new Results(res_path + "theta_ex2_sd", if(light) "SD" else "ex2 - SD")
    val bau_ex3 = new Results(res_path + "theta_ex3_bau", if(light) "BAU" else"ex3 - BAU")
    val np_ex3 = new Results(res_path + "theta_ex3_np", if(light) "NP"else"ex3 - NP")
    val sd_ex3 = new Results(res_path + "theta_ex3_sd", if(light) "SD"else"ex3 - SD")
    //Comparaison des différents scénarios
    def plotScenarios(scn: List[Results], name: String) {
      plotXY(scn.map(_.x), xLabel = "theta", yLabel = "x", legend = (scn.size > 1), title = "theta_x_" + name)
      plotXY(scn.map(_.ts), xLabel = "theta", yLabel = "Ts", legend = (scn.size > 1), title = "theta_ts_" + name)
      plotXY(scn.map(_.gk), xLabel = "theta", yLabel = "gK [%]", legend = (scn.size > 1), title = "theta_gk_" + name)
      if (scn.map(_.tf._2).flatten.size > 0) {
        plotXY(scn.map(_.tf), xLabel = "theta", yLabel = "Tf", legend = (scn.size > 1), title = "theta_tf_" + name)
        plotXY(scn.map(_.y), xLabel = "theta", yLabel = "Tf-Ts", legend = (scn.size > 1), title = "theta_y_" + name)
      }
      plotXY(scn.map(_.s), xLabel = "theta", yLabel = "s [%]", legend = (scn.size > 1), title = "theta_s_" + name)
      plotXY(scn.map(_.mu), xLabel = "theta", yLabel = "mu [%]", legend = (scn.size > 1), title = "theta_mu_" + name)
      plotXY(scn.map(_.p), xLabel = "theta", yLabel = "p/p0", legend = (scn.size > 1), title = "theta_p_" + name)
    }
    // Light version only ex1 & ex3, bau & sd
    if (light) {
      plotScenarios(List(bau_ex1, sd_ex1), "ex1")
      plotScenarios(List(bau_ex1), "ex1_bau")
      plotScenarios(List(bau_ex3, sd_ex3), "ex3")
      plotScenarios(List(bau_ex3), "ex3_bau")

    } else {
      plotScenarios(List(bau_ex1, np_ex1, sd_ex1), "ex1")
      plotScenarios(List(bau_ex1), "ex1_bau")
      plotScenarios(List(bau_ex1, bau_ex2), "ex1_ex2_bau")
      plotScenarios(List(bau_ex2, np_ex2, sd_ex2), "ex2")
      plotScenarios(List(bau_ex3, np_ex3, sd_ex3), "ex3")
    }
  }

  // Rôle du PT !
  // On ne joue pas sur la valeur limite mais sur le temps pour l'atteindre
  def qf_date(dyn : Dynamic_Params, scn : Scenario, Tf : Int){
     val out = new java.io.PrintStream(new java.io.FileOutputStream(res_path +Tf.toInt +"/" + "qf_r" + "_"+dyn.name +"_"+ scn.name))
    (2018 to 2040).map(t => {
      val s = new Scenario(new ParamsScenario(qf_0, scn.qf.xt, qf_f, t-calib.year),scn.ye)
     val r = dyn.simulate_int(calib, s, Tf-calib.year, plot = false, Some(list_th(1, Tf-calib.year)))
       printRes(out, s.qf.r_old, r)
    })
    out.close()
  }
  
  def qf_results(step_qf: Int, dyn: Dynamic_Params, Tf : Int) {
    scenarios.map(scn => qf_detailed_results(step_qf, scn, dyn,Tf))
    // qf_detailed_results(step_qf, np, dyn)
  }

  def qf_detailed_results(step_qf: Int, scenario: Scenario, dyn: Dynamic_Params, Tf : Int, qfl_max : Double = 0.5) {
    val qfs = (1 until step_qf).toList.map(i => {
      val ratio_qf = qfl_max + (i*qfl_max/ step_qf.toDouble)
      // Si la nouvelle limite est plus petite que la valeur en 2040, on hausse la valeur en 2040 pour dire qu'on a fait 1/3 du chemin d'ici 2040.
       val ratio_qt = scenario.qf.xt / scenario.qf.x0
      val ratio_qt_corr = 1 - 1.0 / 3 * (1 - ratio_qf) // else ratio_qt
      new Scenario(new ParamsScenario(scenario.qf.x0, scenario.qf.x0 * ratio_qt_corr, scenario.qf.x0 * ratio_qf, scenario.qf.t), scenario.ye)
    })
    detailedResults(qfs, dyn, calib, dyn.name + "_" + scenario.name, Tf)
  }

  def detailedResults(qfs: List[Scenario], dyn: Dynamic_Params, calib: calibration_results_CI, label: String, Tf : Int) {
    // val out0 = new java.io.PrintStream(new java.io.FileOutputStream(res_path +Tf.toInt +"/" + "qf_" + label + "theta0"))
    // val out1 = new java.io.PrintStream(new java.io.FileOutputStream(res_path +Tf.toInt +"/" + "qf_" + label+ "theta1"))
    val out = new java.io.PrintStream(new java.io.FileOutputStream(res_path +Tf.toInt +"/" + "qf_" + label))
    
    qfs.map(qf => {
      //val r0 = dyn.simulate_int(calib, qf, Tf-calib.year, plot = false,  Some(list_th(0.0, Tf-calib.year)))//(dyn,Tf-calib.year))
      //val r1 = dyn.simulate_int(calib, qf, Tf-calib.year, plot = false,  Some(list_th(1.0, Tf-calib.year)))
      // out.print(qf.qf.xf + "\t" + r.x.last + "\t" + r.k.last + "\t" + r.gK.last + "\t" + r.end_year.getOrElse(0.0) + "\t" + r.s.toList.max + "\t" + r.model.mu.last + "\t" + r.model.alpha.last + "\t" + r.z.last.toString() + "\n")
      // printRes(out0, qf.qf.xf, r0); printRes(out1, qf.qf.xf, r1)
      val r = dyn.simulate_int(calib, qf, Tf-calib.year, plot = false, theta_var_ref(dyn))
      printRes(out, qf.qf.xf, r);
    })
    out.close();
    //out0.close(); out1.close()
  }
  def printRes(out: java.io.PrintStream, param: Double, r: DynamicResults) {
    if(r.s.size > 1) out.print(param + "\t" + r.x.last + "\t" + r.end_year.getOrElse(0) + "\t" + r.start_year.getOrElse(0) + "\t" + r.gK.last + "\t" + r.model.mu.toList(r.model.mu.size - 2) + "\t" + r.s.toList(r.s.size - 2) + "\t" + r.model.p.toList(r.model.p.size - 2) / calib.p + "\n")
    else out.print(param + "\t" + r.x.last + "\t" + r.end_year.getOrElse(0) + "\t" + r.start_year.getOrElse(0) + "\t" + r.gK.last + "\t" + r.model.mu.last + "\t" + r.s.last + "\t" + r.model.p.last / calib.p + "\n")
  }


  def plotQfResults(dyn: Dynamic_Params, light: Boolean = false) {
    //val model_path = "../model_data/simulations/qf_"
    val bau = new Results(res_path + "qf_" + dyn.name + "_bau", "BAU",calib.qf)
    val np = new Results(res_path + "qf_" + dyn.name + "_np", "NP",calib.qf)
    val sd = new Results(res_path + "qf_" + dyn.name + "_sd", "SD",calib.qf)
    // Plot xl vs qfl/qf0
    val scn = if (light) List(bau, sd) else List(bau, np, sd)
    plotXY(scn.map(_.x), xLabel = "qfl/qf0", yLabel = "x", legend = true, title = "qfl_x_" + dyn.name)
    plotXY(scn.map(_.gk), xLabel = "qfl/qf0", yLabel = "gK [%]", legend = true, title = "qfl_gk_" + dyn.name)

    // Plot tf vs qfl/qf0
    plotXY(scn.map(_.tf), xLabel = "qfl/qf0", yLabel = "Tf", legend = true, title = "qfl_tf_" + dyn.name)
    plotXY(scn.map(_.ts), xLabel = "qfl/qf0", yLabel = "Ts", legend = true, title = "qfl_ts_" + dyn.name)
    plotXY(scn.map(_.y), xLabel = "qfl/qf0", yLabel = "Tf - Ts", legend = true, title = "qfl_y_" + dyn.name)
    plotXY(scn.map(_.s), xLabel = "qfl/qf0", yLabel = "s [%]", legend = true, title = "qfl_s_" + dyn.name)
    plotXY(scn.map(_.mu), xLabel = "qfl/qf0", yLabel = "mu [%]", legend = true, title = "qfl_mu_" + dyn.name)
    plotXY(scn.map(_.p), xLabel = "qfl/qf0", yLabel = "p/p0", legend = true, title = "qfl_p_" + dyn.name)

  }
  
  def plotHistoryParameters{
    val y = (1990 to 2018).toList
    val cal = y.map(i => new calibration_results_CI(year=i))
    cal.map(c => println(c.ye.to(MegaTonOilEquivalent) + "\t" + c.qf + "\t" + c.ef.to(MegaTonOilEquivalent) + "\t" + c.yf))
    plotXY(List((y.map(_.toDouble),cal.map(_.vf),"vf")),yLabel="vf", title="history_vf", int_x_axis=true)
     plotXY(List((y.map(_.toDouble),cal.map(_.xf),"xf")),yLabel="xf", title="history_xf", int_x_axis=true)
    plotXY(List((y.map(_.toDouble),cal.map(_.qf),"qf")),yLabel="qf", title="history_qf", int_x_axis=true)
    plotXY(List((y.map(_.toDouble),cal.map(_.eroi),"eroi")),yLabel="eroi", title="history_eroi", int_x_axis=true)
   plotXY(List((y.map(_.toDouble),cal.map(_.gK),"gK")),yLabel="gK", title="history_gk", int_x_axis=true)
  }
  
  def computeGkMinimum(scn: Scenario, Tf : Int) {
  val out = new java.io.PrintStream(new java.io.FileOutputStream(res_path +Tf.toInt +"/" + "gk_min_10_" + scn.name))
  val theta = (0 to 10).toList.map(_/10.0)
  var lastGk = 0.037 // calib.gK
  theta.map(th =>{
    var end = false
    ((lastGk*100*100).toInt until 500).map(_/10000.0).map(gk =>{
      if(!end){
        print(gk +"\t")
        val res = new Dynamic_gk_eta(gk, calib.eta).simulate_int(calib, scn, Tf-calib.year, false, Some(list_th(th, Tf-calib.year)))
         if(res.x.last==1.0){
           end=true; lastGk = gk;
           out.print(gk + "\t")
           printRes(out, th, res)
         }
      }
    })
  })
     }
}