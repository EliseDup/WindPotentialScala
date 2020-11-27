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

  def main(args: Array[String]): Unit = {
    //plotCTExample
    //plotScenarios()
    //plotMarginalCostCurve()
    plotThetaPaths()
  }

  def plotCTExample {
    val scn_constant = new Scenario(new ParamsScenario(qf_0, qf_0, qf_0, 1), new ParamsScenario(ye_0, ye_0, ye_0, 1))
    // Enveloppe k_up(x) et k_inf(x)
    val k_env = dyn_1.k_x_ye(qf_0, calib.z, 0.5, ye_0)
    val res = dyn_1.simulate_int(calib, scn_constant, 20, false, Some(0.5))
    val index = (0 until res.x.size / 2).map(_ * 2).toList

    plotXY(List((index.map(res.x.toList(_)), index.map(res.k.toList(_)), "CT"), (k_env.map(_._1), k_env.map(_._2.min), "k_inf"),
      (k_env.map(_._1), k_env.map(_._2.max), "k_sup"),
      (k_env.map(_._1), k_env.map(_._2.mean), "k, theta = 1/2")), xLabel = "x", yLabel = "k", title = "CT_example")
  }

  def plotThetaPaths(thetas: List[(Double, String)] = List((0.0, "th=0"), (0.5, "th=1/2"), (1.0, "th=1"))) {

    val res = thetas.map(i => (i, dyn_1.simulate_int(calib, bau, 600, false, Some(i._1))))
    plotXY(res.map(i => (i._2.x.toList, i._2.k.toList, i._1._2)), xLabel = "x", yLabel = "k", legend = true, title = "k_x_3CT")
    plotXY(res.map(i => (i._2.x.toList, i._2.gK.toList.map(_ * 100), i._1._2)), xLabel = "x", yLabel = "gK [%]", legend = true, title = "gk_x_3CT")
    plotXY(res.map(i => (i._2.years.map(_.toDouble), i._2.x.toList, i._1._2)), yLabel = "x", legend = true, title = "t_x_3CT", int_x_axis = true)
    plotXY(res.map(i => (i._2.years.map(_.toDouble), i._2.gK.toList, i._1._2)), yLabel = "gK [%]", legend = true, title = "t_gk_3CT", int_x_axis = true)
  }
  
  def plotThetaResults(file: String = "../model_data/theta_ex1") {
    val res = getLines(file, "\t").map(i => (0 to 18).toList.map(j => i(j).toDouble))
    val theta = res.map(_(0))
    val x_bau = (theta, res.map(_(1)), "BAU"); val ts_bau = (theta, res.map(_(3) + 2017), "BAU"); val gk_bau = (theta, res.map(_(5)), "BAU")
    plotXY(List(x_bau, (theta, res.map(_(7)), "NP"), (theta, res.map(_(13)), "DD")), xLabel = "theta", yLabel = "x", legend = true, title = "theta_x")
    plotXY(List(ts_bau, (theta, res.map(_(9) + 2017), "NP"), (theta, res.map(_(15) + 2017), "DD")), xLabel = "theta", yLabel = "Ts", legend = true, title = "theta_ts")
    plotXY(List(gk_bau, (theta, res.map(_(11)), "NP"), (theta, res.map(_(17)), "DD")), xLabel = "theta", yLabel = "gK [%]", legend = true, title = "theta_gk")

    val tf_bau = {
      val indexes = res.map(_(2)).zipWithIndex.filter(_._1 > 0).map(_._2)
      ((indexes).map(i => res(i)(0)), (indexes).map(i => res(i)(2) + 2017), "BAU")
    }
    val tf_np = {
      val indexes = res.map(_(8)).zipWithIndex.filter(_._1 > 0).map(_._2)
      ((indexes).map(i => res(i)(0)), (indexes).map(i => res(i)(8) + 2017), "NP")
    }
    val tf_dd = {
      val indexes = res.map(_(14)).zipWithIndex.filter(_._1 > 0).map(_._2)
      ((indexes).map(i => res(i)(0)), (indexes).map(i => res(i)(14) + 2017), "DD")
    }

    plotXY(List(tf_bau, tf_np, tf_dd), xLabel = "theta", yLabel = "Tf", legend = true, title = "theta_tf")
    val y_bau = {
      val indexes = res.map(_(4)).zipWithIndex.filter(_._1 > 0).map(_._2)
      ((indexes).map(i => res(i)(0)), (indexes).map(i => res(i)(4)), "BAU")
    }
    val y_np = {
      val indexes = res.map(_(10)).zipWithIndex.filter(_._1 > 0).map(_._2)
      ((indexes).map(i => res(i)(0)), (indexes).map(i => res(i)(10)), "NP")
    }
    val y_dd = {
      val indexes = res.map(_(16)).zipWithIndex.filter(_._1 > 0).map(_._2)
      ((indexes).map(i => res(i)(0)), (indexes).map(i => res(i)(16)), "DD")
    }
    plotXY(List(y_bau, y_np, y_dd), xLabel = "theta", yLabel = "# years to finish transtion", legend = true, title = "theta_y")

    // Plot for BAU only
    plotXY(List(x_bau), xLabel = "theta", yLabel = "x", title = "theta_x_bau")
    plotXY(List(ts_bau), xLabel = "theta", yLabel = "Ts", title = "theta_ts_bau")
    plotXY(List(gk_bau), xLabel = "theta", yLabel = "gK [%]", title = "theta_gk_bau")
    plotXY(List(y_bau), xLabel = "theta", yLabel = "# years to finish transtion", title = "theta_y_bau")
    plotXY(List(tf_bau), xLabel = "theta", yLabel = "Tf", title = "theta_tf_bau")

  }

  def plotScenarios(tf: Int = 2150) {
    val t = (0 until (2150 - 2017)).toList

    plotXY(List((t.map(_.toDouble + 2017), t.map(i => MegaTonOilEquivalent(bau.ye_t(i)).to(Exajoules)), "BAU"), (t.map(_.toDouble + 2017), t.map(i => MegaTonOilEquivalent(np.ye_t(i)).to(Exajoules)), "NP"),
      (t.map(_.toDouble + 2017), t.map(i => MegaTonOilEquivalent(sd.ye_t(i)).to(Exajoules)), "SD")), yLabel = "Ye [EJ/year]", title = "ye_scenarios", legend = true, int_x_axis = true)

    plotXY(List((t.map(_.toDouble + 2017), t.map(bau.qf_t(_) / bau.qf.x0), "BAU"),
      (t.map(_.toDouble + 2017), t.map(np.qf_t(_) / bau.qf.x0), "NP"),
      (t.map(_.toDouble + 2017), t.map(sd.qf_t(_) / bau.qf.x0), "SD")), yLabel = "qf/qf0", title = "qf_scenarios", legend = true, int_x_axis = true)

  }

  def plotMarginalCostCurve(file: String = "potential_reduced") {

    val lines = getLines(file, "\t")

    val ye = lines.map(i => MegaTonOilEquivalent(i(0).toDouble).to(Exajoules))
    val xe = lines.map(i => MegaTonOilEquivalent(i(1).toDouble).to(Exajoules))
    val ke = lines.map(i => MegaTonOilEquivalent(i(2).toDouble).to(Exajoules))

    val is = (1 until ye.size).toList
    val ye_i = is.map(i => ye(i))

    val cout_i = is.map(i => (xe(i) + ke(i) / 25.0) / ye(i))
    val delta_xe = is.map(i => (xe(i) - xe(i - 1)) / (ye(i) - ye(i - 1)))
    val delta_ke = is.map(i => (ke(i) - ke(i - 1)) / (ye(i) - ye(i - 1)))

    val cout_margi = (1 until cout_i.size).toList.map(i => ((xe(i) + ke(i) / 25) - (xe(i - 1) + ke(i - 1) / 25)) / (ye(i) - ye(i - 1)))

    val cout_xe = is.map(i => xe(i) / ye(i))
    val cout_ke = is.map(i => (ke(i) / 25.0) / ye(i))
    val cout_marg_xe = (1 until cout_xe.size).toList.map(i => (xe(i) - xe(i - 1)) / (ye(i) - ye(i - 1)))
    val cout_marg_ke = (1 until cout_xe.size).toList.map(i => (ke(i) - ke(i - 1)) / 25.0 / (ye(i) - ye(i - 1)))
    val ye_marge = (1 until cout_xe.size).toList.map(i => ye(i))

    plotXY(List((ye_i, cout_i, "Total"), (ye_i, cout_ke, "deKe/Ye"), (ye_i, cout_xe, "Xe/Ye")), xLabel = "Final Energy Produced [EJ/year]", yLabel = "EJ invested/EJ produced", legend = true, title = "cost")
    //  plotXY(List((ye_i, cout_xe, "Xe/Ye")), xLabel = "Final Energy Produced [EJ/year]", yLabel = "Xe EJ/EJ produced", title = "xe_pot")
    //  plotXY(List((ye_i, cout_ke, "deKe/Ye")), xLabel = "Final Energy Produced [EJ/year]", yLabel = "deKe EJ/EJ produced", title = "ke_pot")
    // plotXY(List((ye_i, cout_i, "(Xe+deKe)/Ye")), xLabel = "Final Energy Produced [EJ/year]", yLabel = "EJ invested/EJ produced", title = "cost_pot_tot")

    plotXY(List((ye_marge, cout_margi, "Total"), (ye_marge, cout_marg_ke, "ddeKe/dYe"), (ye_marge, cout_marg_xe, "dXe/dYe")), xLabel = "Final Energy Produced [EJ/year]", yLabel = "Marginal Cost [dEJ/dEJ]", legend = true, title = "marg_cost")
    // Detailed
    //   plotXY(List((ye_marge, cout_marg_xe, "dXe/dYe")), xLabel = "Final Energy Produced [EJ/year]", yLabel = "dXe/dYe", title = "dXe_dYe")
    //   plotXY(List((ye_marge, cout_marg_ke, "ddeKe/dYe")), xLabel = "Final Energy Produced [EJ/year]", yLabel = "ddeKe/dYe", title = "ddeKe_dYe")
    //   plotXY(List((ye_marge, cout_margi, "d(Xe+deKe)/Ye")), xLabel = "Final Energy Produced [EJ/year]", yLabel = "Marginal Cost [dEJ/dEJ]", title = "marg_cost_pot_tot")

    // Plot ve, qe et xe

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

}