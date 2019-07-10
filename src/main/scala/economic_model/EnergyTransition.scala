package economic_model
import wind_solar._
import squants.time.Hours
import squants.energy._
import squants.space._
import wind_energy._
import solar_energy._
import utils._
import solar_energy.PVMono
import solar_energy.CSPTowerStorage12h

object EnergyTransition {
  import PlotHelper._

  var t0 = System.currentTimeMillis()
  val result_file = "/Users/Elise/Model_Doctorat/results_consumption_max"

  val results = Helper.getLines(result_file, "\t").map(i =>
    new OptimisationResult(new GeoPoint(Degrees(i(0).toDouble), Degrees(i(1).toDouble)), GigawattHours(i(2).toDouble),
      GigawattHours(i(3).toDouble), GigawattHours(i(4).toDouble), GigawattHours(i(5).toDouble), GigawattHours(i(6).toDouble), i(7).toDouble)).sortBy(_.eroi).reverse

  println("Load Results in " + (System.currentTimeMillis() - t0) / 1000 + " seconds")

  def main(args: Array[String]): Unit = {
    println(TerawattHours(1.4).to(Exajoules))

    println(results.size)
    println("Total Gross E :" + results.map(_.grossE).foldLeft(Joules(0))(_ + _).to(Exajoules))
    println("Total Ed :" + results.map(_.ed).foldLeft(Joules(0))(_ + _).to(Exajoules))
    println("Total Net E :" + results.map(_.netE).foldLeft(Joules(0))(_ + _).to(Exajoules))

    // val r = simulateEconomy(Exajoules(400), 20, 0.1)
    // plotXY(List((r._1, r._9, "Y"), (r._1, r._10, "C"), (r._1, (0 until r._1.size).toList.map(i => r._9(i) - r._10(i)), "I")), xLabel = "RE Share [%]", yLabel = "[G US $ 2010]", legend = true)

    val eroi = List(20)
    val isLegend = eroi.size > 1

    val res = {
      for (e <- eroi) yield simulateEconomy(Exajoules(400), e, 0.1, print = true)
    }

    val list_eroi = (0 until eroi.size).map(i => (res(i)._1, res(i)._2, "EROI_nre = " + eroi(i).toString)).toList
    plotXY(list_eroi, legend = isLegend, xLabel = "RE Share [%]", yLabel = "EROI")
    val list_chi = (0 until eroi.size).map(i => (res(i)._1, res(i)._3, "EROI_nre = " + eroi(i).toString)).toList
    plotXY(list_chi, legend = isLegend, xLabel = "RE Share [%]", yLabel = "Capital Intensity")
    val list_grossE = (0 until eroi.size).map(i => (res(i)._1, res(i)._4.map(_.to(Exajoules)), "EROI_nre = " + eroi(i).toString)).toList
    plotXY(list_grossE, legend = isLegend, xLabel = "RE Share [%]", yLabel = "Gross Energy [EJ/year]")
    val list_ee = (0 until eroi.size).map(i => (res(i)._1, res(i)._6.map(_.to(Exajoules)), "EROI_nre = " + eroi(i).toString)).toList
    val list_oe = (0 until eroi.size).map(i => (res(i)._1, res(i)._7.map(_.to(Exajoules)), "EROI_nre = " + eroi(i).toString)).toList
    val list_inputs = (0 until eroi.size).map(i => (res(i)._1, (0 until res(i)._9.size).toList.map(j => (res(i)._7(j) + res(i)._6(j)).to(Exajoules)), "EROI_nre = " + eroi(i).toString)).toList
    val list_nete = (0 until eroi.size).map(i => (res(i)._1, res(i)._8.map(_.to(Exajoules)), "EROI_nre = " + eroi(i).toString)).toList
    val list_ed = (0 until eroi.size).map(i => (res(i)._1, res(i)._5.map(_.to(Exajoules)), "EROI_nre = " + eroi(i).toString)).toList
    val list_y = (0 until eroi.size).map(i => (res(i)._1, res(i)._9, "EROI_nre = " + eroi(i).toString)).toList
    val list_c = (0 until eroi.size).map(i => (res(i)._1, res(i)._10, "EROI_nre = " + eroi(i).toString)).toList

    val list_s = (0 until eroi.size).map(i => (res(i)._1, (0 until res(i)._9.size).toList.map(j => 100 * (1 - res(i)._10(j) / res(i)._9(j))), "EROI_nre = " + eroi(i).toString)).toList

    plotXY(list_ee, legend = isLegend, xLabel = "RE Share [%]", yLabel = "Embodied Energy in Energy Sector [EJ/year]")
    plotXY(list_oe, legend = isLegend, xLabel = "RE Share [%]", yLabel = "Operational Energy in Energy Sector [EJ/year]")
    plotXY(list_inputs, legend = isLegend, xLabel = "RE Share [%]", yLabel = "Energy Inputs [EJ/year]")
    plotXY(list_nete, legend = isLegend, xLabel = "RE Share [%]", yLabel = "Net Energy [EJ/year]")
    plotXY(list_ed, legend = isLegend, xLabel = "RE Share [%]", yLabel = "Energy Delivered [EJ/year]")
    plotXY(list_y, legend = isLegend, xLabel = "RE Share [%]", yLabel = "Output [G US $ 2010")
    plotXY(list_c, legend = isLegend, xLabel = "RE Share [%]", yLabel = "Consumption [G US $ 2010]")
    plotXY(list_s, legend = isLegend, xLabel = "RE Share [%]", yLabel = "Savings Rate [%]")

    printRes(list_grossE, "Gross E"); printRes(list_ed, "Ed"); printRes(list_nete, "Net E");
    printRes(list_ee, "EE"); printRes(list_oe, "OE"); printRes(list_eroi, "EROI"); printRes(list_chi, "chi")
    def printRes(res: List[(List[Double], List[Double], String)], name: String) {
      res.map(i => println(name + " from " + i._2(0) + " " + i._2(i._2.size - 1)))
    }
  }

  def simulateFixedNetEnergy(initialNetE: Energy, eroi_nre: Double, chi_nre: Double, q: Double = 1.4, v: Double = 4, deltay: Double = 1.0 / 15, print: Boolean = false) = {
    val i = grosse_oe_ee_fromNetE(initialNetE, eroi_nre, chi_nre)
    var nete_nre = List(initialNetE); var nete_re = List(Joules(0))
    var ed_nre = List(i._1 - i._2)
    var grossE_nre = List(i._1); var ee_nre = List(i._2); var oe_nre = List(i._3)
    var ed_re = List(Joules(0))
    var grossE_re = List(Joules(0)); var ee_re = List(Joules(0)); var oe_re = List(Joules(0))
    var eroi = List(eroi_nre); var chi = List(chi_nre)

    val ed = ed_nre(0).to(TerawattHours); val ee = ee_nre(0).to(TerawattHours)
    var y = List(ed / q)
    var c = List((1 - deltay * v) / q * ed - ee / q)
    var j = 0
    while (nete_nre(nete_nre.size - 1).value > 0) {
      val i = results(j)
      nete_nre = nete_nre :+ nete_nre(j) - i.netE; nete_re = nete_re :+ nete_re(j) + i.netE;

      val res_nre = grosse_oe_ee_fromNetE(nete_nre(j) - i.netE, eroi_nre, chi_nre)
      grossE_nre = grossE_nre :+ res_nre._1; ee_nre = ee_nre :+ res_nre._2; oe_nre = oe_nre :+ res_nre._3; ed_nre = ed_nre :+ res_nre._1 - res_nre._2

      grossE_re = grossE_re :+ (grossE_re(j) + i.grossE); oe_re = oe_re :+ (oe_re(j) + i.oe); ee_re = ee_re :+ (ee_re(j) + i.ee); ed_re = ed_re :+ (ed_re(j) + i.ed)

      j += 1
      val ed = (ed_nre(j) + ed_re(j)).to(TerawattHours); val ee = (ee_nre(j) + ee_re(j)).to(TerawattHours)
      val output = ed / q
      val cons = (1 - deltay * v) / q * ed - ee / q
      y = y :+ output; c = c :+ cons

      val e = (grossE_re(j) + grossE_nre(j)) / (oe_re(j) + oe_nre(j) + ee_re(j) + ee_nre(j))
      eroi = eroi :+ e
      val ch = (ee_re(j) + ee_nre(j)) / (oe_re(j) + oe_nre(j) + ee_re(j) + ee_nre(j))
      chi = chi :+ ch

      if (print && j % 100 == 0) {
        println(eroi(j) + "\t" + chi(j) + "\t" + grossE_nre(j).to(Exajoules) + "\t" + grossE_re(j).to(Exajoules) + "\t" +
          ed_nre(j).to(Exajoules) + "\t" + ed_re(j).to(Exajoules) + "\t" +
          oe_nre(j).to(Exajoules) + "\t" + oe_re(j).to(Exajoules) + "\t" +
          ee_nre(j).to(Exajoules) + "\t" + ee_re(j).to(Exajoules) + "\t" +
          y(j) + "\t" + c(j))
      }
    }
    val list = (0 until j).toList.map(_.toDouble)
    val reShare = (0 until j).map(i => 100 * ed_re(i) / (ed_nre(i) + ed_re(i))).toList

    val tot_grossE = (0 until j).map(i => (grossE_nre(i) + grossE_re(i))).toList
    val tot_ed = (0 until j).map(i => (ed_nre(i) + ed_re(i))).toList
    val tot_netE = (0 until j).map(i => (nete_nre(i) + nete_re(i))).toList

    val tot_ee = (0 until j).map(i => (ee_nre(i) + ee_re(i))).toList
    val tot_oe = (0 until j).map(i => (oe_nre(i) + oe_re(i))).toList

    // plotXY(List((reShare, eroi, "")), xLabel = "RE Share [%]", yLabel = "EROI")
    // plotXY(List((reShare, chi, "")), xLabel = "RE Share [%]", yLabel = "Capital Intensity")
    (reShare, eroi, chi, tot_grossE, tot_ed, tot_ee, tot_oe, tot_netE, y, c)
  }

  def simulateFixedED(ed: Energy, eroi_nre: Double, chi_nre: Double, print: Boolean = false) = {
    // Simulate in ordre to always have a fixed amount of energy delivered to the economy = 500 EJ / year
    // Begin with 100% of fossil fuels, with an EROI of 15 and a capital intensity of 10%
    var ed_nre = List(ed)
    val i = grosse_oe_ee_fromED(ed, eroi_nre, chi_nre)
    var netE_nre = List(i._1 - i._2 - i._3); var netE_re = List(Joules(0))
    var grossE_nre = List(i._1); var ee_nre = List(i._2); var oe_nre = List(i._3)
    var ed_re = List(Joules(0))
    var grossE_re = List(Joules(0)); var ee_re = List(Joules(0)); var oe_re = List(Joules(0))
    var eroi = List(eroi_nre); var chi = List(chi_nre)
    var j = 0
    while (ed_nre(ed_nre.size - 1).value > 0) {
      val i = results(j)
      // Ed is produce with RE and not anymore with NRE
      ed_re = ed_re :+ ed_re(j) + i.ed
      ed_nre = ed_nre :+ ed_nre(j) - i.ed

      val res_nre = grosse_oe_ee_fromED(ed_nre(j) - i.ed, eroi_nre, chi_nre)
      grossE_nre = grossE_nre :+ res_nre._1; ee_nre = ee_nre :+ res_nre._2; oe_nre = oe_nre :+ res_nre._3;
      netE_nre = netE_nre :+ res_nre._1 - res_nre._2 - res_nre._3
      grossE_re = grossE_re :+ (grossE_re(j) + i.grossE)
      oe_re = oe_re :+ (oe_re(j) + i.oe)
      ee_re = ee_re :+ (ee_re(j) + i.ee)
      netE_re = netE_re :+ (netE_re(j) + i.netE)
      j += 1
      val e = (grossE_re(j) + grossE_nre(j)) / (oe_re(j) + oe_nre(j) + ee_re(j) + ee_nre(j))
      eroi = eroi :+ e
      val ch = (ee_re(j) + ee_nre(j)) / (oe_re(j) + oe_nre(j) + ee_re(j) + ee_nre(j))
      chi = chi :+ ch

      if (print && j % 100 == 0) {
        println(eroi(j) + "\t" + chi(j) + "\t" + grossE_nre(j).to(Exajoules) + "\t" + grossE_re(j).to(Exajoules) + "\t" +
          ed_nre(j).to(Exajoules) + "\t" + ed_re(j).to(Exajoules) + "\t" +
          oe_nre(j).to(Exajoules) + "\t" + oe_re(j).to(Exajoules) + "\t" +
          ee_nre(j).to(Exajoules) + "\t" + ee_re(j).to(Exajoules))
      }
    }
    val list = (0 until j).toList.map(_.toDouble)
    val reShare = (0 until j).map(i => 100 * ed_re(i) / (ed_nre(i) + ed_re(i))).toList
    val tot_grossE = (0 until j).map(i => (grossE_nre(i) + grossE_re(i))).toList
    val tot_ed = (0 until j).map(i => (ed_nre(i) + ed_re(i))).toList
    val tot_ee = (0 until j).map(i => (ee_nre(i) + ee_re(i))).toList
    val tot_oe = (0 until j).map(i => (oe_nre(i) + oe_re(i))).toList
    val tot_netE = (0 until j).map(i => (netE_nre(i) + netE_re(i))).toList

    // plotXY(List((reShare, eroi, "")), xLabel = "RE Share [%]", yLabel = "EROI")
    // plotXY(List((reShare, chi, "")), xLabel = "RE Share [%]", yLabel = "Capital Intensity")
    (reShare, eroi, chi, tot_grossE, tot_ed, tot_ee, tot_oe, tot_netE)
  }

  // EROI = Gross E / (OE + EE)
  // Chi = EE / (OE + EE)
  // Ed = Gross E - OE
  def grosse_oe_ee_fromED(ed: Energy, eroi: Double, chi: Double) = {
    val oe = ed * (1 - chi) / (eroi - 1 + chi)
    val ee = chi / (1 - chi) * oe
    val grossE = ed + oe
    (grossE, ee, oe)
  }
  // Net E = Gross E - OE - EE
  def grosse_oe_ee_fromNetE(nete: Energy, eroi: Double, chi: Double) = {
    val oe = nete * (1 - chi) / (eroi - 1)
    val ee = chi / (1 - chi) * oe
    val grossE = nete + ee + oe
    (grossE, ee, oe)
  }

  def simulateEconomy(initialNetE: Energy, eroi_nre: Double, chi_nre: Double, q: Double = 1.4, v: Double = 4, deltay: Double = 1.0 / 15, print: Boolean = false) = {
    val init = grosse_oe_ee_fromNetE(initialNetE, eroi_nre, chi_nre)
    var nete_nre = List(initialNetE); var nete_re = List(Joules(0))
    var ed_nre = List(init._1 - init._2)
    var grossE_nre = List(init._1); var ee_nre = List(init._2); var oe_nre = List(init._3)
    var ed_re = List(Joules(0))
    var grossE_re = List(Joules(0)); var ee_re = List(Joules(0)); var oe_re = List(Joules(0))
    var eroi = List(eroi_nre); var chi = List(chi_nre)

    val ed = ed_nre(0).to(TerawattHours); val ee = ee_nre(0).to(TerawattHours)
    var y = List(ed / q)
    var c = List((1 - deltay * v) / q * ed - ee / q)
    var j = 0
    for (i <- results) {
      if (nete_nre(nete_nre.size - 1).value > 0) {
        nete_nre = nete_nre :+ nete_nre(j) - i.netE; nete_re = nete_re :+ nete_re(j) + i.netE;

        val res_nre = grosse_oe_ee_fromNetE(nete_nre(j) - i.netE, eroi_nre, chi_nre)
        grossE_nre = grossE_nre :+ res_nre._1; ee_nre = ee_nre :+ res_nre._2; oe_nre = oe_nre :+ res_nre._3; ed_nre = ed_nre :+ res_nre._1 - res_nre._2
        grossE_re = grossE_re :+ (grossE_re(j) + i.grossE); oe_re = oe_re :+ (oe_re(j) + i.oe); ee_re = ee_re :+ (ee_re(j) + i.ee); ed_re = ed_re :+ (ed_re(j) + i.ed)

        j += 1
        val ed = (ed_nre(j) + ed_re(j)).to(TerawattHours); val ee = (ee_nre(j) + ee_re(j)).to(TerawattHours)
        val output = ed / q
        val cons = (1 - deltay * v) / q * ed - ee / q
        y = y :+ output; c = c :+ cons

        val e = (grossE_re(j) + grossE_nre(j)) / (oe_re(j) + oe_nre(j) + ee_re(j) + ee_nre(j))
        eroi = eroi :+ e
        val ch = (ee_re(j) + ee_nre(j)) / (oe_re(j) + oe_nre(j) + ee_re(j) + ee_nre(j))
        chi = chi :+ ch

      } else {

        nete_nre = nete_nre :+ Joules(0); nete_re = nete_re :+ nete_re(j) + i.netE;
        grossE_nre = grossE_nre :+ Joules(0); ee_nre = ee_nre :+ Joules(0); oe_nre = oe_nre :+ Joules(0); ed_nre = ed_nre :+ Joules(0)

        grossE_re = grossE_re :+ (grossE_re(j) + i.grossE); oe_re = oe_re :+ (oe_re(j) + i.oe); ee_re = ee_re :+ (ee_re(j) + i.ee); ed_re = ed_re :+ (ed_re(j) + i.ed)
        j += 1
        val ed = (ed_re(j)).to(TerawattHours); val ee = (ee_re(j)).to(TerawattHours)
        val output = ed / q
        val cons = (1 - deltay * v) / q * ed - ee / q
        y = y :+ output; c = c :+ cons

        val e = (grossE_re(j)) / (oe_re(j) + ee_re(j))
        eroi = eroi :+ e
        val ch = (ee_re(j)) / (oe_re(j) + ee_re(j))
        chi = chi :+ ch
      }

      if (print && j % 100 == 0) {
        println(eroi(j) + "\t" + chi(j) + "\t" + grossE_nre(j).to(Exajoules) + "\t" + grossE_re(j).to(Exajoules) + "\t" +
          ed_nre(j).to(Exajoules) + "\t" + ed_re(j).to(Exajoules) + "\t" +
          oe_nre(j).to(Exajoules) + "\t" + oe_re(j).to(Exajoules) + "\t" +
          ee_nre(j).to(Exajoules) + "\t" + ee_re(j).to(Exajoules) + "\t" +
          y(j) + "\t" + c(j))
      }
    }
    val list = (0 until j).toList.map(_.toDouble)
    val reShare = (0 until j).map(i => 100 * ed_re(i) / (ed_nre(i) + ed_re(i))).toList

    val tot_grossE = (0 until j).map(i => (grossE_nre(i) + grossE_re(i))).toList
    val tot_ed = (0 until j).map(i => (ed_nre(i) + ed_re(i))).toList
    val tot_netE = (0 until j).map(i => (nete_nre(i) + nete_re(i))).toList

    val tot_ee = (0 until j).map(i => (ee_nre(i) + ee_re(i))).toList
    val tot_oe = (0 until j).map(i => (oe_nre(i) + oe_re(i))).toList

    // plotXY(List((reShare, eroi, "")), xLabel = "RE Share [%]", yLabel = "EROI")
    // plotXY(List((reShare, chi, "")), xLabel = "RE Share [%]", yLabel = "Capital Intensity")
    (reShare, eroi, chi, tot_grossE, tot_ed, tot_ee, tot_oe, tot_netE, y, c)
  }
}

class OptimisationResult(val center: GeoPoint, val grossE: Energy, val ed: Energy, val netE: Energy, val oe: Energy, val ee: Energy, val eroi: Double)