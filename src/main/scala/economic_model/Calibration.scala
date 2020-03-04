package economic_model
import utils._
import squants.energy._

object Calibration {

  val data = CalibrationData
  val year_double = data.year.map(_.toDouble)
  val ind = data.ind
  def delta_(t: Int, ratio_left: Double = 0.1) = 1.0 - math.pow(ratio_left, 1.0 / t)
 
  def index_year(year: Int) = data.year.zipWithIndex.find(i => i._1 == year).get._2

  def rho(alpha: Double, gamma: Double) = 1 - alpha + alpha * gamma

  def calibration_results(year: Int, delta: Double, alpha: Double, gpt: Double, new_s: Option[Double]) = {
    val i = index_year(year)
    val s = new_s.getOrElse(data.s(i))
    val gk = data.g(i) - gpt
    val v = s / (gk + delta)
    val p = alpha * data.pib(i) / data.e(i).toKilowattHours // Prix réel de l'énergie
    val qy = data.e(i).toKilowattHours / data.pib(i) * data.gamma(i) / (1 - alpha + alpha * data.gamma(i)) // Intensité énergétique de l'économie
    val vy = v * (1 - alpha) / rho(alpha, data.gamma(i)) // Intensité capitalistique de l'économie
    val ve = alpha * data.gamma(i) / (1 - alpha + alpha * data.gamma(i)) * (1 - data.qe(i)) / qy * v // Intensité capitalistique du secteur énergétique

    (1 / (data.qe(i) + delta * ve * qy), qy, vy, ve, v)
  }
  def calibration_results_work(year: Int, delta: Double, alpha: Double, m: Double, gpt: Double) = {
    val i = index_year(year)
    val gk = data.g(i) - gpt
    val v = data.s(i) / (gk + delta)
    val k = v * data.pib(i); val ke = m / (1 + m) * k; val ky = 1 / (1 + m) * k
    val p = alpha * data.pib(i) / data.e(i).toKilowattHours // Prix réel de l'énergie
    val y = data.pib(i) - p * data.ce(i).toKilowattHours
    val qy = data.ey(i).toKilowattHours / y // Intensité énergétique de l'économie
    val vy = ky / y // Intensité capitalistique de l'économie
    val ve = ke / data.u(i).toKilowattHours // Intensité capitalistique du secteur énergétique

    (1 / (data.qe(i) + delta * ve * qy), qy, vy, ve, v)
  }

  def round(x: Double) = {
    math.round(100 * x) / 100.0
  }
  
  def printTableCalibration(year : Int = 2017, ss: List[Double], deltas: List[Double], alphas: List[Double], gpts: List[Double]) {
    val cals = ss.map(s => deltas.map(delta => alphas.map(alpha => gpts.map(gpt => ((s, delta, alpha, gpt), calibration_results(year, delta, alpha, gpt, Some(s))))).flatten).flatten).flatten
    val ref = calibration_results(2017,delta_(25, 0.1), 0.05, 0.1 / 100, Some(0.5) )
    print("begin{tabular}{c "); cals.map(i => print("c ")); println("}");

    print("$s$ [%]&"); print("textbf{"); print("50 "); print("}"); cals.map(cal => print(" & " + cal._1._1 * 100)); println(" \\" + "\\")
    print("$delta$ [%]&"); print("textbf{"); print(round(delta_(25, 0.1) * 100)); print("}"); cals.map(cal => print(" & " + round(cal._1._2 * 100))); println(" \\" + "\\")
    print("$alpha$ [%]&"); print("textbf{"); print("5"); print("}"); cals.map(cal => print(" & " + cal._1._3 * 100)); println(" \\" + "\\")
    print("$g_{pt}$ [%]&"); print("textbf{"); print("0.1"); print("}"); cals.map(cal => print(" & " + cal._1._4 * 100)); println(" \\" + "\\")

    print("$v$ & "); print("textbf{"); print(round(ref._5)); print("}"); cals.map(cal => print(" & " + round(cal._2._5))); println(" \\" + "\\")
    print("$q_y$ & "); print("textbf{"); print(round(ref._2)); print("}"); cals.map(cal => print(" & " + round(cal._2._2))); println(" \\" + "\\")
    print("$v_y$ & "); print("textbf{"); print(round(ref._3)); print("}"); cals.map(cal => print(" & " + round(cal._2._3))); println(" \\" + "\\")
    print("$v_e$ & "); print("textbf{"); print(round(ref._4)); print("}"); cals.map(cal => print(" & " + round(cal._2._4))); println(" \\" + "\\")
    print("$epsilon$ & "); print("textbf{"); print(round(ref._1)); print("}"); cals.map(cal => print(" & " + round(cal._2._1))); println(" \\" + "\\")

    println
    print("textbf{"); print(round(100 * (delta_(25, 0.1) * ref._2 * ref._4))); print("}");
    cals.map(cal => print(" & " + round(round(100 * (cal._1._2 * cal._2._4 * cal._2._2))))); println(" \\" + "\\")

  }

  def printTableCalibration_new(year : Int = 2017, ts: List[Int], alphas: List[Double], ms: List[Double], gpts: List[Double]) {

    val cals = ts.map(t => alphas.map(alpha => ms.map(m => gpts.map(gpt => ((t, delta_(t), alpha, m, gpt), calibration_results_work(year,delta_(t),alpha,m,gpt)))).flatten).flatten).flatten
    val ref = calibration_results_work(year,delta_(15),0.05,0.05,0.1/100)

    print("begin{tabular}{c "); cals.map(i => print("c ")); println("}");

    print("$T$ [years]&"); print("textbf{"); print("15"); print("}"); cals.map(cal => print(" & " + cal._1._1)); println(" \\" + "\\")

    // print("$delta$ [%]&"); print("textbf{"); print(round(delta_(25, 0.1) * 100)); print("}"); cals.map(cal => print(" & " + round(cal._1._2 * 100))); println(" \\" + "\\")
    print("$alpha$ [%]&"); print("textbf{"); print("5"); print("}"); cals.map(cal => print(" & " + cal._1._3 * 100)); println(" \\" + "\\")
    print("$m$ [%]&"); print("textbf{"); print("5"); print("}"); cals.map(cal => print(" & " + cal._1._4 * 100)); println(" \\" + "\\")
    print("$g_{pt}$ [%]&"); print("textbf{"); print("0.1"); print("}"); cals.map(cal => print(" & " + cal._1._5 * 100)); println(" \\" + "\\")

    print("$v$ & "); print("textbf{"); print(round(ref._5)); print("}"); cals.map(cal => print(" & " + round(cal._2._5))); println(" \\" + "\\")
    print("$q_y$ & "); print("textbf{"); print(round(ref._2)); print("}"); cals.map(cal => print(" & " + round(cal._2._2))); println(" \\" + "\\")
    print("$v_y$ & "); print("textbf{"); print(round(ref._3)); print("}"); cals.map(cal => print(" & " + round(cal._2._3))); println(" \\" + "\\")
    print("$v_e$ & "); print("textbf{"); print(round(ref._4)); print("}"); cals.map(cal => print(" & " + round(cal._2._4))); println(" \\" + "\\")
    print("$epsilon$ & "); print("textbf{"); print(round(ref._1)); print("}"); cals.map(cal => print(" & " + round(cal._2._1))); println(" \\" + "\\")

    println
    print("textbf{"); print(round(100 * (delta_(15, 0.1) * ref._2 * ref._4))); print("}");
    cals.map(cal => print(" & " + round(round(100 * (cal._1._2 * cal._2._4 * cal._2._2))))); println(" \\" + "\\")

  }
}

object CalibrationData {
  import Helper._
  import PlotHelper._

  val data_folder = "/Users/Elise/Model_Doctorat/WindPotentialScala/data_eco/"

  val smoothing = (false, 1)

  val data = getLines(data_folder + "data_calibration", "\t").map(i => (i(0).toInt, i(1).toDouble, MegaTonOilEquivalent(i(2).toDouble), MegaTonOilEquivalent(i(3).toDouble), MegaTonOilEquivalent(i(4).toDouble), i(5).toDouble / 100, i(6).toDouble / 100))
  val n = data.size
  val ind = (0 until n).toList

  val year = data.map(_._1); val pib = data.map(_._2); val a = data.map(_._3); val e = data.map(_._4); val ce = data.map(_._5)

  val s = smooth_double(data.map(_._6), smoothing); val gpt = smooth_double(data.map(_._7), smoothing);

  // Calculs directs
  val u = ind.map(i => a(i) + e(i))
  val qe = smooth_double(ind.map(i => a(i) / u(i)), smoothing)
  val ey = ind.map(i => e(i) - ce(i))
  val gamma = smooth_double(ind.map(i => ey(i) / e(i)), smoothing)
  val g = smooth_double(List((pib(1) - pib(0)) / pib(0)) ++ (0 until pib.size - 1).map(i => (pib(i + 1) - pib(i)) / pib(i)), smoothing) // On met pour les 2 premières années le même taux de croissance pour ne pas avoir de pb de dimensions
  // (n, ind, year, pib, a, e, ce, s, u, qe, ey, gamma, g, gpt)

  // Calcule la moyenne roulante sur n années
  def smooth_double(x: List[Double], n: (Boolean, Int)): List[Double] = {
    if (n._1) {
      List(x(0)) ++ ((1 until x.size).map(i => {
        (math.max(i - n._2 + 1, 0) to i).map(x(_)).sum / (math.min(i + 1, n._2))
      })).toList
    } else x
  }
  def smooth_energy(x: List[Energy], n: (Boolean, Int)): List[Energy] = {
    if (n._1) {
      List(x(0)) ++ ((1 until x.size).map(i => {
        (math.max(i - n._2 + 1, 0) to i).map(x(_)).foldLeft(Joules(0))(_ + _) / (math.min(i + 1, n._2))
      })).toList
    } else x
  }
  def qe_by_sources = {
    val tfc_data = getLines(data_folder + "total_final_consumption", "\t")
    val years = (1 until tfc_data.size).map(i => tfc_data(i)(0).toDouble).toList
    val tfc = (1 until tfc_data(0).size).map(i =>
      (tfc_data(0)(i).toString, (1 until tfc_data.size).map(j => TonOilEquivalent(tfc_data(j)(i).toDouble * 1000)).toList)).toMap
    val own_use_data = getLines(data_folder + "energy_industry_own_use", "\t")
    val own_use = (1 until own_use_data(0).size).map(i =>
      (own_use_data(0)(i).toString, (1 until own_use_data.size).map(j => -TonOilEquivalent(if (own_use_data(j)(i).nonEmpty) own_use_data(j)(i).toDouble * 1000 else 0.0)).toList)).toMap

    val sources = tfc.keys.toList.filter(i => !i.contains("Crude oil"))
    val qe = sources.map(s => (s, (0 until tfc(s).size).map(i => own_use(s)(i) / (own_use(s)(i) + tfc(s)(i))).toList)).toMap
    val qe_total = (0 until tfc(sources(0)).size).map(i => sources.map(s => own_use(s)(i).toJoules).sum / (sources.map(s => own_use(s)(i).toJoules).sum + sources.map(s => tfc(s)(i).toJoules).sum)).toList
    plotXY(qe.map(q => (years, q._2.map(_ * 100), q._1)).toList ++ List((years, qe_total.map(_ * 100), "Total")), yLabel = "qe [%]", legend = true, title = "qe_i")

    val coal_tfc_china = getLines(data_folder + "coal_china", "\t").map(i => TonOilEquivalent(i(2).toDouble * 1000))
    val coal_qe_china = getLines(data_folder + "coal_china", "\t").map(i => -i(1).toDouble / (-i(1).toDouble + i(2).toDouble))

    plotXY(List((years, tfc("Coal").map(_.to(MegaTonOilEquivalent)), "Total"), (years, coal_tfc_china.map(_.to(MegaTonOilEquivalent)), "China")), yLabel = "Coal - TFC [Mtoe]", legend = true)
    plotXY(List((years, qe("Coal").map(_ * 100), "Total"), (years, coal_qe_china.map(_ * 100), "China")), yLabel = "Coal - qe [%]", legend = true)

  }
}