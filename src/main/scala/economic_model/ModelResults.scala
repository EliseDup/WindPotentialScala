package economic_model

import utils.MegaTonOilEquivalent
import utils.Exajoules

case class DynamicResults(val years: List[Int], val x: List[Double], val k: List[Double], val gK: List[Double], val s: List[Double], val ye: List[Double], val z: List[Z_xi], val theta: List[Double], val start_year: Option[Int], val end_year: Option[Int],
    val model: ModelResults) { //val mu: List[Double], val p_ratio: List[Double], val ce_ratio: List[Double], val cf_ratio: List[Double]) {

  val last = x.size - 2
  override def toString() = x(last) + "\t" + k(last) + "\t" + gK(last) + "\t" + s(last) + "\t" + theta + "\t" + end_year + "\t" + model.mu(last) // + "\t" + p_ratio(last) + "\t" + ce_ratio(last) + "\t" + cf_ratio(last)
  val is = (0 until years.size).toList
  val Ee = is.map(i => ye(i) * z(i).qe)
  val Ef = is.map(i => model.Yf.toList(i) * z(i).qf)
  val pin = is.map(i => model.pib.toList(i) - (z(i).deltae * model.Ke(i) + z(i).deltaf * model.Kf(i)))
  val alphan = is.map(i => model.VANe(i) / pin(i))
  val P1 = is.map(i => 1 - model.ner(i))
  val P2 = is.map(i => {
    val xi = model.Ce(i) / ye(i)
    (1 - (z(i).xf + z(i).deltaf * z(i).vf) - model.p(i) * z(i).qf) * xi
  })
  val v = is.map(i => model.K.toList(i) / model.pib.toList(i))
  def g_v = (1 until v.toList.size).toList.map(i => (v(i) - v(i - 1)) / v(i - 1))

  val ye_EJ = is.map(i => MegaTonOilEquivalent(ye(i)).to(Exajoules))
  val yer_EJ = is.map(i => ye_EJ(i) * x(i))
  val yenr_EJ = is.map(i => ye_EJ(i) * (1 - x(i)))
  val Ef_EJ = is.map(i => MegaTonOilEquivalent(Ef(i)).to(Exajoules))
  val Ee_EJ = is.map(i => MegaTonOilEquivalent(Ee(i)).to(Exajoules))
  val Ce_EJ = is.map(i => MegaTonOilEquivalent(model.Ce.toList(i)).to(Exajoules))
  val YT = is.map(i => model.Yf.toList(i) + model.p.toList(i)*ye(i))
  
}

class ModelResults(calib: calibration_results_CI, params: Dynamic_Params) {
  val C, Ce, Df, Cf, K, Ke, Kf, Yf, pib, I, mu, p, eta, gamma, alpha, VAe, VAf, VANe, VANf, eroi, ner, Xe, Xf, X = scala.collection.mutable.ArrayBuffer.empty[Double];

  /*C += calib.C; Ce += calib.ce(calib.i); Cf += calib.Cf; Df += (calib.yf - calib.Xe - calib.Xf); Ke += calib.Ke; Kf += calib.Kf; Yf += calib.yf; pib += calib.pib;
  mu += calib.mu; p += calib.p; eta += calib.eta; gamma += calib.gammab; Xe += calib.Xe; Xf += calib.Xf;
  alpha += calib.alpha; VAe += calib.va_e; VAf += calib.va_f; eroi += calib.eroi; ner += calib.ner
  VANe += calib.va_e - calib.z.deltae*calib.Ke; VANf += calib.va_f - calib.z.deltaf*calib.Kf*/

  def update(k: Double, Kt: Double, z: Z_xi, s: Double, ye: Double, gK: Double) {
    mu += z.ve / k; K += Kt
    Ke += mu.last * Kt; Kf += (1 - mu.last) * Kt
    // Ke += ye * z.ve; Kf += K - Ke.last; 
    Yf += Kf.last / z.vf; Xe += ye * z.xe; Xf += Yf.last * z.xf; X += Xe.last + Xf.last;
    Ce += ye * (1 - z.qe) - z.qf * Yf.last;
    Df += Yf.last - Xe.last - Xf.last
    // delta += (z.deltae * Ke.last + z.deltaf * Kf.last) / K
    pib += (gK + z.delta) * Kt / params.s_k(k, z); C += (1 - s) * pib.last; I += s * pib.last

    p += params.p(this)
    Cf += C.last - p.last * Ce.last
    eta += p.last * Ce.last / C.last; gamma += Ce.last / Cf.last

    VAe += (p.last * (1 - z.qe) - z.xe) * ye; VANe += VAe.last - z.deltae * Ke.last
    VAf += (1 - z.xf - p.last * z.qf) * Yf.last; VANf += VAf.last - z.deltaf * Kf.last
    alpha += VAe.last / pib.last
    eroi += eroi_z(z)
    ner += ner_z(z)
  }

  def g_pib = (1 until pib.toList.size).toList.map(i => (pib(i) - pib(i - 1)) / pib(i - 1))
  def pCe = (0 until p.toList.size).toList.map(i => p.toList(i) * Ce.toList(i))

  def eroi_z(z: Z_xi) = 1.0 / (z.qe + (z.deltae * z.ve + z.xe) * z.qf)
  def ner_z(z: Z_xi) = 1.0 - (z.qe + z.qf * (z.xe + z.deltae * z.ve) + (1.0 - z.qe) * (z.xf + z.deltaf * z.vf))
}

class Scenario(val qf: ParamsScenario, val ye: ParamsScenario, val name: String = "") {
  def qf_t(T: Int) = qf.old_x(T)
  def ye_t(T: Int) = ye.old_x(T)
}

// xt = x0*(1+r)^t => r = (xt/x0)^(1/t) - 1
class ParamsScenarioConstantRate(x0: Double, xt: Double, t: Int) extends ParamsScenario(x0, xt, xt, t) {
  val r = math.pow(xt / x0, 1.0 / t) - 1
  override def old_x(T: Int) = x0 * math.pow(1 + r, T)
}

class ParamsScenario(val x0: Double, val xt: Double, val xf: Double, val t: Int) {
  val r1 = if (x0 == xt) 0.0
  else math.pow(xt / x0, 1.0 / t) - 1
  val r2 = if (x0 == xf) 0.0
  else 1 - (xt * (1 + r1) - xf) / (xt - xf)
  def x(T: Int) = {
    if (T < t) x0 * math.pow(1 + r1, T)
    else xf + (xt - xf) * Math.pow(1 - r2, (T - t))
  }
  val r_old = if (x0 == xf) 0.0 else 1 - math.pow((xt - xf) / (x0 - xf), 1.0 / t)
  def old_x(T: Int) = {
    xf + (x0 - xf) * Math.pow(1 - r_old, T)
  }
  override def toString() = "x0 = " + x0 + " , xt = " + xt + " , xf = " + xf + "," + "rate " + r_old
}

class ParamsScenarioLogistic(x0: Double, xt: Double, xf: Double, t: Int) extends ParamsScenario(x0, xt, xf, t) {
  var new_r = r_old
  var new_xt = 0.0

  for (i <- (0 until 1000)) {
    if (math.abs(1 - new_xt / xt) > .00001) {
      new_xt = x0
      (0 until t).map(i => new_xt = new_xt * (1 + new_r * (1 - new_xt / xf)))
      new_r = new_r * xt / new_xt
    }
  }
  override def old_x(T: Int) = {
    var new_x = x0
    (0 until T).map(i => new_x = new_x * (1 + new_r * (1 - new_x / xf)))
    new_x
  }
  override def toString() = "x0 = " + x0 + " , xt = " + xt + " , xf = " + xf + "," + "rate " + new_r

}
case class Interval(val min: Double, val max: Double, val beta: Double = 0.5) {
  val mean = (1 - beta) * min + beta * max
  override def toString() = mean + "," + min + "," + max
  // assert(min < max, "Interval with min > max " + toString())
}