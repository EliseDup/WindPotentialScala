package economic_model

object GrowthModel {
  
  // Données historiques
  val pib2017 = 8.02501E+13; val pib2016 = 7.77968E+13; // US $ 2010
  val tfc2016 = 111128*1E9 // kWh
  val qy = tfc2016/pib2016 // kWh / US $ 2010
  val delta =  delta_(25, 0.1)// Taux de dépréciation du capital : Kt = (1-delta)^t * K0
  val s = 0.25 // Taux d'épargne
  val g0 = (pib2017-pib2016)/pib2017
  
  val gpt0 = 0.01
  val gk0 = g0-gpt0
  // Estimations
  val m0 = 280.0/4000 // GEMBA
  val qe0 = 0.084 //  0.05
  val v0 = s / (gk0+delta)
  
  // Calibration
  val vy = v0/(1+m0)
  val ve0 =  m0*vy*(1-qe0)/qy
  val e0 = 1/(qe0+delta*ve0*qy)
  
  def ve(e: Double, qe: Double): Double = (1-e*qe)/(e*delta*qy)
  def v(e: Double, qe: Double): Double = vy + ve(e,qe)*qy/(1-qe)
  def delta_v(e0: Double, et: Double, qe0: Double, qet: Double) = v(et,qet)-v(e0,qe0)
  def gk(e: Double, qe: Double): Double = s/v(e,qe)-delta
  def g(e0: Double, et: Double, qe0: Double, qet: Double) = (s-delta_v(e0,et,qe0,qet))/(vy + ve(et,qet)*qy/(1-qet)) - delta
  
  // Kt / K0 = (1 - delta)^t => delta = 1 - (Kt/Ko)^1/t
  def delta_(t: Int, ratio_left: Double = 0.1) = 1.0 - math.pow(ratio_left, 1.0/t)
    
  def main(args: Array[String]): Unit = {
    println(g0 + "\t" + gk0 + "\t" + v0)
    println("Calibration :" +"\t" + "vy : " + vy + "\t" + ", ve :"+ ve0 + "\t" + ", eroi : " +e0)
    println(v(e0,qe0))
    printScenario(e0,e0,qe0,qe0)
  }
  
  def printScenario(e0: Double, et: Double, qe0: Double, qet: Double) {

    println("Scenario: e(0)="+e0+", e(t)="+et +", qe(0)=" + qe0 + ", qe(t)="+ qet)
    println("g :" + math.round(g(e0,et,qe0,qet)*100) +"%")
    println("gk :" + math.round(gk(et,qet)*100) +"%")
    println("v :" + v(et,qet) + ", ve :" + ve(et,qet))
  }
}