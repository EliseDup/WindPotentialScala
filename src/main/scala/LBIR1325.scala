

object LBIR1325 {

  def main(args: Array[String]): Unit = {
    val cp = 2.04
    
  }
  
    def exo1 {
      val Mm = 28.96 * 0.001
      val R = 8.314
      val Rstar = R / Mm
      val m = 0.25
      val p1 = 1E5
      val p2 = 10E5
      val T1 = 300
      val v1 = Rstar * T1 / p1

      // Compression isotherme
      val T2isotherme = 300
      val v2isotherme = Rstar * T2isotherme / p2
      val wm_isotherme = Rstar * T2isotherme * math.log(v2isotherme / v1) / 1000

      println("Isotherme " + "\t" + v2isotherme + "\t" + wm_isotherme + "\t" + wm_isotherme * m)

      // Compression isentropique
      val gamma = 1.4
      val v2isentropique = math.pow(p1 * math.pow(v1, gamma) / p2, 1 / gamma)
      val T2isentropique = p2 * v2isentropique / Rstar
      val wm_isentropique = wm_polytropique(p1, v1, p2, v2isentropique, gamma) / 1000
      println("Isentropique" + "\t" + v2isentropique + "\t" + T2isentropique + "\t" + wm_isentropique + "\t" + m * wm_isentropique)

      // Polytropique m = 1.6
      val v2polytropique = math.pow(p1 * math.pow(v1, 1.6) / p2, 1 / 1.6)
      val T2polytropique = p2 * v2polytropique / Rstar
      val wm_poly = wm_polytropique(p1, v1, p2, v2polytropique, 1.6) / 1000
      println("Polytropique" + "\t" + v2polytropique + "\t" + T2polytropique + "\t" + wm_poly + "\t" + m * wm_poly)
      val deltaU = m * 5 / 2 * Rstar * (T2polytropique - T1)
      println("Delta U" + "\t" + deltaU / 1000 + "\t" + (deltaU / 1000 - m * wm_poly))
    }
    def wm_polytropique(p1: Double, v1: Double, p2: Double, v2: Double, m: Double): Double = {
      p1 * math.pow(v1, m) / (m - 1) * (math.pow(v2, 1 - m) - math.pow(v1, 1 - m))
    }
  
}