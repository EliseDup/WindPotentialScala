package economic_model

import utils.MegaTonOilEquivalent
import squants.energy._

object Calibration2017 {

  def apply() = new Calibration2017()
  def apply(T: Int, alpha: Double, m: Double) = new Calibration2017(T, alpha, m)
  def units(energy: EnergyUnit, pib: Double, pop: Double) = new Calibration2017(energyUnits = energy, pibUnits = pib, popUnits = pop)

  def T(T: Int): Calibration2017 = new Calibration2017(T = T)
  def alpha(alpha: Double): Calibration2017 = new Calibration2017(alpha = alpha)
  def m(m: Double): Calibration2017 = new Calibration2017(m = m)
}

class Calibration2017(val T: Int = 20, val alpha: Double = 6 / 100.0, val m: Double = 6.5 / 100.0, val gpt: Double = 0.1 / 100, val theta: Double = 0.4,
    val energyUnits: EnergyUnit = KilowattHours, val pibUnits: Double = 1.0, val popUnits: Double = 1.0) {

  def convertEnergy(energy: Energy) = energy.to(energyUnits)
  def convertPIB(pib: Double) = pib /pibUnits
  def convertPop(pop: Double) = pop /popUnits

  // Données observées
  // IEA
  val E = convertEnergy(MegaTonOilEquivalent(9717.295))
  val Ce = convertEnergy(MegaTonOilEquivalent(2064.274) + 0.5 * MegaTonOilEquivalent(2808.148))
  val Ef = E - Ce
  val Ee = convertEnergy(MegaTonOilEquivalent(816.818))
  val Ye = E + Ee
  // Banque Mondiale
  val PIB_prev = convertPIB(7.77968E+13) // PIB 2016
  val PIB = convertPIB(8.02501E+13)
  val g = (PIB - PIB_prev) / PIB_prev
  val s = 25.0 / 100
  // L=3422; % Pop active (10^6 personnes) % donnee non retenue
  val L = convertPop(2871.0 * 1E6); //* 1E6; // Pop employee (10^6 personnes) % donnee retenue

  // Données calculées
  val qe = Ee / Ye; val gamma = Ef / E;
  val gk = g - gpt
  val delta = 1.0/T 
  // Données calibrées
  val v = s / (gk + delta)
  // v = K/PIB
  val K = v * PIB; val Ke = K * m / (1 + m); val Kf = K / (1 + m);
  val ve = Ke / Ye
  // pE = alpha PIB
  val p = alpha * PIB / E
  val Yf = PIB - p * Ce
  val qf = Ef / Yf; val vf = Kf / Yf;

  val eroi = 1 / (qe + delta * ve * qf)

  // Autres données calculées
  val r = theta * (gk + delta) / s; // Loyer du capital
  val w = (1 - theta) * PIB / L; // Salaire
  val lf = (1 - p * qf - r * vf) / w; // Intensité en travail de l'économie
  val le = ((1 - qe) * p - r * ve) / w; // Intensité en travail du secteur énergétique
  val Lf = lf * Yf; // #Travailleurs dans le secteur "autre"
  val Le = le * Ye; // #Travailleurs dans le secteur énergie
  val rho = Yf / PIB;
  // autre variables
  val VAy = (1 - p * qf) * Yf;
  val VAe = p * E;
  val I = s * PIB;
  val Cy = Yf - I;

  val n = p * Ce / Cy; // coefficient de partage du budget de C
  // val sy = s * (1 + n) / (1 + s * n); // I/Y
  val e = E / PIB;
  val c = Ce / Cy;
  val k = K / Ye;

  val (ki, ks) = interval_k(s, n, ve, vf, qe, qf, le, lf)
  val (gki, gks) = interval_gk(s, n, ve, vf, qe, qf, le, lf)
  val (gi, gs) = (gki + gpt, gks + gpt)

  // Conditions d'existence: intervalle admissible pour k = K/U
  def interval_k(s: Double, n: Double, ve: Double, vf: Double, qe: Double, qf: Double, le: Double, lf: Double) = {
    // n'
    val m = (1 - s) * n / (1 + s * n)
    val k1 = m * lf / le + (1 + m) * qf / (1 - qe)
    val k2 = m * vf / ve + (1 + m) * qf / (1 - qe)
    val bounds = List(ve + vf / k2, ve + vf / k1)
    (bounds.min, bounds.max)
  }
  // Conditions d'existence: intervalle admissible pour le taux de croissance du capital gk
  def interval_gk(s: Double, n: Double, ve: Double, vf: Double, qe: Double, qf: Double, le: Double, lf: Double) = {
    val (ki, ks) = interval_k(s, n, ve, vf, qe, qf, le, lf)
    val sy = s * (1 + n) / (1 + s * n); // I/Y
    val gki = sy * (1 - ve / ki) / vf - delta;
    val gks = sy * (1 - ve / ks) / vf - delta;
    (gki, gks)
  }
}