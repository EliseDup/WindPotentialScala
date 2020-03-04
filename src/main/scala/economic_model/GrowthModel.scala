package economic_model

import utils._
import squants.energy._
import wind_solar._
import wind_energy._
import solar_energy._

object GrowthModel {
  
  import Helper._
  import PlotHelper._

  def main(args: Array[String]): Unit = {
    Calibration.printTableCalibration(1990, List(0.25,0.8), List(Calibration.delta_(15),Calibration.delta_(40)), List(0.05,0.08),List(0.1/100))
   Calibration.printTableCalibration_new(1990, List(10,25), List(0.03,0.07), List(0.03,0.1),List(0.1/100))
    
  }
  
  def simulateTransition(qy: Double, s: Double, delta: Double, alpha: Double) {
    val all_sites = Grid().cells
    val techs = List(OnshoreWindTechnology, OffshoreWindTechnology, PVMono) //, CSPTowerStorage12h)
    val sites_sf = all_sites.filter(s => techs.map(_.suitabilityFactor(s)).sum > 0)
   // val delta = GrowthModel.delta
   // val (k, qy, vy, qe, ve, v) = GrowthModel.calibration(smoothing = (true, 5))

  }
  
}