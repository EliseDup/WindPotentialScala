

import utils.Helper
import java.io.PrintStream
import utils.PlotHelper
import squants.space.Degrees
import grid.WorldGrid
import wind_energy.CapacityFactorCalculation
import wind_energy.WindPotential
import squants.space.SquareKilometers
import squants.radio.WattsPerSquareMeter
import utils.Exajoules
import squants.energy._
import wind_energy.WindPowerTransmission
import grid.GlobCoverClasses
import java.io.FileOutputStream
import squants.radio.Irradiance
import utils._
import utils.TerawattHours
import utils.Terawatts
import squants.time.Hours
import squants.space.Area
import grid.GridCell
import squants.motion.Velocity
import squants.motion.MetersPerSecond
import utils._
import squants.space.Meters
import org.apache.commons.math3.special.Gamma
import wind_energy.WakeEffect
import wind_energy.WindProfile
import wind_energy.WindFarmEnergyInputs
import utils.PetawattHours
import wind_energy.NormalizedPowerCurve
import squants.space.Kilometers
import wind_energy.Weibull
import squants.space.Radians
import solar_energy.CSPParabolic
import solar_energy.CSPParabolicStorage12h
import solar_energy.CSPTowerStorage12h
import wind_solar.Grid

object Test {

  import Helper._
  import PlotHelper._

  val a = 0.5

  def main(args: Array[String]): Unit = {
    val x = (0 to 100).map(_.toDouble / 20).toList
    val t = (0 to 100).map(_.toDouble / 20).toList
//    plotXY(x.map(i => s(0,i)), x)
    // Plot des caractéristiques 
    val caracA =  (0 until 3).toList.map(s => {
      val x_positif = x.filter(i => t_carac_A(i,s) > 0)
      (x_positif, x_positif.map(i => t_carac_A(i,s)),"s/L0="+s.toString)
    })
     val caracB =  (1 until 5).toList.map(s => {
      val x_positif = x.filter(i => t_carac_B(i,s) > 0)
      (x_positif, x_positif.map(i => t_carac_B(i,s)),"c0tau/L0="+s.toString)
    })
    //plotXY(caracA, legend=true,  xLabel = "x/L0", yLabel = "c0t/L0",title="caracA")
    plotXY(caracB, legend=true,  xLabel = "x/L0", yLabel = "c0t/L0",title="caracB")
    //plotXY(caracA++caracB, legend=true,  xLabel = "x/L0", yLabel = "c0t/L0",title="carac")
    
   val list_u_a = (0 until 4).map(t => (x, x.map(i => u_A(i, t.toDouble)), "c0t/L0=" + t.toString)).toList
    plotXY(list_u_a, legend = true, xLabel = "x/L0", yLabel = "u/U")
     val list_u_b = (0 until 4).map(t => (x, x.map(i => u_B(i, t.toDouble)), "c0t/L0=" + t.toString)).toList
    plotXY(list_u_b, legend = true, xLabel = "x/L0", yLabel = "u/U")
  }

  def c(x: Double) = (1 - a * math.exp(-x))
  // s/L0
  def u0(x: Double) = math.cos(2 * math.Pi * x)
  def s(x: Double, t: Double) = math.log(a + math.exp(-t) * (math.exp(x) -a))
  
  
  def u_A(x: Double, t: Double) = {
    val s_ = s(x, t)
    c(s_) / c(x) * u0(s_) * math.exp(x - s_)
  }
def u_B(x: Double, t: Double) = {
  (1-a)*math.exp(x)/(1-a*math.exp(-x))
}
  // Région A : caractéristiques qui émane d'un certain (x,0)
  def t_carac_A(x: Double, s: Double) = math.log((math.exp(x) - a) / (math.exp(s) - a))
  // Région B
  def t_carac_B(x: Double, tau: Double) = math.log((math.exp(x) - a)/(1-a)) + tau

}