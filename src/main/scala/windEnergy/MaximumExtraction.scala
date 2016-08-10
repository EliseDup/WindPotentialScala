package windEnergy

import squants.motion._
import squants.mass._
import squants.space._
import squants.radio.WattsPerSquareMeter
import utils.PlotHelper
import utils.GeoPoint
import squants.radio.Irradiance
import scala.io.Source
import utils.GeoPoint
import utils.Helper
import squants.energy.Watts
import utils.Terawatts

object MaximumExtraction {
  def main(args: Array[String]): Unit = {
        
    // Constants
    val rho = KilogramsPerCubicMeter(1.225)
    val z_0 = Millimeters(1)
    val h_hub = Meters(80)
    val h_top = Meters(2000)
    val d = Meters(61)
    val A_rotor = Math.PI * d * d / 4.0
    val density = (1 to 1000).map(_ * 0.01).toList
    val Cf = 0.56
    val C_DN = Math.pow(0.4, 2) * Math.pow(Math.log(h_hub.toMeters / z_0.toMeters), -2)
    val v_hub_0 = MetersPerSecond(10)

    def v_top(v_hub: Velocity) = v_hub * Math.log(h_top / z_0) / Math.log(h_hub / z_0)

    val J_in_0 = WattsPerSquareMeter(2.15)
    val J_transfer_0 = WattsPerSquareMeter(1.68)

    val F_acc_0 = 0.168 // W * s/m^3
    // Assumption 2
    val F_transfer = F_acc_0

    def v_hub(density: Double) = {
      val den = 1.225 * (0.0013 + 0.5 * 0.56 * A_rotor.toSquareKilometers * density)
      Math.sqrt(F_transfer / den)
    }
    // Turbine density = #turbines / km^2
    def F_turbine(density: Double) = 0.5 * rho.toKilogramsPerCubicMeter * Math.pow(v_hub(density), 2) * A_rotor.toSquareKilometers * density
    def P_turbine(density: Double) = F_turbine(density) * v_hub(density)

    // Momentum at hub height : F_tranfer - F_surface - F_turbine = 0 => F_surface = F_acc_0 - F_turbine
    // Momentum at the top : F_acc - F_transfer = 0 => F_acc_0 = F_acc = F_transfer
    def F_surface(density: Double) = rho.toKilogramsPerCubicMeter * Math.pow(v_hub(density), 2) * C_DN

    val v = density.map(v_hub(_))
    val power = density.map(P_turbine(_))
  //  PlotHelper.plotXY(density, power)

    // 10D * 3D
    val densityTest = 1 / (10 * d.toKilometers * 3 * d.toKilometers)
  //  println(v_hub(densityTest))
  //  println(P_turbine(densityTest))

  }
}