package utils

import squants.energy._
import squants.MetricSystem
import squants.mass._
import squants.space._
import squants.thermal._
import squants.motion._
import squants.radio._
import grid.GridCell
import wind_energy.WindProfile
import org.apache.commons.math3.special.Gamma

object TerawattHours extends EnergyUnit {
  val conversionFactor = Watts.conversionFactor  * MetricSystem.Tera
  val symbol = "TWh"
}
object PetawattHours extends EnergyUnit {
  val conversionFactor = Watts.conversionFactor  * MetricSystem.Peta
  val symbol = "PWh"
}
object TonOilEquivalent extends EnergyUnit {
  val conversionFactor = 11.63 * Watts.conversionFactor  * MetricSystem.Mega
  val symbol = "toe"
}
object MegaTonOilEquivalent extends EnergyUnit {
  val conversionFactor = TonOilEquivalent.conversionFactor * MetricSystem.Mega 
  val symbol = "Mtoe"
}
object Exajoules extends EnergyUnit {
  val conversionFactor = Joules.conversionFactor * MetricSystem.Exa
  val symbol = "EJ"
}
object Terawatts extends PowerUnit {
  val conversionFactor = MetricSystem.Tera
  val symbol = "TW"
}
// Gray is J/kg
object GigajoulesPerton extends SpecificEnergyUnit {
  val conversionFactor = MetricSystem.Giga / MetricSystem.Kilo
  val symbol = "GJ/t"
}
object GigaHectares extends AreaUnit {
  val symbol = "Gha"
  val conversionFactor = 10000d*MetricSystem.Giga
}

object Thermodynamics {
  // Pressure = 1bar * (1 - 0.0065 z / T)^5.255
  def pressure(elevation : Length) = Pascals(101325) * Math.pow(1 - 0.0065 * elevation.toMeters / 288.15, 5.255)
  // density = P/RT
  def airDensity(elevation : Length) = KilogramsPerCubicMeter(pressure(elevation).toPascals / (287.05 * 288.15))
  // Power Density = 1/2 pho v^3
 def powerDensity(windSpeed : Velocity, elevation : Length) = WattsPerSquareMeter(0.5 * airDensity(elevation).toKilogramsPerCubicMeter * Math.pow(windSpeed.toMetersPerSecond, 3))
 
  def theoreticalPowerDensity(cell : GridCell, wind : WindProfile)={
    WattsPerSquareMeter(0.5 * airDensity(wind.height + cell.altitude).toKilogramsPerCubicMeter * Math.pow(wind.c.toMetersPerSecond,3) * Gamma.gamma(1 + 3.0/wind.k))
  }
}