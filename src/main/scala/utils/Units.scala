package utils

import squants.energy.Energy
import squants.energy.EnergyUnit
import squants.energy.Watts
import squants.MetricSystem
import squants.energy.SpecificEnergyUnit
import squants.mass._

object TerawattHours extends EnergyUnit {
  val conversionFactor = Watts.conversionFactor * MetricSystem.Tera
  val symbol = "TWh"
}
// Gray is J/kg
object GigajoulesPerton extends SpecificEnergyUnit {
  val conversionFactor = MetricSystem.Giga / MetricSystem.Kilo
  val symbol = "GJ/t"
}