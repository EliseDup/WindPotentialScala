package utils

import squants.energy.Energy
import squants.energy.EnergyUnit
import squants.energy.Watts
import squants.MetricSystem
import squants.energy.SpecificEnergyUnit
import squants.mass._
import squants.energy._

object TerawattHours extends EnergyUnit {
  val conversionFactor = Watts.conversionFactor  * MetricSystem.Tera
  val symbol = "TWh"
}
object TonOilEquivalent extends EnergyUnit {
  val conversionFactor = 11.63 * Watts.conversionFactor  * MetricSystem.Mega
  val symbol = "toe"
}
object MegaTonOilEquivalent extends EnergyUnit {
  val conversionFactor = TonOilEquivalent.conversionFactor * MetricSystem.Mega 
  val symbol = "Mtoe"
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
