package utils

import squants.energy.Energy
import squants.energy.EnergyUnit
import squants.energy.Watts
import squants.MetricSystem

object TerawattHours extends EnergyUnit {
  val conversionFactor = Watts.conversionFactor * MetricSystem.Tera
  val symbol = "TWh"
}