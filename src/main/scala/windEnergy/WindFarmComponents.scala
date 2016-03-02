package windEnergy

import squants.energy.Power
import squants.energy.Energy
import squants.space.Length
import construction._
import squants.SquantifiedDouble

/**
 * A wind farm consists of windmills, highvoltage transformer stations and transmission grid.
 * Internal cables, transformer station and external cables to a wind farm
 *
 */

class WindFarm(val ratedPower: Power) {

  val availabilityFactor = 1.0
  val arrrayFactor = 0.9

  val turbine = new WindTurbineComponents("2MW")
  val nTurbines = ratedPower / turbine.ratedPower

  def embodiedEnergy = turbine.embodiedEnergy * nTurbines + WindPowerTransmission.embodiedEnergyOnshoreTransmission(ratedPower)
}

class OffshoreWindFarm(ratedPower: Power,
    val distanceToShore: Length, val waterDepth: Length) extends WindFarm(ratedPower) {

  val turbines = OffshoreWindTurbineComponents.embodiedEnergy
  val foundations = OffshoreFoundations.foundation(waterDepth).embodiedEnergy
  val transmission = WindPowerTransmission.embodiedEnergyOffshoreTransmission(ratedPower, distanceToShore)

  override def embodiedEnergy: Energy = nTurbines * (turbines + foundations) + transmission

}