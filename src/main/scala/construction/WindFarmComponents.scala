package construction

import squants.energy.Power
import squants.mass.Mass
import utils.Helper
import squants.energy.Megawatts
import squants.energy.Energy
import operation.WindTurbine
import squants.space.Length

/**
 * A wind farm consists of windmills, highvoltage transformer stations and transmission grid.
 * Internal cables, transformer station and external cables to a wind farm
 *
 */

class WindFarm(val turbine : WindTurbine, val ratedPower : Power) {

  val transformerStation = Transmission.transformerStation(ratedPower)
  val internalCables =  Transmission.externalOnshoreCables(ratedPower)
  val externalCables = Transmission.internalOnshoreCables(ratedPower)

  val nTurbines = ratedPower / turbine.ratedPower
  
  def embodiedEnergy = turbine.specs.embodiedEnergy * nTurbines + 
  transformerStation.embodiedEnergy + internalCables.embodiedEnergy + externalCables.embodiedEnergy
}

class OffshoreWindFarm(distanceToShore : Length, turbine : WindTurbine, ratedPower : Power) extends WindFarm(turbine, ratedPower) {
  
  override def embodiedEnergy: Energy = super.embodiedEnergy + Transmission.toOnshoreGridCablesEnergy(distanceToShore)
  
}