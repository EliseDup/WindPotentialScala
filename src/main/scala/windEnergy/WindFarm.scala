package windEnergy

import squants.energy.Power
import squants.energy.Energy
import squants.space.Length
import construction._
import squants.SquantifiedDouble
import squants.energy.Gigajoules

/**
 * A wind farm consists of windmills, highvoltage transformer stations and transmission grid.
 * Internal cables, transformer station and external cables to a wind farm
 *
 */

object WindFarm {
  
  val availabilityFactor = 1.0
  val arrrayFactor = 0.9

  val turbine = new WindTurbineComponents("2MW")
  def nTurbines(ratedPower: Power) = ratedPower / turbine.ratedPower

  def embodiedEnergy(ratedPower: Power) = {
    turbine.embodiedEnergy * nTurbines(ratedPower) + WindPowerTransmission.embodiedEnergyOnshoreTransmission(ratedPower)
  }
  
}

object OffshoreWindFarm {

  val turbine = OffshoreWindTurbineComponents
  
  def nTurbines(ratedPower: Power) = ratedPower / turbine.ratedPower
  def foundations(waterDepth : Length) = OffshoreFoundations.foundation(waterDepth).embodiedEnergy
  def transmission(ratedPower: Power, distanceToShore: Length) = WindPowerTransmission.embodiedEnergyOffshoreTransmission(ratedPower, distanceToShore)

  def embodiedEnergy(ratedPower: Power, distanceToShore: Length, waterDepth : Length): Energy = {
    nTurbines(ratedPower) * (turbine.embodiedEnergy + foundations(waterDepth) ) + transmission(ratedPower,distanceToShore)
  }

}

object SimpleWindFarm {
  
  val embodiedEnergyPerMW = Gigajoules(15000)
  
  def embodiedEnergy(ratedPower: Power, distanceToShore: Length, waterDepth : Length): Energy = {
    MultiplyingFactor.factor(waterDepth, distanceToShore)*embodiedEnergyPerMW*ratedPower.toMegawatts
  }
  def embodiedEnergy(ratedPower: Power): Energy = embodiedEnergyPerMW*ratedPower.toMegawatts
  
}

object MultiplyingFactor {

  def factor(depth: Length, distance: Length) = {
    val i = distances.zipWithIndex.find(x => x._1._1 <= distance.toKilometers && x._1._2 > distance.toKilometers).get._2
    val j = depths.zipWithIndex.find(x => x._1._1 <= depth.toMeters && x._1._2 > depth.toMeters).get._2
    factors(j)(i)
  }

  val depths = List((0, 20), (20, 30), (30, 40), (40, 100000))
  val distances = List((-100000, 10), (10, 20), (20, 30), (30, 40), (40, 50), (50, 100), (100, 200), (200, 100000))
  val factors =
    Array(Array(1.0, 1.022, 1.043, 1.065, 1.086, 1.183, 1.408, 1.598),
      Array(1.067, 1.090, 1.113, 1.136, 1.159, 1.262, 1.501, 1.705),
      Array(1.237, 1.264, 1.290, 1.317, 1.344, 1.464, 1.741, 1.977),
      Array(1.396, 1.427, 1.457, 1.418, 1.517, 1.653, 1.966, 2.232))
}