package wind_energy

import squants.energy._
import squants.space.Length
import construction._
import squants.SquantifiedDouble
import grid.GridCell

/**
 * A wind farm consists of windmills, highvoltage transformer stations and transmission grid.
 * Internal cables, transformer station and external cables to a wind farm
 *
 */

object WindFarm {

  val availabilityFactor = 1.0
  val arrayFactor = 0.9

  val turbine = new WindTurbineComponents("2MW")
  def nTurbines(ratedPower: Power) = ratedPower / turbine.ratedPower

  def embodiedEnergy(ratedPower: Power) = {
    println("Onshore" + turbine.embodiedEnergy.toGigajoules/2.0 + "\t" + WindPowerTransmission.embodiedEnergyOnshoreTransmission(ratedPower).toGigajoules)
    turbine.embodiedEnergy * nTurbines(ratedPower) + WindPowerTransmission.embodiedEnergyOnshoreTransmission(ratedPower)
  }

  def weight(material: Material, ratedPower: Power) = {
    turbine.weight(material) * nTurbines(ratedPower) + WindPowerTransmission.weightOnshore(material, ratedPower)
  }

}

object OffshoreWindFarm {

  val turbine = OffshoreWindTurbineComponents

  def nTurbines(ratedPower: Power) = ratedPower / turbine.ratedPower
  def foundations(waterDepth: Length) = OffshoreFoundations.foundation(waterDepth)
  def transmission(ratedPower: Power, distanceToShore: Length) = WindPowerTransmission.embodiedEnergyOffshoreTransmission(ratedPower, distanceToShore)

  def embodiedEnergy(ratedPower: Power, distanceToShore: Length, waterDepth: Length): Energy = {
    println("Offshore" + turbine.embodiedEnergy.toGigajoules/5.0 + "\t" + foundations(waterDepth).embodiedEnergy.toGigajoules/5.0 + "\t" + transmission(ratedPower, distanceToShore).toGigajoules)
    nTurbines(ratedPower) * (turbine.embodiedEnergy + foundations(waterDepth).embodiedEnergy) + transmission(ratedPower, distanceToShore)
  }

  def weight(material: Material, ratedPower: Power, distanceToShore: Length, waterDepth: Length) = {
    (turbine.weight(material) + foundations(waterDepth).weight(material)) * nTurbines(ratedPower) +
    WindPowerTransmission.weightOffshore(material, ratedPower, distanceToShore)
  }

}

object SimpleWindFarm {

  val embodiedEnergyPerMW = Gigajoules(15860)

  def embodiedEnergy(ratedPower: Power, distanceToShore: Length, waterDepth: Length): Energy = {
    MultiplyingFactor.factor(waterDepth, distanceToShore) * embodiedEnergyPerMW * ratedPower.toMegawatts * 1.2
  }
  def embodiedEnergy(ratedPower: Power): Energy = embodiedEnergyPerMW * ratedPower.toMegawatts

  def embodiedEnergy(cell : GridCell): Energy = 
    if(cell.onshore) embodiedEnergy(Megawatts(1))
    else embodiedEnergy(Megawatts(1), cell.distanceToCoast, cell.waterDepth)
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