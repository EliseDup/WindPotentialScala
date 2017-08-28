package solar_energy

import grid.EnergyGenerationPotential
import squants.energy._
import grid.GridCell
import squants.time.Hours
import squants.radio.WattsPerSquareMeter

object SolarPotential {
  def apply() = new SolarPotential()
  def eff_17 = new SolarPotential(0.17, Gigajoules(18000))
  def eff_24 = new SolarPotential(0.24, Gigajoules(15000))
}

class SolarPotential(val efficiency: Double = 0.17, val inputs1MW: Energy = Gigajoules(18000)) extends EnergyGenerationPotential {
  // 25 years
  val lifeTimeYears = 25.0

  def landUseFactor(cell: GridCell) = cell.landCovers.suitabilityFactorSolar
  def powerDensity(cell: GridCell) = cell.irradiance.mean

  val performanceRatio = 0.81

  def power(cell: GridCell, eroi_min: Double, suitable: Boolean) = {
    if (power(cell, suitable) * lifeTime / energyInputs(cell, suitable) >= eroi_min) power(cell, suitable)
    else Watts(0)
  }
  def power(cell: GridCell, suitable: Boolean) = {
    powerDensity(cell) * cell.suitableArea(suitable, this) * performanceRatio * efficiency
  }

  val size1MW = Megawatts(1) / (WattsPerSquareMeter(1000) * efficiency)

  def energyInputs(cell: GridCell, suitable: Boolean) = cell.suitableArea(suitable, this) / size1MW * EmbodiedEnergyPV.inputs(Megawatts(1), powerDensity(cell) * cell.suitableArea(suitable, this) * performanceRatio * efficiency * lifeTime) //  Megajoules(2750)
  // For 1MW as an example
  def capacityFactor(cell: GridCell) = (size1MW * powerDensity(cell) * efficiency * performanceRatio) / Megawatts(1)

  def netEnergyPerYear(cell: GridCell, eroi_min: Double, suitable: Boolean): Energy = energyPerYear(cell, eroi_min, suitable) - energyInputs(cell, suitable) / lifeTimeYears

  def energyPerMonth(cell: GridCell, month: Int, suitable: Boolean): Energy = cell.irradiance.month(month) * cell.suitableArea(suitable) * performanceRatio * efficiency * Hours(24 * 30)

  def eroi(cell: GridCell, eroi_min: Double, suitable: Boolean): Double = {
    if (cell.irradiance.mean.value == 0 || cell.suitableArea(suitable, SolarPotential()).value == 0) 0.0
    else {
      val out = lifeTimeYears * energyPerYear(cell, eroi_min, suitable)
      out / energyInputs(cell, suitable)
    }
  }
}

object EmbodiedEnergyPV {
  val materials = Gigajoules(63538 + 238 + 410620 + 805011 + 1025393 + 616199 + 1058732 + 378456 + 32465 + 69631 + 6015 + 932395) +
    Gigajoules(6614 + 1549 + 1601009 + 338861 + 639637 + 1773269 + 110206 + 1475603 + 3379 + 271490 + 39195 + 595291) +
    Gigajoules(23079 + 3324 + 808 + 6758 + 10296 + 150391 + 9566 + 7540 + 968 + 4290)

  val manufacturing = Gigajoules(1460441 + 515122 + 1115083 + 958274 + 345558)

  val transport =
    Gigajoules(11336 + 9674 + 1434 + 2013 + 675 + 236 + 1086 + 323 + 2319 + 5005 + 141698 + 32248 + 833 + 13418 + 40521 + 4056 + 32584 + 768 + 7728 + 15560 + 1558 + 12512 + 278868 + 36651)
  val installation = Gigajoules(71652 + 185565 + 10603)
  val decommisionning = Gigajoules(71652)
  val maintenance = Gigajoules(0)
  def operation(output: Energy) = output.toGigajoules * Gigajoules(0) //(0.0097)

  def inputs(ratedPower: Power, output: Energy) = (ratedPower / Gigawatts(1)) * (materials + manufacturing + transport + installation + decommisionning + maintenance + operation(output))
}
