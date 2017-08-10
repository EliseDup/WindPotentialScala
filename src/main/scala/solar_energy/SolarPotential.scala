package solar_energy

import grid.EnergyGenerationPotential
import squants.energy._
import grid.GridCell
import squants.time.Hours

object SolarPotential extends EnergyGenerationPotential {
  // 25 years
  val lifeTimeYears = 25.0
  
  def landUseFactor(cell: GridCell) = cell.landCovers.suitabilityFactorSolar
  def powerDensity(cell: GridCell) = cell.irradiance.mean

  // 15 - 25%
  val technologyEfficiency = 0.25
  val performanceRatio = 0.82

  def power(cell: GridCell, eroi_min: Double, suitable: Boolean) = {
    if (power(cell, suitable) * lifeTime / energyInputs(cell,suitable) >= eroi_min) power(cell, suitable)
    else Watts(0)
  }
  def power(cell : GridCell, suitable : Boolean) = powerDensity(cell) * cell.suitableArea(suitable, this) * performanceRatio * technologyEfficiency

  def energyInputs(cell: GridCell, suitable: Boolean) = cell.suitableArea(suitable, this).toSquareMeters * Megajoules(2750)
  
  def netEnergyPerYear(cell: GridCell, eroi_min: Double, suitable: Boolean): Energy = energyPerYear(cell, eroi_min, suitable) - energyInputs(cell, suitable) / lifeTimeYears

  def energyPerMonth(cell: GridCell, month: Int, suitable: Boolean): Energy = cell.irradiance.month(month) * cell.suitableArea(suitable) * performanceRatio * technologyEfficiency * Hours(24 * 30)

  def eroi(cell: GridCell, eroi_min: Double, suitable: Boolean): Double = {
    if (cell.irradiance.mean.value == 0 || cell.suitableArea(suitable, SolarPotential).value == 0) 0.0
    else {
      val out = lifeTimeYears * energyPerYear(cell, eroi_min, suitable)
      out / energyInputs(cell, suitable)
    }
  }
}
