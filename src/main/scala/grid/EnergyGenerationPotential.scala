package grid

import squants.radio._
import squants.energy._
import squants.time._
import squants.motion._
import squants.space._
import utils.Thermodynamics
import wind_energy._
import org.jfree.data.time.Year
import org.apache.commons.math3.special.Gamma
import construction.Material
import utils.TerawattHours

trait EnergyGenerationPotential {
  // [0.0 -> 1.0] a multiplicating factor for the available area, 
  // indicating the part of the grid cell that can be used for the renewable technology
  def suitabilityFactor(cell: GridCell): Double = {
    // if (cell.protectedArea) 0.0
    //else landUseFactor(cell)
    landUseFactor(cell)
  }
  def suitableArea(cell: GridCell) = cell.area * suitabilityFactor(cell)
  // % of area of the cell that is available for this technology
  def landUseFactor(cell: GridCell): Double

  // The percentage of the time the facility is assumed to be working
  def availabilityFactor(cell: GridCell): Double
  // The losses compared to the energy generated (array factor, loss in cables, ...)
  def lossFactor(cell: GridCell): Double

  // Power density = theoritical energy contained in the flux (wind, solar radiation, ...)
  def powerDensity(cell: GridCell): Irradiance

  def powerGenerated(cell: GridCell, suitabilityFactor: Option[Double] = None, density: Option[Irradiance] = None): Power
  def energyGeneratedPerYear(cell: GridCell, suitabilityFactor: Option[Double] = None, density: Option[Irradiance] = None): Energy = powerGenerated(cell, suitabilityFactor, density) * Hours(24 * 365)
  def EROI(cell: GridCell, suitabilityFactor: Option[Double] = None, density: Option[Irradiance] = None): Double
}

object SolarPotential extends EnergyGenerationPotential {

  def landUseFactor(cell: GridCell) = cell.landCovers.suitabilityFactorSolar

  def availabilityFactor(cell: GridCell) = 0.9
  def lossFactor(cell: GridCell) = 0.9
  def powerDensity(cell: GridCell) = cell.irradiance.mean

  val technologyEfficiency = 0.14
  val performanceRatio = 0.75

  def powerGenerated(cell: GridCell, suitabilityF: Option[Double] = None, density: Option[Irradiance] = None) =
    powerDensity(cell) * cell.area * suitabilityF.getOrElse(suitabilityFactor(cell)) * performanceRatio * technologyEfficiency * availabilityFactor(cell)

  def energyGeneratedPerMonth(cell: GridCell, month: Int, suitabilityF: Option[Double] = None): Energy =
    cell.irradiance.month(month) * cell.area * suitabilityF.getOrElse(suitabilityFactor(cell)) * performanceRatio * technologyEfficiency * Hours(24 * 30)

  def EROI(cell: GridCell, suitabilityF: Option[Double], density: Option[Irradiance] = None): Double = {
    if (suitabilityF.getOrElse(suitabilityFactor(cell)) == 0 || cell.irradiance.mean.value == 0) 0.0
    else {
      val out = 25 * energyGeneratedPerYear(cell, suitabilityF)
      // 2106 MJ /m^2
      val in = Megajoules(2300) * cell.area.toSquareMeters * suitabilityF.getOrElse(suitabilityFactor(cell))
      out / in
    }
  }
}