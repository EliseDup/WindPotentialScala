package gridData

import squants.radio._
import squants.energy._
import squants.time._
import squants.motion._
import squants.space._
import utils.Thermodynamics
import windEnergy._
import org.jfree.data.time.Year
import org.apache.commons.math3.special.Gamma
import construction.Material

trait EnergyGenerationPotential {

  // [0.0 -> 1.0] a multiplicating factor for the available area, 
  // indicating the part of the grid cell that can be used for the renewable technology
  def suitabilityFactor(cell: GridCell): Double = {
    if (cell.protectedArea || cell.center.latitude.toDegrees <= -60) 0.0
    else (1.0 - cell.urbanFactor) * landUseFactor(cell)
  }
  // % of area of the cell that is available for this technology
  def landUseFactor(cell: GridCell): Double

  // The percentage of the time the facility is assumed to be working
  def availabilityFactor(cell: GridCell): Double
  // The losses compared to the energy generated (array factor, loss in cables, ...)
  def lossFactor(cell: GridCell): Double

  // Power density = theoritical energy contained in the flux (wind, solar radiation, ...)
  def powerDensity(cell: GridCell): Irradiance

  def powerGenerated(cell: GridCell): Power = powerGenerated(cell, suitabilityFactor(cell))
  def powerGenerated(cell: GridCell, suitabilityFactor: Double): Power
  def powerGeneratedExergy(cell: GridCell, suitabilityFactor: Double) = powerGenerated(cell, suitabilityFactor)

  def energyGeneratedPerYear(cell: GridCell): Energy = energyGeneratedPerYear(cell, suitabilityFactor(cell))
  def energyGeneratedPerYear(cell: GridCell, suitabilityFactor: Double): Energy = powerGenerated(cell, suitabilityFactor) * Hours(24 * 365)
  def energyGeneratedPerYearExergy(cell: GridCell, suitabilityFactor: Double) = energyGeneratedPerYear(cell, suitabilityFactor)

  def EROI(cell: GridCell): Double = EROI(cell, suitabilityFactor(cell))
  def EROI(cell: GridCell, suitabilityFactor: Double): Double
  def EROIExergy(cell: GridCell, suitabilityFactor: Double) = EROI(cell, suitabilityFactor)
}

object WindPotential extends EnergyGenerationPotential {

  def nominalPower(cell: GridCell) = if (cell.onshore) Megawatts(2) else Megawatts(5)
  def diameterRotor(cell: GridCell) = if (cell.onshore) Meters(90) else Meters(126)

  // nD x nD
  val nD = 9
  val wakeEffect = WakeEffect.wakeEffect(100, nD)

  def areaTurbine(cell: GridCell) = nD * diameterRotor(cell) * nD * diameterRotor(cell)
  def areaRotor(cell: GridCell) = Math.PI * diameterRotor(cell) * diameterRotor(cell) / 4.0

  def technologyDensity(cell: GridCell) = nominalPower(cell) / SquareKilometers(1) // nominalPower(cell) / areaTurbine(cell)
  def nTurbines(cell: GridCell) = cell.area * suitabilityFactor(cell) / areaTurbine(cell)

  def weight(cell: GridCell, material: Material) = WindFarm.weight(material, powerInstalled(cell))

  def powerInstalled(cell: GridCell): Power = powerInstalled(cell, suitabilityFactor(cell))
  def powerInstalled(cell: GridCell, suitabilityFactor: Double): Power = cell.area * suitabilityFactor * technologyDensity(cell)

  def powerGenerated(cell: GridCell, suitabilityFactor: Double): Power = {
    powerInstalled(cell, suitabilityFactor) * capacityFactor(cell) * availabilityFactor(cell)
  }
  def powerGeneratedPerArea(cell: GridCell, suitabilityFactor: Double): Irradiance = {
    if (powerGenerated(cell, suitabilityFactor).value == 0) WattsPerSquareMeter(0)
    else powerGenerated(cell, suitabilityFactor) / (cell.area * suitabilityFactor)
  }
  override def energyGeneratedPerYearExergy(cell: GridCell, suitabilityFactor: Double): Energy = {
    powerGeneratedExergy(cell, suitabilityFactor) * Hours(365 * 24)
  }
  override def powerGeneratedExergy(cell: GridCell, suitabilityFactor: Double): Power = {
    val p = powerInstalled(cell, suitabilityFactor) * capacityFactor(cell) * availabilityFactor(cell) * lossFactor(cell)
    val res = if (p > maxExergy(cell)) maxExergy(cell) else p
    res
  }
  // Measurement of wind speed is taken at 10 metres height
  def hubAltitude(cell: GridCell) = Meters(Math.max(0.0, cell.elevation.toMeters) + cell.hubHeight.toMeters)
  def powerDensity(cell: GridCell) = Thermodynamics.powerDensity(cell.windSpeed, hubAltitude(cell))
  def powerDensityAtHub(cell: GridCell) = Thermodynamics.powerDensity(cell.windSpeedHub, hubAltitude(cell))

  /**
   * Onshore we restrict the area to altitude < 2000 m
   * Offshore we restrict the area to maximum depth of 200 m and minimum distance to coast of 10 km
   * We exclude all the areas that are protected
   *
   */
  def altitudeFactor(cell: GridCell) = {
    if (cell.onshore)
      if (cell.elevation.toMeters <= 2000) 1.0 else 0.0
    else if (cell.offshore)
      if (cell.elevation.toMeters >= -200) 1.0 else 0.0
    else 0.0
  }
  def landUseFactor(cell: GridCell) = {
    val cover = cell.lc
    altitudeFactor(cell) * (
      if (cell.onshore) {
        if (cover.croplands) 0.7
        else if (cover.grassland || cover.sparseVegetation || cover.bareAreas) 0.8
        else if (cover.shrubland) 0.5
        else if (cover.mosaicGrasslandForestShrubland) 0.5 * 0.8
        else if (cover.mosaicVegetationCropland) 0.5 * 0.7 + 0.5 * 0.8
        else 0.0
      } else {
        val d = cell.distanceToCoast.toKilometers
        if (d < 10) 0.04
        else if (d < 50) 0.1
        else 0.25
      })
  }
  def windRegimeFactor(cell: GridCell) = if (cell.windSpeed.toMetersPerSecond >= 4) 1.0 else 0.0

  def loadHours(cell: GridCell) =
    // EU REPORT
    Hours(Math.max(0, Math.min(5500, 626.51 * cell.windSpeedHub.value - 1901)))
  // HOOGWIJK REPORT -> 
  // Hours(Math.max(0, Math.min(4000, 565 * windSpeedAtHub(cell).value - 1745)))

  // EU Report
  def availabilityFactor(cell: GridCell): Double = if (cell.offshore /*|| cell.elevation.toMeters >= 600*/ ) 0.9 else 0.97
  def lossFactor(cell: GridCell): Double = wakeEffect
  def capacityFactor(cell: GridCell): Double = CapacityFactorCalculation(cell.weibull)

 // def energyGenerated1MW(cell: GridCell) = capacityFactor(cell) * availabilityFactor(cell) * lossFactor(cell) * Megawatts(1) * Hours(365 * 24)

  def maxExergy(cell: GridCell) = maxPowerDensity(cell) * cell.area 
  def maxPowerDensity(cell: GridCell) = cell.kineticEnergyDissipation * 0.385 *  16.0 / 27

  def EROI(cell: GridCell, suitabilityFactor: Double): Double = {
    if (suitabilityFactor == 0) 0.0
    else {
      val out = 20 * energyGeneratedPerYear(cell, suitabilityFactor)
      val in =
        (if (cell.onshore) SimpleWindFarm.embodiedEnergy(powerInstalled(cell, suitabilityFactor))
        else SimpleWindFarm.embodiedEnergy(powerInstalled(cell, suitabilityFactor), cell.distanceToCoast, -cell.elevation))
      out / in
    }
  }
  override def EROIExergy(cell: GridCell, suitabilityFactor: Double): Double = {
    if (suitabilityFactor == 0) 0.0
    else {
      val out = 20 * energyGeneratedPerYearExergy(cell, suitabilityFactor)
      val in =
        (if (cell.onshore) SimpleWindFarm.embodiedEnergy(powerInstalled(cell, suitabilityFactor))
        else SimpleWindFarm.embodiedEnergy(powerInstalled(cell, suitabilityFactor), cell.distanceToCoast, -cell.elevation))
      out / in
    }
  }

}

object SolarPotential extends EnergyGenerationPotential {

  def landUseFactor(cell: GridCell) = {
    val cover = cell.lc
    if (cover.croplands || cover.shrubland || cover.sparseVegetation) 0.01
    else if (cover.mosaicGrasslandForestShrubland || cover.mosaicVegetationCropland) 0.01
    else if (cover.grassland || cover.bareAreas) 0.05
    else 0.0
  }

  def availabilityFactor(cell: GridCell) = 0.9
  def lossFactor(cell: GridCell) = 0.9
  def powerDensity(cell: GridCell) = cell.irradiance.mean

  val technologyEfficiency = 0.14
  val performanceRatio = 0.75

  def powerGenerated(cell: GridCell, suitabilityFactor: Double) =
    powerDensity(cell) * cell.area * suitabilityFactor * performanceRatio * technologyEfficiency * availabilityFactor(cell)

  def energyGeneratedPerMonth(cell: GridCell, month: Int, suitabilityFactor: Double): Energy =
    cell.irradiance.perMonth(month) * cell.area * suitabilityFactor * performanceRatio * technologyEfficiency * Hours(24 * 30)

  def energyGeneratedPerMonth(cell: GridCell, month: Int): Energy = energyGeneratedPerMonth(cell, month, suitabilityFactor(cell))

  def EROI(cell: GridCell, suitabilityFactor: Double): Double = {
    if (suitabilityFactor == 0 || cell.irradiance.mean.value == 0) 0.0
    else {
      val out = 25 * energyGeneratedPerYear(cell, suitabilityFactor)
      // 2106 MJ /m^2
      val in = Megajoules(2300) * cell.area.toSquareMeters * suitabilityFactor
      out / in
    }
  }
}