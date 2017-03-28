package wind_energy

import grid.EnergyGenerationPotential
import grid.GridCell
import squants.energy._
import squants.space._
import squants.radio._
import construction.Material
import grid.WorldGrid
import utils.Thermodynamics
import squants.time.Hours

object WindPotential extends EnergyGenerationPotential {

  def nominalPower(cell: GridCell) = if (cell.onshore) Megawatts(2) else Megawatts(5)
  def diameterRotor(cell: GridCell) = if (cell.onshore) Meters(80) else Meters(126)

  // A turbine occupied nD * nD space
  def nd(cell: GridCell, suitabilityFactor: Option[Double] = None, density: Option[Irradiance] = None) = {
    Math.sqrt(nominalPower(cell).toWatts / (density.getOrElse(capacityDensity(cell)).toWattsPerSquareMeter * (diameterRotor(cell).toMeters * diameterRotor(cell)).toMeters))
  }

  def wakeEffect(cell: GridCell, suitabilityFactor: Option[Double] = None, density: Option[Irradiance] = None) = gustavsonArrayEffect(cell, suitabilityFactor, density) // WakeEffect.wakeEffect(100, Math.ceil(nd(cell, suitabilityFactor, density)).toInt)

  def gustavsonArrayEffect(cell: GridCell, suitabilityFactor: Option[Double] = None, density: Option[Irradiance] = None) = {
    val lambda = Math.PI / Math.pow(nd(cell, suitabilityFactor, density) * 2, 2)
    GustavsonWakeEffect.arrayEfficiency(nTurbines(cell, suitabilityFactor, density), lambda)
  }

  def capacityDensity(cell: GridCell, maxDensity: Double = 5.0) = WattsPerSquareMeter(2)

  def nTurbines(cell: GridCell, suitabilityF: Option[Double] = None, density: Option[Irradiance] = None) = cell.area * suitabilityF.getOrElse(suitabilityFactor(cell)) * density.getOrElse(capacityDensity(cell)) / nominalPower(cell)

  def weight(cell: GridCell, material: Material, suitabilityF: Option[Double] = None, density: Option[Irradiance] = None) = WindFarm.weight(material, powerInstalled(cell, suitabilityF, density))

  def powerInstalled(cell: GridCell, suitabilityF: Option[Double] = None, density: Option[Irradiance] = None): Power = {
    // val d = WattsPerSquareMeter(Math.min(density.getOrElse(capacityDensity(cell)).toWattsPerSquareMeter, 1.0 / (CapacityFactorCalculation(cell) * lossFactor(cell) * availabilityFactor(cell))))
    cell.area * suitabilityF.getOrElse(suitabilityFactor(cell)) * density.getOrElse(capacityDensity(cell))
  }

  def powerGenerated(cell: GridCell, suitabilityF: Option[Double] = None, density: Option[Irradiance] = None): Power = powerInstalled(cell, suitabilityF, density) * CapacityFactorCalculation(cell) * availabilityFactor(cell) * lossFactor(cell, suitabilityF, density)

  def energyGeneratedPerYearTopDown(world: WorldGrid, cell: GridCell, suitabilityF: Option[Double] = None, density: Option[Irradiance] = None) = powerGeneratedTopDown(world, cell: GridCell, suitabilityF, density) * Hours(365 * 24)
  def powerGeneratedTopDown(world: WorldGrid, cell: GridCell, suitabilityF: Option[Double] = None, density: Option[Irradiance] = None) = {
    val res = powerGenerated(cell, suitabilityF, density)
    if ((res / (cell.area * suitabilityF.getOrElse(suitabilityFactor(cell)))).to(WattsPerSquareMeter) > world.dissipation(cell).toWattsPerSquareMeter) world.dissipation(cell) * cell.area * suitabilityF.getOrElse(suitabilityFactor(cell))
    else res
  }

  // Estimation of wind speed is for 100 metres height
  def powerDensity(cell: GridCell, wind: WindProfile) = Thermodynamics.powerDensity(wind.mean, wind.height + cell.altitude)
  def powerDensity(cell: GridCell) = powerDensity(cell, cell.wind100m)

  /**
   * Onshore we restrict the area to altitude < 2000 m
   * Offshore we restrict the area to maximum depth of 200 m and minimum distance to coast of 10 km
   * We exclude all the areas that are protected
   *
   */
  def elevationFactor(cell: GridCell): Double = {
    if (cell.onshore)
      if (cell.elevation.toMeters <= 2000) 1.0 else 0.0
    else if (cell.offshore)
      if (cell.waterDepth.toMeters <= 1000) 1.0 else 0.0
    else 0.0
  }

  def landUseFactor(cell: GridCell): Double = {
    if (cell.center.latitude.toDegrees < -60) 0.0
    else {
      elevationFactor(cell) * (if (cell.onshore) {
        (1 - cell.protectedArea) * cell.landCovers.suitabilityFactorWind
      } else {
        // EU Report 4 % of 0-10 km, 10 % of 10-50km, 25 % of > 50 km
        // NREL Report 0 % of 0-5 Nm, 33% of 5-20 Nm, 67% of > 20 Nm
        val d = cell.distanceToCoast.toNauticalMiles
        if (d < 5) 0.1
        else if (d < 20) 0.33
        else 0.67
      })
    }
  }

  // EU Report
  def availabilityFactor(cell: GridCell): Double = if (cell.offshore) 0.9 else 0.97
  def lossFactor(cell: GridCell, suitabilityF: Option[Double] = None, density: Option[Irradiance] = None): Double = wakeEffect(cell, suitabilityF, density)
  def lossFactor(cell: GridCell): Double = wakeEffect(cell)

  override def EROI(cell: GridCell, suitabilityF: Option[Double] = None, density: Option[Irradiance] = None): Double = {
    if (suitabilityF.getOrElse(suitabilityFactor(cell)) == 0 || cell.area.value == 0) 0.0
    else {
      val pow = density.getOrElse(capacityDensity(cell)) * suitabilityF.getOrElse(suitabilityFactor(cell)) * cell.area
      val out = 20 * energyGeneratedPerYear(cell, suitabilityF, density)
      val in =
        (if (cell.onshore) SimpleWindFarm.embodiedEnergy(pow)
        else SimpleWindFarm.embodiedEnergy(pow, cell.distanceToCoast, -cell.elevation))
      out / in
    }
  }

  // EROI_1MW = CF * 1MW * 20 ans / Embodied Energy 1 MW (= 15.860 GJ onshore)
  def EROI1MW(cell: GridCell): Double = {
    // We do not install wind turbines deeper than 1000 meters .. At least we don't know how much it will cost in energy !
    if (cell.waterDepth.toMeters > 1000) 0.0
    else {
      val input =
        (if (cell.onshore) SimpleWindFarm.embodiedEnergy(Megawatts(1))
        else SimpleWindFarm.embodiedEnergy(Megawatts(1), cell.distanceToCoast, cell.waterDepth))
      CapacityFactorCalculation(cell) * Megawatts(1) * Hours(365 * 24 * 20) * 0.9 / input
    }
  }
}