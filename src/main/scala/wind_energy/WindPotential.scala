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
import squants.motion.Velocity
import squants.motion.MetersPerSecond

object WindPotential extends EnergyGenerationPotential {

  val lifeTimeYears = 25.0
  
  // A turbine occupied nD * nD space
 /* def nd(cell: GridCell, density: Option[Irradiance] = None) = Math.sqrt(314.0 / density.getOrElse(capacityDensity(cell)).toWattsPerSquareMeter)
  def gustavsonArrayEffect(cell: GridCell, suitable: Boolean = true, density: Option[Irradiance] = None) = {
    val lambda = Math.PI / Math.pow(nd(cell, density) * 2, 2)
    GustavsonWakeEffect.arrayEfficiency(nTurbines(cell, suitable, density), lambda)
  }

  def capacityDensity(cell: GridCell, maxDensity: Double = 5.0) = WattsPerSquareMeter(2)

  def nominalPower(cell: GridCell) = if (cell.onshore) Megawatts(3) else Megawatts(8)
  def nTurbines(cell: GridCell, suitable: Boolean, density: Option[Irradiance] = None) = cell.suitableArea(suitable) * density.getOrElse(capacityDensity(cell)) / nominalPower(cell)
*/
  // Estimation of wind speed is for 100 metres height
  def powerDensity(cell: GridCell, wind: WindProfile) = Thermodynamics.windPowerDensity(wind.mean, wind.height + cell.altitude)
  def powerDensity(cell: GridCell) = powerDensity(cell, cell.wind100m)

  /**
   * Offshore we restrict the area to maximum depth of 1000 m and Exclusive Economic Zones
   * We exclude all the areas that are protected
   */
  def elevationFactor(cell: GridCell): Double = {
    if (cell.onshore) 1.0
    else if (cell.offshore && cell.EEZ)
      if (cell.waterDepth.toMeters <= 1000) 1.0 else 0.0
    else 0.0
  }

  val excludedCountries = List("NA", "Antarctica", "Greenland", "French Southern & Antarctic Lands")

  def landUseFactor(cell: GridCell): Double = {
    if (excludedCountries.contains(cell.country.name) || cell.country.name.contains("Is.") || cell.country.name.contains("Islands")) 0.0
    else {
      (1 - cell.protectedArea) * elevationFactor(cell) * (if (cell.onshore) {
        cell.landCovers.suitabilityFactorWind
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
  def availabilityFactor(cell: GridCell): Double = if (cell.offshore) 0.95 else 0.97
 // def lossFactor(cell: GridCell, suitable: Boolean = true, density: Option[Irradiance] = None): Double = gustavsonArrayEffect(cell, suitable, density)
//  def lossFactor(cell: GridCell): Double = gustavsonArrayEffect(cell)

  def energyInputs(installedCapacity: Power, energyOutput: Energy, cell: GridCell) = {
    (if (cell.onshore) onshoreEnergyInputs(installedCapacity, energyOutput, cell.distanceToCoast)
    else offshoreEnergyInputs(installedCapacity, energyOutput, cell.distanceToCoast, cell.waterDepth))
  }

  def offshoreEnergyInputs(installedCapacity: Power, energyOutput: Energy, distanceToCoast: Length, waterDepth: Length) = {
    WindFarmEnergyInputs.offshoreEnergyInputs(installedCapacity, energyOutput, waterDepth, distanceToCoast)
  }
  def onshoreEnergyInputs(installedCapacity: Power, energyOutput: Energy, distanceToCoast: Length) = {
    WindFarmEnergyInputs.onshoreEnergyInputs(installedCapacity, energyOutput, distanceToCoast)
  }

  // Relationship between rated power, rotor diameter and rated wind speed
  val cp = 0.5
  def coeff(elevation: Length) = 0.5 * cp * Thermodynamics.airDensity(elevation).toKilogramsPerCubicMeter * Math.PI / 4
  val defaultVr = MetersPerSecond(11)

  def rotorDiameter(ratedPower: Power, ratedSpeed: Velocity, hubAltitude: Length) = Meters(Math.sqrt(ratedPower.toWatts / (coeff(hubAltitude) * Math.pow(ratedSpeed.toMetersPerSecond, 3))))
  def ratedSpeed(ratedPower: Power, rotorDiameter: Length, hubAltitude: Length) = MetersPerSecond(Math.pow(ratedPower.toWatts / (coeff(hubAltitude) * Math.pow(rotorDiameter.toMeters, 2)), 1.0 / 3))
  def ratedPower(rotorDiameter: Length, ratedSpeed: Velocity, hubAltitude: Length) = Watts(coeff(hubAltitude) * Math.pow(rotorDiameter.toMeters, 2) * Math.pow(ratedSpeed.toMetersPerSecond, 3))
  def capacityDensity(ratedSpeed: Velocity, n: Double, hubAltitude: Length): Irradiance = WattsPerSquareMeter(coeff(hubAltitude) * Math.pow(ratedSpeed.toMetersPerSecond, 3) / (n * n))
  def capacityDensity(cell: GridCell, eroi_min: Double, suitable: Boolean): Irradiance = {
    if (cell.suitableArea(suitable).value > 0) capacityDensity(cell.optimalRatedSpeed(eroi_min), cell.optimalN(eroi_min), cell.hubAltitude)
    else WattsPerSquareMeter(0)
  }

  def spacingParameter(ratedSpeed: Velocity, density: Irradiance, hubAltitude: Length) = Math.sqrt(coeff(hubAltitude) * Math.pow(ratedSpeed.toMetersPerSecond, 3) / density.toWattsPerSquareMeter)

  def installedCapacity(cell: GridCell, vr: Velocity, n: Double, suitable: Boolean): Power = cell.suitableArea(suitable) * capacityDensity(vr, n, cell.hubAltitude)
  def installedCapacity(cell: GridCell, eroi_min: Double, suitable: Boolean): Power = installedCapacity(cell, cell.optimalRatedSpeed(eroi_min), cell.optimalN(eroi_min), suitable)

  def power(cell: GridCell, vr: Velocity, n: Double, suitable: Boolean, topDown: Boolean = false): Power = {
    val wi = installedCapacity(cell, vr, n, suitable)
    val res = wi * CapacityFactorCalculation.cubic(cell, vr.toMetersPerSecond) * GustavsonWakeEffect.arrayEfficiency(wi.toMegawatts / 3.0, Math.PI / (4 * Math.pow(n, 2))) * availabilityFactor(cell)
    if (topDown && res / cell.suitableArea(suitable) > cell.keDissipation) cell.keDissipation * cell.suitableArea(suitable)
    else res
  }
  def power(cell: GridCell, eroi_min: Double, suitable: Boolean): Power = power(cell, cell.optimalRatedSpeed(eroi_min), cell.optimalN(eroi_min), suitable)

  def powerDensity(cell: GridCell, eroi_min: Double, suitable: Boolean): Irradiance = {
    if (cell.suitableArea(suitable).value > 0) power(cell, eroi_min, suitable) / cell.suitableArea(suitable) else WattsPerSquareMeter(0)
  }

  def energyPerYear(cell: GridCell, vr: Velocity, n: Double, suitable: Boolean, topDown: Boolean = false): Energy = {
    power(cell, vr, n, suitable, topDown) * Hours(365 * 24)
  }
  def energyPerYear(cell: GridCell, density: Irradiance, suitable: Boolean, topDown: Boolean): Energy = {
    energyPerYear(cell, defaultVr, spacingParameter(defaultVr, density, cell.hubAltitude), suitable, topDown)
  }
  def netEnergyPerYear(cell: GridCell, vr: Velocity, n: Double, suitable: Boolean, topDown: Boolean = false): Energy = {
    val wi = installedCapacity(cell, vr, n, suitable)
    val out = energyPerYear(cell, vr, n, suitable, topDown) * 25
    (out - energyInputs(wi, out, cell)) / 25.0
  }
  def netEnergyPerYear(cell: GridCell, eroi_min: Double, suitable: Boolean): Energy = netEnergyPerYear(cell, cell.optimalRatedSpeed(eroi_min), cell.optimalN(eroi_min), suitable)
  def netEnergyPerYear(cell: GridCell, density: Irradiance, suitable: Boolean, topDown: Boolean): Energy = netEnergyPerYear(cell, defaultVr, spacingParameter(defaultVr, density, cell.hubAltitude), suitable, topDown)

  def eroi(cell: GridCell, vr: Velocity, n: Double, suitable: Boolean, topDown: Boolean = false): Double = {
    val wi = installedCapacity(cell, vr, n, suitable)
    if (wi.value == 0) 0
    else {
      val out = energyPerYear(cell, vr, n, suitable, topDown) * 25
      out / energyInputs(wi, out, cell)
    }
  }
  def eroi(cell: GridCell, eroi_min: Double, suitable: Boolean): Double = eroi(cell, cell.optimalRatedSpeed(eroi_min), cell.optimalN(eroi_min), suitable)
  def eroi(cell: GridCell, density: Irradiance, suitable: Boolean, topDown: Boolean): Double = eroi(cell, defaultVr, spacingParameter(defaultVr, density, cell.hubAltitude), suitable, topDown)

  def minimumEfficiency(cell: GridCell, eroi_min: Double): Double = {
    eroi_min * energyInputs(Megawatts(1), 0.2 * Megawatts(1) * Hours(365 * 24 * 25), cell) / (Megawatts(1) * Hours(365 * 24 * 25))
  }

  // RESULTS
  def potentialFixedDensity(density: Irradiance, eroi_min: Double, grids: List[GridCell], suitable: Boolean = true, topDown: Boolean = true): Energy =
    grids.map(g => (if (WindPotential.eroi(g, density, suitable, topDown) >= eroi_min) energyPerYear(g, density, suitable, topDown) else Joules(0))).foldLeft(Joules(0))(_ + _)
  def netPotentialFixedDensity(density: Irradiance, eroi_min: Double, grids: List[GridCell], suitable: Boolean = true, topDown: Boolean = true): Energy =
    grids.map(g => (if (WindPotential.eroi(g, density, suitable, topDown) >= eroi_min) netEnergyPerYear(g, density, suitable, topDown) else Joules(0))).foldLeft(Joules(0))(_ + _)

}