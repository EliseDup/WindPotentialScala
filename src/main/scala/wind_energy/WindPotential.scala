package wind_energy

import grid.EnergyGenerationPotential
import grid.GridCell
import squants.energy._
import squants.space._
import squants.radio._
import construction.Material
import grid.WorldGrid
import utils.WindPower
import squants.time.Hours
import squants.motion.Velocity
import squants.motion.MetersPerSecond
import utils.Helper

object WindPotential {
  def apply() = new WindPotential()
  def apply(cp_max: Double) = new WindPotential(cp_max)
  def apply(cp_max: Double, topDown : Boolean) = new WindPotential(cp_max, topDown)
}

class WindPotential(val cp_max: Double = 0.5, val top_down: Boolean = true) extends EnergyGenerationPotential {

  val lifeTimeYears = 25.0

  // Estimation of wind speed is for 100 metres height
  def powerDensity(cell: GridCell, wind: WindProfile) = WindPower.windPowerDensity(wind.mean, wind.height + cell.altitude)
  def powerDensity(cell: GridCell) = powerDensity(cell, cell.wind100m)

  /**
   * Offshore we restrict the area to maximum depth of 1000 m and Exclusive Economic Zones
   * We exclude all the areas that are protected
   */
  def elevationFactor(cell: GridCell): Double = {
    if (cell.onshore || (cell.offshore && cell.EEZ && cell.waterDepth.toMeters <= 1000)) 1.0
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

  def availabilityFactor(cell: GridCell): Double = if (cell.offshore) 0.95 else 0.97

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
  // Power_rated = 1/2 * Cp_max * rho * PI / 4 * D^2 * v_rated^3
  // => v_rated = (Power_rated / (1/2 * Cp_max * rho * PI / 4 * D^2) )^(1/3)
  // => D = (Power_rated / (1/2 * Cp_max * rho * PI / 4 * v^3) )^(1/2)
  def coeff(elevation: Length) = 0.5 * cp_max * WindPower.airDensity(elevation).toKilogramsPerCubicMeter * Math.PI / 4
  val defaultVr = MetersPerSecond(11)

  def ratedPower(rotorDiameter: Length, ratedSpeed: Velocity, hubAltitude: Length) = Watts(coeff(hubAltitude) * Math.pow(rotorDiameter.toMeters, 2) * Math.pow(ratedSpeed.toMetersPerSecond, 3))
  def rotorDiameter(ratedPower: Power, ratedSpeed: Velocity, hubAltitude: Length) = Meters(Math.sqrt(ratedPower.toWatts / (coeff(hubAltitude) * Math.pow(ratedSpeed.toMetersPerSecond, 3))))
  def ratedSpeed(ratedPower: Power, rotorDiameter: Length, hubAltitude: Length) = MetersPerSecond(Math.pow(ratedPower.toWatts / (coeff(hubAltitude) * Math.pow(rotorDiameter.toMeters, 2)), 1.0 / 3))
  // Capacity Density = W/m^2, with nD*nD between 2 turbines
  // Rated Power / (nD*nD) = 1/2 * Cp_max * rho * PI / 4 * D^2 * v_rated^3 / (nD*nD) = 1/2 * Cp_max * rho * PI / 4 * v_rated^3  / n^2
  def capacityDensity(ratedSpeed: Velocity, n: Double, hubAltitude: Length): Irradiance = WattsPerSquareMeter(coeff(hubAltitude) * Math.pow(ratedSpeed.toMetersPerSecond, 3) / (n * n))
  def capacityDensity(cell: GridCell, eroi_min: Double, suitable: Boolean): Irradiance = {
    if (cell.suitableArea(suitable).value > 0) capacityDensity(cell.optimalRatedSpeed(eroi_min), cell.optimalN(eroi_min), cell.hubAltitude)
    else WattsPerSquareMeter(0)
  }

  def spacingParameter(ratedSpeed: Velocity, density: Irradiance, hubAltitude: Length) = Math.sqrt(coeff(hubAltitude) * Math.pow(ratedSpeed.toMetersPerSecond, 3) / density.toWattsPerSquareMeter)
  def installedCapacity(cell: GridCell, vr: Velocity, n: Double, suitable: Boolean): Power = cell.suitableArea(suitable) * capacityDensity(vr, n, cell.hubAltitude)
  def installedCapacity(cell: GridCell, eroi_min: Double, suitable: Boolean): Power = installedCapacity(cell, cell.optimalRatedSpeed(eroi_min), cell.optimalN(eroi_min), suitable)
 def installedCapacity(eroi_min: Double, suitable: Boolean = true, grids: List[GridCell]): Power = grids.map(installedCapacity(_, eroi_min, suitable)).foldLeft(Watts(0))(_ + _)
     
  def power(cell: GridCell, vr: Velocity, n: Double, suitable: Boolean): Power = {
    val wi = installedCapacity(cell, vr, n, suitable)
    val res = wi * CapacityFactorCalculation.cubic(cell, vr.toMetersPerSecond) * WakeEffect.arrayEfficiency(wi.toMegawatts / 3.0, Math.PI / (4 * Math.pow(n, 2))) * availabilityFactor(cell)
    if (top_down && res / cell.suitableArea(suitable) > cell.keDissipation) cell.keDissipation * cell.suitableArea(suitable)
    else res
  }

  def power(cell: GridCell, eroi_min: Double, suitable: Boolean): Power = power(cell, cell.optimalRatedSpeed(eroi_min), cell.optimalN(eroi_min), suitable)

  def powerDensity(cell: GridCell, eroi_min: Double, suitable: Boolean): Irradiance = {
    if (cell.suitableArea(suitable).value > 0) power(cell, eroi_min, suitable) / cell.suitableArea(suitable) else WattsPerSquareMeter(0)
  }

  def energyPerYear(cell: GridCell, vr: Velocity, n: Double, suitable: Boolean): Energy = {
    power(cell, vr, n, suitable) * Hours(365 * 24)
  }
  def energyPerYear(cell: GridCell, density: Irradiance, suitable: Boolean): Energy = {
    energyPerYear(cell, defaultVr, spacingParameter(defaultVr, density, cell.hubAltitude), suitable)
  }
  def netEnergyPerYear(cell: GridCell, vr: Velocity, n: Double, suitable: Boolean): Energy = {
    val wi = installedCapacity(cell, vr, n, suitable)
    val out = energyPerYear(cell, vr, n, suitable) * lifeTimeYears
    (out - energyInputs(wi, out, cell)) / lifeTimeYears
  }
  def netEnergyPerYear(cell: GridCell, eroi_min: Double, suitable: Boolean): Energy = netEnergyPerYear(cell, cell.optimalRatedSpeed(eroi_min), cell.optimalN(eroi_min), suitable)
  def netEnergyPerYear(cell: GridCell, density: Irradiance, suitable: Boolean): Energy = netEnergyPerYear(cell, spacingParameter(defaultVr, density, cell.hubAltitude), suitable)

  def eroi(cell: GridCell, vr: Velocity, n: Double, suitable: Boolean): Double = {
    val wi = installedCapacity(cell, vr, n, suitable)
    if (wi.value == 0) 0
    else {
      val out = energyPerYear(cell, vr, n, suitable) * lifeTimeYears
      out / energyInputs(wi, out, cell)
    }
  }
  def eroi(cell: GridCell, eroi_min: Double, suitable: Boolean): Double = eroi(cell, cell.optimalRatedSpeed(eroi_min), cell.optimalN(eroi_min), suitable)
  def eroi(cell: GridCell, density: Irradiance, suitable: Boolean): Double = eroi(cell, defaultVr, spacingParameter(defaultVr, density, cell.hubAltitude), suitable)

  def minimumEfficiency(cell: GridCell, eroi_min: Double): Double = {
    eroi_min * energyInputs(Megawatts(1), 0.2 * Megawatts(1) * Hours(365 * 24 * 25), cell) / (Megawatts(1) * Hours(365 * 24 * 25))
  }

  // RESULTS
  def potentialFixedDensity(density: Irradiance, eroi_min: Double, grids: List[GridCell], suitable: Boolean = true): Energy =
    grids.map(g => (if (eroi(g, density, suitable) >= eroi_min) energyPerYear(g, density, suitable) else Joules(0))).foldLeft(Joules(0))(_ + _)
  def netPotentialFixedDensity(density: Irradiance, eroi_min: Double, grids: List[GridCell], suitable: Boolean = true): Energy =
    grids.map(g => (if (eroi(g, density, suitable) >= eroi_min) netEnergyPerYear(g, density, suitable) else Joules(0))).foldLeft(Joules(0))(_ + _)

  def meanCfCountry(world: WorldGrid, country: String, meanSpeed: Velocity) = {
    val c = world.country(country).filter(_.wind100m.mean >= meanSpeed)
    Math.round(Helper.mean(c.map(g => (g, CapacityFactorCalculation(g) * 1000)))) / 10.0
  }

  def meanEfficiency(cells: List[GridCell], eroi_min: Double) = {
    Math.round(Helper.mean(cells.map(g => (g, ((installedCapacity(g, eroi_min, true) * Hours(365 * 24)) / energyPerYear(g, eroi_min, true)) * 1000)))) / 10.0
  }
  def meanArrayEffect(cells : List[GridCell], eroi_min : Double) = {
    Helper.mean(cells.map(g => (g,{
       val wi = installedCapacity(g, eroi_min, true)
       val n = g.optimalN(eroi_min)
      WakeEffect.arrayEfficiency(wi.toMegawatts / 3.0, Math.PI / (4 * Math.pow(n, 2)))
    })))
  }
  def meanCf(cells: List[GridCell]) = {
    Math.round(Helper.mean(cells.map(g => (g, CapacityFactorCalculation(g) * 1000)))) / 10.0
  }
  def meanProductionDensity(cells: List[GridCell], e: Double) = {
    Helper.mean(cells.filter(_.optimalRatedSpeed(e).toMetersPerSecond > 0).map(g => (g, powerDensity(g, e, true).toWattsPerSquareMeter)))
  }
  def minProductionDensity(cells: List[GridCell], e: Double) = cells.filter(_.optimalRatedSpeed(e).toMetersPerSecond > 0).map(powerDensity(_, e, true).toWattsPerSquareMeter).min
  def maxProductionDensity(cells: List[GridCell], e: Double) = cells.filter(_.optimalRatedSpeed(e).toMetersPerSecond > 0).map(powerDensity(_, e, true).toWattsPerSquareMeter).max
  def minCapacityDensity(cells: List[GridCell], e: Double) = cells.filter(_.optimalRatedSpeed(e).toMetersPerSecond > 0).map(capacityDensity(_, e, true).toWattsPerSquareMeter).min
  def maxCapacityDensity(cells: List[GridCell], e: Double) = cells.filter(_.optimalRatedSpeed(e).toMetersPerSecond > 0).map(capacityDensity(_, e, true).toWattsPerSquareMeter).max
  def meanCapacityDensity(cells: List[GridCell], e: Double) = {
    Helper.mean(cells.filter(_.optimalRatedSpeed(e).toMetersPerSecond > 0).map(g => (g, capacityDensity(g, e, true).toWattsPerSquareMeter)))
  }

  def powerInstalled(eroi_min: Double, suitable: Boolean, grids: List[GridCell]): Power = grids.map(g => installedCapacity(g, eroi_min, suitable)).foldLeft(Watts(0))(_ + _)
  def area(eroi_min: Double, suitable: Boolean, grids: List[GridCell]): Area = grids.map(g => if (g.optimalRatedSpeed(eroi_min).value > 0) (g.suitableArea(suitable, this)) else SquareKilometers(0)).foldLeft(SquareKilometers(0))(_ + _)

}

object WTSpecifications {

}