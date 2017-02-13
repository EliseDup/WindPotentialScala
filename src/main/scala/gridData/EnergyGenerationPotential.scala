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
import utils.TerawattHours

trait EnergyGenerationPotential {
  // [0.0 -> 1.0] a multiplicating factor for the available area, 
  // indicating the part of the grid cell that can be used for the renewable technology
  def suitabilityFactor(cell: GridCell): Double = {
    if (cell.protectedArea) 0.0
    else (1.0 - cell.urbanFactor) * landUseFactor(cell)
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
    GustavsonWakeEffect.wakeEffect(nTurbines(cell, suitabilityFactor, density), lambda)
  }

  def capacityDensity(cell: GridCell, maxDensity: Double = 5.0) = WattsPerSquareMeter(2) 
  
    //4/(Math.PI * 42.5681) *  nominalPower(cell) / (diameterRotor(cell) * diameterRotor(cell)) 
   //val max = (0.385 * cell.kineticEnergyDissipation / (CapacityFactorCalculation(cell) * 0.9 * availabilityFactor(cell))).toWattsPerSquareMeter
    //val max = 1.0 / (CapacityFactorCalculation(cell) * 0.9 * availabilityFactor(cell))
    //WattsPerSquareMeter(Math.min(max, maxDensity))
  

  def nTurbines(cell: GridCell, suitabilityF: Option[Double] = None, density: Option[Irradiance] = None) = cell.area * suitabilityF.getOrElse(suitabilityFactor(cell)) * density.getOrElse(capacityDensity(cell)) / nominalPower(cell)

  def weight(cell: GridCell, material: Material, suitabilityF: Option[Double] = None, density: Option[Irradiance] = None) = WindFarm.weight(material, powerInstalled(cell, suitabilityF, density))

  def powerInstalled(cell: GridCell, suitabilityF: Option[Double] = None, density: Option[Irradiance] = None): Power = {
    // val d = WattsPerSquareMeter(Math.min(density.getOrElse(capacityDensity(cell)).toWattsPerSquareMeter, 1.0 / (CapacityFactorCalculation(cell) * lossFactor(cell) * availabilityFactor(cell))))
    cell.area * suitabilityF.getOrElse(suitabilityFactor(cell)) * density.getOrElse(capacityDensity(cell))
  }

  def powerGenerated(cell: GridCell, suitabilityF: Option[Double] = None, density: Option[Irradiance] = None): Power = {
    val res = powerInstalled(cell, suitabilityF, density) * CapacityFactorCalculation(cell) * availabilityFactor(cell) * lossFactor(cell, suitabilityF, density)
    // if ((res / cell.area * suitabilityF.getOrElse(suitabilityFactor(cell))).to(WattsPerSquareMeter) > 1.0) WattsPerSquareMeter(1.0) * cell.area * suitabilityF.getOrElse(suitabilityFactor(cell)) else res
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
  def elevationFactor(cell: GridCell) = {
    if (cell.onshore)
      if (cell.elevation.toMeters <= 2000) 1.0 else 0.0
    else if (cell.offshore)
      if (cell.waterDepth.toMeters <= 1000) 1.0 else 0.0
    else 0.0
  }

  def landUseFactor(cell: GridCell) = {
    if (cell.center.latitude.toDegrees < -60) 0.0
    else {
      val cover = cell.lc
      elevationFactor(cell) * (
        if (cell.onshore) {
          if (cover.croplands) 0.7
          else if (cover.grassland || cover.sparseVegetation || cover.bareAreas) 0.8
          else if (cover.shrubland) 0.5
          else if (cover.mosaicGrasslandForestShrubland) 0.5 * 0.5 + 0.5 * 0.8
          else if (cover.mosaicVegetationCropland) 0.5 * 0.7 + 0.5 * 0.8
          else if (cover.forests) 0.1
          else 0.0
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
  def windRegimeFactor(cell: GridCell) = if (cell.windSpeed.toMetersPerSecond >= 4) 1.0 else 0.0

  def loadHoursLinear(cell: GridCell) =
    // EU REPORT
    Hours(Math.max(0, Math.min(5500, 626.51 * cell.windSpeedHub.value - 1901)))
  // HOOGWIJK REPORT -> 
  // Hours(Math.max(0, Math.min(4000, 565 * windSpeedAtHub(cell).value - 1745)))

  // EU Report
  def availabilityFactor(cell: GridCell): Double = 1.0 // if (cell.offshore /*|| cell.elevation.toMeters >= 600*/ ) 0.9 else 0.97
  def lossFactor(cell: GridCell, suitabilityF: Option[Double] = None, density: Option[Irradiance] = None): Double = wakeEffect(cell, suitabilityF, density)
  def lossFactor(cell: GridCell): Double = wakeEffect(cell)

  def maxExergy(cell: GridCell) = maxPowerDensity(cell) * cell.area
  def maxPowerDensity(cell: GridCell) = cell.kineticEnergyDissipation * 0.385

  override def EROI(cell: GridCell, suitabilityF: Option[Double] = None, density: Option[Irradiance] = None): Double = {
    if (suitabilityF.getOrElse(suitabilityFactor(cell)) == 0) 0.0
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

  def powerGenerated(cell: GridCell, suitabilityF: Option[Double] = None, density: Option[Irradiance] = None) =
    powerDensity(cell) * cell.area * suitabilityF.getOrElse(suitabilityFactor(cell)) * performanceRatio * technologyEfficiency * availabilityFactor(cell)

  def energyGeneratedPerMonth(cell: GridCell, month: Int, suitabilityF: Option[Double] = None): Energy =
    cell.irradiance.perMonth(month) * cell.area * suitabilityF.getOrElse(suitabilityFactor(cell)) * performanceRatio * technologyEfficiency * Hours(24 * 30)

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