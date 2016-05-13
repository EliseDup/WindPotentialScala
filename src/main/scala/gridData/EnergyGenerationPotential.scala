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

trait EnergyGenerationPotential {
  // exemple W / km^2 -> assumptions for the capacity that can be installed per unit of area
  def technologyDensity(cell: GridCell): Irradiance

  // [0.0 -> 1.0] a multiplicating factor for the available area, 
  // indicating the part of the grid cell that can be used for the renewable technology
  def suitabilityFactor(cell: GridCell): Double = {
    if (cell.protectedArea||cell.center.latitude.toDegrees <= -60) 0.0
    else (1.0 - cell.urbanFactor) * landUseFactor(cell)
  }
  def landUseFactor(cell: GridCell): Double
  
  // The percentage of the time the facility is assumed to be working
  def availabilityFactor(cell: GridCell): Double
  // The losses compared to the energy generated (array factor, loss in cables, ...)
  def lossFactor(cell: GridCell): Double
  
  // The "yield" of the installation, i.e. the % if the time it is producing power at its nominal capacity
  def capacityFactor(cell: GridCell): Double

  // Power density = theoritical energy contained in the flux (wind, solar radiation, ...)
//  def powerDensity(cell: DefaultGridCell): Irradiance

  // The capacity installed is only function of the suitability factor and the technology density
  def energy1MW(cell: GridCell): Energy = capacityFactor(cell) * availabilityFactor(cell) * lossFactor(cell) * Hours(365 * 24) * Megawatts(1)

  def energyGeneratedPerYear(cell: GridCell): Energy = energyGeneratedPerYear(cell, suitabilityFactor(cell))

  def energyGeneratedPerYear(cell: GridCell, suitabilityfactor: Double): Energy = powerInstalled(cell, suitabilityfactor).toMegawatts * energy1MW(cell)

  def powerInstalled(cell: GridCell): Power = powerInstalled(cell, suitabilityFactor(cell))
  def powerInstalled(cell: GridCell, suitabilityfactor: Double): Power = cell.area * technologyDensity(cell) * suitabilityfactor
  def powerCaptured(cell: GridCell, suitabilityfactor: Double, topDown: Boolean = false): Power = powerInstalled(cell, suitabilityfactor) * capacityFactor(cell)
   
  def powerCaptured(cell: GridCell): Power = powerCaptured(cell, suitabilityFactor(cell))

  def EROI(cell: GridCell): Double

}

object WindPotential extends EnergyGenerationPotential {

  def nominalPower(cell: GridCell) = if (cell.onshore) Megawatts(2) else Megawatts(5)
  def diameterRotor(cell: GridCell) = if (cell.onshore) Meters(90) else Meters(126)

  // nD x nD
  val nD = 9
  val wakeEffect = WakeEffect.wakeEffect(100, nD)

  def areaTurbine(cell: GridCell) = nD * diameterRotor(cell) * nD * diameterRotor(cell)
  def areaRotor(cell: GridCell) = Math.PI * diameterRotor(cell) * diameterRotor(cell) / 4.0

  def technologyDensity(cell: GridCell) = nominalPower(cell) / areaTurbine(cell)

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
      if (cell.elevation.toMeters >= -200 && cell.distanceToCoast.toKilometers >= 10) 1.0 else 0.0
    else 0.0
  }
  def landUseFactor(cell: GridCell) = {
    altitudeFactor(cell)*(
    if (cell.onshore)
      if (cell.lc.isAgricultural) 0.7
      else if (cell.lc.isForest) 0.1
      else if (cell.lc.isOpenArea || cell.lc.isUrban) 0.9
      else 0.0
    else 1.0)
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

  def EROI(cell: GridCell): Double = {
    val out = 20 * energy1MW(cell)
    val in =
      if (cell.onshore) SimpleWindFarm.embodiedEnergy(Megawatts(1))
      else SimpleWindFarm.embodiedEnergy(Megawatts(1), cell.distanceToCoast, -cell.elevation)

    out / in
  }
}

object SolarPotential extends EnergyGenerationPotential {

  def technologyDensity(cell: GridCell) = Megawatts(1) / SquareKilometers(1)
  
  def landUseFactor(cell : GridCell) = {
    if(cell.lc.isForest || cell.lc.isWaterBodies) 0.0
    else if(cell.lc.isAgricultural) 0.01
    else 0.05
  }
  def availabilityFactor(cell: GridCell) = 1.0
  def lossFactor(cell: GridCell) = 0.0
  def powerDensity(cell: GridCell) = cell.irradiance

  def capacityFactor(cell: GridCell) = 0.15

  def EROI(cell: GridCell) = 10.0
}