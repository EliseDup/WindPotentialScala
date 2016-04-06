package gridData

import squants.radio._
import squants.energy._
import squants.time._
import squants.motion._
import squants.space._
import utils.Thermodynamics
import windEnergy._
import org.jfree.data.time.Year

trait EnergyGenerationPotential {
  // exemple W / km^2 -> assumptions for the capacity that can be installed per unit of area
  def technologyDensity(cell: GridCell): Irradiance

  // [0.0 -> 1.0] a multiplicating factor for the available area, 
  // indicating the part of the grid cell that can be used for the renewable technology
  def suitabilityFactor(cell: GridCell): Double
  // The percentage of the time the facility is assumed to be working
  def availabilityFactor(cell: GridCell): Double
  // The losses compared to the energy generated (array factor, loss in cables, ...)
  def lossFactor(cell: GridCell): Double
  // The "yield" of the wind turbine, i.e. the % if the time it is producing power at its nominal capacity
  def capacityFactor(cell: GridCell): Double

  // Power density = theoritical energy contained in the flux (wind, solar radiation, ...)
  def powerDensity(cell: GridCell): Irradiance

  // The capacity installed is only function of the suitability factor and the technology density
  def energy1MW(cell: GridCell): Energy = capacityFactor(cell) * availabilityFactor(cell) * lossFactor(cell) * Hours(365 * 24) * Megawatts(1)

  def energyGeneratedPerYear(cell: GridCell): Energy = energyGeneratedPerYear(cell, suitabilityFactor(cell)*(if(cell.windSpeed.toMetersPerSecond>=4) 1.0 else 0.0))
  def energyGeneratedPerYear(cell: GridCell, suitabilityfactor: Double): Energy = powerInstalled(cell, suitabilityfactor).toMegawatts * energy1MW(cell)

  def powerInstalled(cell: GridCell): Power = powerInstalled(cell, suitabilityFactor(cell))
  def powerInstalled(cell: GridCell, suitabilityfactor: Double): Power = cell.area * technologyDensity(cell) * suitabilityfactor
  
  def EROI(cell: GridCell): Double

}

object WindPotential extends EnergyGenerationPotential {

  def technologyDensity(cell: GridCell) =
    if (cell.onshore) Megawatts(2) / SquareKilometers(Math.pow(9 * 0.09, 2))
    else Megawatts(5) / SquareKilometers(Math.pow(9 * 0.126, 2))

  // Measurement of wind speed is taken at 10 metres height
  val h0 = Meters(10)
  def hubHeight(cell: GridCell): Length = if (cell.onshore) Meters(80) else Meters(90)
  def hubAltitude(cell: GridCell) = Meters(Math.max(0.0, cell.elevation.toMeters) + hubHeight(cell).toMeters)
  def powerDensity(cell: GridCell) = Thermodynamics.powerDensity(cell.windSpeed, hubAltitude(cell))
  def powerDensityAtHub(cell: GridCell) = Thermodynamics.powerDensity(windSpeedAtHub(cell), hubAltitude(cell))

  def windSpeedAtHub(cell: GridCell): Velocity = {
    Math.log(hubHeight(cell).toMeters / cell.lc.z0.toMeters) / Math.log(h0.toMeters / cell.lc.z0.toMeters) * cell.windSpeed
  }

  /**
   * Onshore we restrict the area to altitude < 2000 m
   * Offshore we restrict the area to maximum depth of 200 m and minimum distance to coast of 10 km
   * We exclude all the areas that are protected
   *
   */
  def urbanFactor(cell: GridCell) = 1.0 - cell.urbanFactor
  def bioReserveFactor(cell: GridCell) = if (cell.protectedArea) 0.0 else 1.0
  def altitudeFactor(cell: GridCell) = {
    if (cell.onshore)
      if (cell.elevation.toMeters <= 2000 /*&& cell.center.latitude.toDegrees >= -60.0*/ ) 1.0 else 0.0
    else if (cell.offshore)
      if (cell.elevation.toMeters >= -200 && cell.distanceToCoast.toKilometers >= 10) 1.0 else 0.0
    else 0.0
  }
  def landUseFactor(cell: GridCell) = {
    if (cell.onshore)
      if (cell.lc.isAgricultural) 0.7
      else if (cell.lc.isForest) 0.1
      else if (cell.lc.isOpenArea || cell.lc.isUrban) 0.9
      else 0.0
    else 1.0
  }
  def geographicFactor(cell: GridCell) = urbanFactor(cell) * bioReserveFactor(cell) * altitudeFactor(cell) * landUseFactor(cell)

  def windRegimeFactor(cell: GridCell) = if (cell.windSpeed.toMetersPerSecond >= 4) 1.0 else 0.0
  def suitabilityFactor(cell: GridCell): Double = {
    geographicFactor(cell) //* windRegimeFactor(cell)
  }

  def loadHours(cell: GridCell) =
    // EU REPORT
    Hours(Math.max(0, Math.min(5500, 626.51 * windSpeedAtHub(cell).value - 1901)))
  // HOOGWIJK REPORT -> 
  //Hours(Math.max(0, Math.min(4000, 565 * windSpeedAtHub(cell).value - 1745)))

  // EU Report
  def availabilityFactor(cell: GridCell): Double = if (cell.offshore /*|| cell.elevation.toMeters >= 600*/ ) 0.9 else 0.97
  def lossFactor(cell: GridCell): Double = 0.9
  def capacityFactor(cell: GridCell): Double = CapacityFactorCalculation(2.0, windSpeedAtHub(cell).toMetersPerSecond)

  def EROI(cell: GridCell): Double = {
    val out = 20 * energy1MW(cell)
    val in =
      if (cell.onshore) SimpleWindFarm.embodiedEnergy(Megawatts(1))
      else SimpleWindFarm.embodiedEnergy(Megawatts(1), cell.distanceToCoast, -cell.elevation)

    out / in
  }
}

object SolarPotential {
  def powerDensity(cell: GridCell) = cell.irradiance
}