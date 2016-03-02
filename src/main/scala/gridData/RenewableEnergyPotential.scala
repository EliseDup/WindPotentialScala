package gridData

import squants.radio._
import squants.energy._
import squants.time._
import squants.motion._
import squants.space._
import utils.Thermodynamics
import windEnergy.WindFarm
import windEnergy.OffshoreWindFarm

trait RenewablePotential {
  
  val density: Irradiance

  def suitabilityFactor(cell: GridCell): Double
  def energyGeneratedPerYear(cell: GridCell): Energy
  def EROI(cell: GridCell): Double

}

object WindPotential extends RenewablePotential {
  val density = Megawatts(2) / SquareKilometers(1)
  
  // Measurement of wind speed is taken at 10 metres height
  val h0 = Meters(10)
  def hubHeight(cell: GridCell): Length = if (cell.onshore) Meters(80) else Meters(90)
  def hubAltitude(cell: GridCell) = Meters(Math.max(0.0, cell.elevation.toMeters) + hubHeight(cell).toMeters)
  def powerDensity(cell: GridCell) = Thermodynamics.powerDensity(cell.windSpeed, hubAltitude(cell))
  def powerDensityAtHub(cell: GridCell) = Thermodynamics.powerDensity(windSpeedAtHub(cell), hubAltitude(cell))

  def windSpeedAtHub(cell: GridCell): Velocity = {
    Math.log(hubHeight(cell).toMeters / cell.lc.z0.toMeters) / Math.log(h0.toMeters / cell.lc.z0.toMeters) * cell.windSpeed
  }

  def loadHours(cell: GridCell) =
    // EU REPORT
    Hours(Math.max(0, Math.min(5500, 626.51 * windSpeedAtHub(cell).value - 1901)))
  // HOOGWIJK REPORT -> Hours(Math.max(0, Math.min(4000, 565 * windSpeedAtHub().value - 1745)))

  def suitabilityFactor(cell: GridCell): Double = {
    val geo =
      if (cell.onshore) cell.elevation.toMeters <= 2000
      else cell.elevation.toMeters >= -200 && cell.distanceToCoast.toKilometers >= 10
    if (geo & cell.windSpeed.toMetersPerSecond >= 4) 1.0
    else 0.0
  }

  def powerInstalled(cell: GridCell): Power = suitabilityFactor(cell) * density * cell.effectiveArea
  def energyGeneratedPerYear(cell: GridCell): Energy = powerInstalled(cell) * loadHours(cell) * 0.9 //TODO !!

  def EROI(cell: GridCell) = {
    if (powerInstalled(cell).value == 0) 0.0
    else {
      val out = 20 * energyGeneratedPerYear(cell)
      val in =
        if (cell.onshore) new WindFarm(powerInstalled(cell)).embodiedEnergy
        else new OffshoreWindFarm(powerInstalled(cell), cell.distanceToCoast, -cell.elevation).embodiedEnergy
      out / in

    }
  }
}


// class SolarPotential(cell : GridCell) 