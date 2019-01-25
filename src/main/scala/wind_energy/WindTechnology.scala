package wind_energy

import wind_solar.RenewableTechnology
import wind_solar.EmbodiedEnergy
import squants.energy._
import squants.motion.Velocity
import wind_solar.Cell
import squants.space.Length
import squants.radio._
import wind_solar.EmbodiedEnergy
import squants.time.Hours

trait WindTechnology extends RenewableTechnology {

  val top_down: Boolean = false; val cp_max: Double = 0.5;
  val lifeTime = 25
  // Energy Inputs
  val operation_variable: Energy; val installation_variable: Energy; val OM_variable: Energy;
  def constructionInputs(depth: Length): Energy;
  def operation(output: Energy): Energy = output.toGigajoules * operation_variable
  def installation(distanceToCoast: Length): Energy = Math.abs(distanceToCoast.toKilometers) * installation_variable
  def OM(distanceToCoast: Length): Energy = Math.abs(distanceToCoast.toKilometers) * OM_variable

  def embodiedEnergy(cell: Cell, eroi_min: Double): Energy = {
    (ratedPower(cell, eroi_min) / Gigawatts(1)) *
      (constructionInputs(cell.waterDepth) + installation(cell.distanceToCoast) + OM(cell.distanceToCoast)) +
      operation(potential(cell, eroi_min) * Hours(365 * 24) * lifeTime)
  }

  def availabilityFactor(cell: Cell): Double = if (cell.offshore) 0.95 else 0.97

  def potential(cell: Cell, eroi_min: Double): Power = power(cell, cell.optimalRatedSpeed(eroi_min), cell.optimalN(eroi_min))
  def ratedPower(cell: Cell, eroi_min: Double): Power = ratedPower(cell, cell.optimalRatedSpeed(eroi_min), cell.optimalN(eroi_min))

  def power(cell: Cell, vr: Velocity, n: Double): Power = {
    val wi = ratedPower(cell, vr, n)
    val res = wi * CapacityFactorCalculation.cubic(cell.wind100m, vr.toMetersPerSecond) * WakeEffect.arrayEfficiency(wi.toMegawatts / 3.0, Math.PI / (4 * Math.pow(n, 2))) * availabilityFactor(cell)
    if (top_down && res / (suitabilityFactor(cell) * cell.area) > cell.keDissipation) cell.keDissipation * cell.area * suitabilityFactor(cell)
    else res
  }
  def ratedPower(cell: Cell, vr: Velocity, n: Double): Power = cell.area * suitabilityFactor(cell) * capacityDensity(vr, n, cell.hubAltitude)
  def capacityDensity(ratedSpeed: Velocity, n: Double, hubAltitude: Length): Irradiance = WattsPerSquareMeter(coeff(hubAltitude) * Math.pow(ratedSpeed.toMetersPerSecond, 3) / (n * n))

  // Relationship between rated power, rotor diameter and rated wind speed
  // Power_rated = 1/2 * Cp_max * rho * PI / 4 * D^2 * v_rated^3
  // => v_rated = (Power_rated / (1/2 * Cp_max * rho * PI / 4 * D^2) )^(1/3)
  // => D = (Power_rated / (1/2 * Cp_max * rho * PI / 4 * v^3) )^(1/2)
  def coeff(elevation: Length) = 0.5 * cp_max * WindPower.airDensity(elevation).toKilogramsPerCubicMeter * Math.PI / 4
}

object OnshoreWindTechnology extends WindTechnology {
  val name = "Onshore WT"
  override def suitabilityFactor(cell: Cell): Double ={
    super.suitabilityFactor(cell)* (if (cell.onshore && cell.EEZ) cell.landCovers.suitabilityFactorWind else 0.0)
  }

  def constructionInputs(depth: Length) = Gigajoules(13744075)
  val operation_variable = Gigajoules(0.035)
  val installation_variable = Gigajoules(605.74)
  val OM_variable = Gigajoules(21.3)

}

object OffshoreWindTechnology extends WindTechnology {
  val name = "Offshore WT"
  override def suitabilityFactor(cell: Cell): Double = {super.suitabilityFactor(cell) *(
    if (!cell.offshoreEEZ || cell.waterDepth.toMeters > 1000) 0.0
    else {
      val d = cell.distanceToCoast.toNauticalMiles
      if (d < 5) 0.1
      else if (d < 20) 0.33
      else 0.67
    })
  }
  val fixedOffshoreFixed = Gigajoules(18185974)
  val fixedOffshoreFloating = Gigajoules(26670974)
  def offshoreFixedFoundations(depth: Length) = scalingFactorFixedFoundations(depth) * (Gigajoules(16173 + 361962 + 10326 + 3477293))
  // For water depth up to 40 m
  def scalingFactorFixedFoundations(depth: Length) = {
    val d = depth.toMeters
    if (d <= 15) 1.0
    else if (d <= 20) 1.08
    else if (d <= 25) 1.34
    else if (d <= 30) 1.57
    else if (d <= 35) 1.95
    else 2.19
  }
  override def constructionInputs(depth: Length): Energy = {
    if (depth.toMeters > 40) fixedOffshoreFloating
    else fixedOffshoreFixed + offshoreFixedFoundations(depth)
  }
  val operation_variable = Gigajoules(0.007)
  val installation_variable = Gigajoules(16904) + Gigajoules(4681 + 105)
  val OM_variable = Gigajoules(6615)

}