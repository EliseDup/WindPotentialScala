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
import squants.motion.MetersPerSecond
import squants.space.Meters

trait WindTechnology extends RenewableTechnology {
  val wind = true; val solar = false; val csp = false; val pv = false;
  val top_down: Boolean = true; val cp_max: Double = 0.5;
  val lifeTime = 25
  val occupationRatio = 1.0

  // Energy Inputs specific to wind power
  val installation_variable: Energy; val OM_variable: Energy; val decommissioning_variable: Energy;
  def construction_depth(depth: Length): Energy;

  override def energyInputsInstallation(cell: Cell, eroi_min: Double): Energy =
    super.energyInputsInstallation(cell, eroi_min) + ee.ratioPower(ratedPower(cell, eroi_min)) * (Math.abs(cell.distanceToCoast.toKilometers) * installation_variable + construction_depth(cell.waterDepth))
  override def energyInputsOMYearly(cell: Cell, eroi_min: Double): Energy =
    super.energyInputsOMYearly(cell, eroi_min) + ee.ratioPower(ratedPower(cell, eroi_min)) * Math.abs(cell.distanceToCoast.toKilometers) * OM_variable
  override def energyInputsDecomissioning(cell: Cell, eroi_min: Double) = super.energyInputsDecomissioning(cell, eroi_min) + ee.ratioPower(ratedPower(cell, eroi_min)) * Math.abs(cell.distanceToCoast.toKilometers) * decommissioning_variable

  def availabilityFactor(cell: Cell): Double = if (cell.offshore) 0.95 else 0.97

  def potential(cell: Cell, eroi_min: Double): Power = power(cell, cell.optimalRatedSpeed(eroi_min), cell.optimalN(eroi_min)) * (1 - operation_variable)
  def potential(cell: Cell, vr: Velocity, n: Double): Power = power(cell, vr, n) * (1 - operation_variable)

  def ratedPower(cell: Cell, eroi_min: Double): Power = ratedPower(cell, cell.optimalRatedSpeed(eroi_min), cell.optimalN(eroi_min))

  private def power(cell: Cell, vr: Velocity, n: Double): Power = {
    val wi = ratedPower(cell, vr, n)
    val res = wi * CapacityFactorCalculation.cubic(cell.wind100m, vr.toMetersPerSecond) * WakeEffect.arrayEfficiency(wi.toMegawatts / 3.0, Math.PI / (4 * Math.pow(n, 2))) * availabilityFactor(cell)
    if (top_down && res / (suitabilityFactor(cell) * cell.area) > cell.keDissipation) cell.keDissipation * cell.area * suitabilityFactor(cell)
    else res
  }

  def ratedPower(cell: Cell, vr: Velocity, n: Double): Power = {
    val res = cell.area * suitabilityFactor(cell) * capacityDensity(vr, n, cell.hubAltitude)
    val efficiency = CapacityFactorCalculation.cubic(cell.wind100m, vr.toMetersPerSecond) * WakeEffect.arrayEfficiency(res.toMegawatts / 3.0, Math.PI / (4 * Math.pow(n, 2))) * availabilityFactor(cell)
    if (top_down && res * efficiency / (suitabilityFactor(cell) * cell.area) > cell.keDissipation) {
      cell.keDissipation * cell.area * suitabilityFactor(cell) / efficiency
    } else res
  }
  def capacityDensity(ratedSpeed: Velocity, n: Double, hubAltitude: Length): Irradiance = WattsPerSquareMeter(coeff(hubAltitude) * Math.pow(ratedSpeed.toMetersPerSecond, 3) / (n * n))

  // Relationship between rated power, rotor diameter and rated wind speed
  // Power_rated = 1/2 * Cp_max * rho * PI / 4 * D^2 * v_rated^3
  // => v_rated = (Power_rated / (1/2 * Cp_max * rho * PI / 4 * D^2) )^(1/3)
  // => D = (Power_rated / (1/2 * Cp_max * rho * PI / 4 * v^3) )^(1/2)
  def coeff(elevation: Length) = 0.5 * cp_max * WindPower.airDensity(elevation).toKilogramsPerCubicMeter * Math.PI / 4
  val defaultVr = MetersPerSecond(11)

  def ratedPower(rotorDiameter: Length, hubAltitude: Length, ratedSpeed: Velocity = defaultVr) = Watts(coeff(hubAltitude) * Math.pow(rotorDiameter.toMeters, 2) * Math.pow(ratedSpeed.toMetersPerSecond, 3))
  def rotorDiameter(ratedPower: Power, hubAltitude: Length, ratedSpeed: Velocity = defaultVr) = Meters(Math.sqrt(ratedPower.toWatts / (coeff(hubAltitude) * Math.pow(ratedSpeed.toMetersPerSecond, 3))))
  def ratedSpeed(ratedPower: Power, rotorDiameter: Length, hubAltitude: Length) = MetersPerSecond(Math.pow(ratedPower.toWatts / (coeff(hubAltitude) * Math.pow(rotorDiameter.toMeters, 2)), 1.0 / 3))

}

object OnshoreWindTechnology extends WindTechnology {
  val name = "Wind-onshore"
  override def suitabilityFactor(cell: Cell): Double = {
    super.suitabilityFactor(cell) * (if (cell.onshore && cell.EEZ) cell.landCovers.suitabilityFactorWind else 0.0)
  }

  def construction_depth(depth: Length) = Joules(0)
  val installation_variable = Gigajoules(605.74)
  val OM_variable = Gigajoules(21.3)
  val decommissioning_variable = Joules(0)
  val ee = new EmbodiedEnergy(Gigawatts(1), Gigajoules(4377757 + 366858), Gigajoules(7869000 + 68760), Gigajoules(153422), Gigajoules(6450 + 348921),
    Gigajoules(38285 + 473322), Gigajoules(41400) / 25, 0.035, 25)
}

object OffshoreWindTechnology extends WindTechnology {
  val name = "Wind-offshore"
  override def suitabilityFactor(cell: Cell): Double = {
    super.suitabilityFactor(cell) * (
      if (!cell.offshoreEEZ || cell.waterDepth.toMeters > 1000) 0.0
      else {
        val d = cell.distanceToCoast.toNauticalMiles
        if (d < 5) 0.1
        else if (d < 20) 0.33
        else 0.67
      })
  }

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

  def construction_depth(depth: Length): Energy = {
    if (depth.toMeters > 40) Joules(0)
    else offshoreFixedFoundations(depth)
  }
  // 0.7 % of electricity directly consumed
  // val operation_variable = 0.007
  val installation_variable = Gigajoules(16904) + Gigajoules(4681 + 105)
  val OM_variable = Gigajoules(6615)
  val decommissioning_variable = Gigajoules(12674)
  
  val ee = new EmbodiedEnergy(Gigawatts(1), Gigajoules(3442580 + 241686), Gigajoules(8523000 + 82662), Gigajoules(1779159), Gigajoules(1283000 + 192719),
    Gigajoules(64150 + 938343), Gigajoules(1639675) / 25, 0.007, 25)
}