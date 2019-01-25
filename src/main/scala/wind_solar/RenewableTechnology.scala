package wind_solar

import squants.energy._
import squants.time.Hours
import squants.radio.WattsPerSquareMeter
import squants.space._
import squants.mass.Mass

trait RenewableTechnology {
  val name: String;
  val ee: EmbodiedEnergy;

  def potential(cell: Cell, eroi_min: Double): Power;
  def suitabilityFactor(cell: Cell): Double;
  def ratedPower(cell: Cell, eroi_min: Double): Power

  def capacityFactor(cell: Cell, eroi_min: Double) = potential(cell, eroi_min: Double) / ratedPower(cell, eroi_min)
  def embodiedEnergy(cell: Cell, eroi_min: Double): Energy = ee.embodiedEnergy(ratedPower(cell, eroi_min), potential(cell, eroi_min) * Hours(365 * 24))
  def netYearlyProduction(cell: Cell, eroi_min: Double): Energy = potential(cell, eroi_min) * Hours(365 * 24) * ee.lifeTime - embodiedEnergy(cell, eroi_min)
  
  def eroi(cell: Cell, eroi_min: Double): Double = {
    val wi = ratedPower(cell, eroi_min)
    if (wi.value == 0) 0.0
    else {
      val out = potential(cell, eroi_min) * Hours(365 * 24) * ee.lifeTime
      out / embodiedEnergy(cell: Cell, eroi_min: Double)
    }
  }
}

class EmbodiedEnergy(val raw_materials: Energy,
    val construction_decomissioning: Energy, val transport_materials: Energy, val O_M_fixed: Energy,
    val O_M_output: Energy, val lifeTime: Int, val construction_variable: Energy = Joules(0), val transport_variable: Energy = Joules(0), val default_area: Area = SquareMeters(1)) {
  
  def truckTransport(weight: Mass, distance: Length) = Megajoules(1.25) * weight.toTonnes * Math.abs(distance.toKilometers)
  def shipTransport(weight: Mass, distance: Length) = Megajoules(0.29) * weight.toTonnes * Math.abs(distance.toKilometers)

  def eroi(cf: Double) = (lifeTime * Gigawatts(1) * cf * Hours(24 * 365)) / embodiedEnergy1GW(cf)
  def eroi(cf: Double, area: Area) = {
    val output_year = Gigawatts(1) * cf * Hours(24 * 365)
    output_year * lifeTime / embodiedEnergyArea(Gigawatts(1), output_year, area)
  }

  def embodiedEnergy1GW(output_1GW_year: Energy) =
    raw_materials + construction_decomissioning + transport_materials +
      lifeTime * (O_M_fixed + output_1GW_year.toGigajoules * O_M_output)

  def embodiedEnergy1GW(cf: Double): Energy = embodiedEnergy1GW(Gigawatts(1) * cf * Hours(24 * 365))

  def embodiedEnergy(rated_power: Power, output_year: Energy): Energy = {
    val ratio = rated_power.toGigawatts
    ratio * embodiedEnergy1GW(output_year / ratio)
  }

  def embodiedEnergy(rated_power: Power, capacity_factor: Double) = {
    rated_power.toGigawatts * embodiedEnergy1GW(capacity_factor)
  }
  // For CSP, the embodied energy was calculated for a default aperture area !
  def embodiedEnergyArea(rated_power: Power, output_year: Energy, area: Area): Energy = {
    val area_ratio = area / default_area
    embodiedEnergy(rated_power, output_year) + area_ratio * (transport_variable + construction_variable)
  }
  // For csp optimzation
  val fixed1MW = Megawatts(1).toGigawatts * (raw_materials + construction_decomissioning + transport_materials + O_M_fixed * lifeTime)
  val variable1MW = {
    val area_ratio = Megawatts(1) / (WattsPerSquareMeter(950) * 0.22) / default_area
    area_ratio * (transport_variable + construction_variable)
  }
}