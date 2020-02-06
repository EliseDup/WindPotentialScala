package wind_solar

import squants.energy._
import squants.time.Hours
import squants.radio.WattsPerSquareMeter
import squants.space._
import squants.mass.Mass

trait RenewableTechnology {
  val name: String;
  val lifeTime: Int;
  val wind: Boolean; val solar: Boolean; val csp: Boolean; val pv: Boolean;
   
  val ee : EmbodiedEnergy
  
  val occupationRatio: Double

  def fixed_energy_inputs_1GW(cell: Cell): Energy

  // Operational electricity should be directly removed from potential as it never gets out of the facility ..
  def potential(cell: Cell, eroi_min: Double): Power;

  val excludedCountries = List("NA", "Antarctica", "Greenland", "French Southern & Antarctic Lands")
  def suitabilityFactor(cell: Cell): Double = {
    if (excludedCountries.contains(cell.country) || cell.country.contains("Is.") || cell.country.contains("Islands")) 0.0
    else (1 - cell.protectedArea)
  }
  def ratedPower(cell: Cell, eroi_min: Double): Power;
  def embodiedEnergy(cell: Cell, eroi_min: Double): Energy;

  def capacityFactor(cell: Cell, eroi_min: Double) = potential(cell, eroi_min: Double) / ratedPower(cell, eroi_min)
  def netYearlyProduction(cell: Cell, eroi_min: Double): Energy =
    if (eroi(cell, eroi_min) >= eroi_min) potential(cell, eroi_min) * Hours(365 * 24) - embodiedEnergy(cell, eroi_min) / lifeTime
    else Joules(0)

  // In EROI calculation we take into account operation variable in the numerator and denominator !
  def eroi(cell: Cell, eroi_min: Double): Double = {
    val wi = ratedPower(cell, eroi_min)
    if (wi.value == 0) 0.0
    else {
      val out_year = potential(cell, eroi_min) * Hours(365 * 24)
      out_year / (1 - ee.om_output) * lifeTime / (embodiedEnergy(cell, eroi_min) + out_year / (1 - ee.om_output) * ee.om_output * lifeTime)
    }
  }
  
  def energyInputsInstallation(cell: Cell, eroi_min:Double): Energy
  def OMYearlyEnergyInputs(cell: Cell, eroi_min:Double): Energy
  def operation_variable: Double = ee.om_output
 
}

class EmbodiedEnergy(val power : Power,
    val raw_materials: Energy, val manufacturing: Energy,
    val installation: Energy, val decommissioning:Energy,
    val transport: Energy, val om_fixed: Energy,
    val om_output: Double, val lifeTime: Int, 
    val construction_variable: Energy = Joules(0), val transport_variable: Energy = Joules(0), val om_variable: Energy = Joules(0),
    val default_area: Area = SquareMeters(1)) {

  //def truckTransport(weight: Mass, distance: Length) = Megajoules(1.25) * weight.toTonnes * Math.abs(distance.toKilometers)
  //def shipTransport(weight: Mass, distance: Length) = Megajoules(0.29) * weight.toTonnes * Math.abs(distance.toKilometers)

  private def embodiedEnergy(rated_power: Power): Energy = {
    val ratio = rated_power/power
    (raw_materials +  manufacturing + installation + decommissioning + transport + lifeTime * om_fixed) * ratio
  }
  // For CSP, the embodied energy was calculated for a default aperture area ! 
  def embodiedEnergyArea(rated_power: Power, area: Area): Energy = {
    val area_ratio = area / default_area
    embodiedEnergy(rated_power) + area_ratio * (transport_variable + construction_variable)
  }
  // For Wind energy, transport varies with distance to shore
  def embodiedEnergyDistance(rated_power: Power, distanceToShore : Length): Energy = {
    distanceToShore.toKilometers*(construction_variable + om_variable)
  }
  
  def installationEnergyInputs(rated_power: Power, area: Area): Energy = {
    val area_ratio = area / default_area
    val ratio = rated_power/power
    (raw_materials + manufacturing + installation + transport) * ratio + area_ratio * (transport_variable + construction_variable)
  }
  def omYearlyEnergyInputs(rated_power: Power) : Energy = rated_power/power*om_fixed
}
