package wind_solar

import solar_energy.EmbodiedEnergy
import squants.energy._
import squants.time.Hours

trait RenewableTechnology {
  val name: String;
  val ee: EmbodiedEnergy; 
  
  def potential(cell : Cell): Power;
  def suitabilityFactor(cell : Cell): Double;
  def eroi(cell : Cell): Double;
  def ratedPower(cell : Cell): Power
  def capacityFactor(cell : Cell) = potential(cell)/ratedPower(cell)
  def embodiedEnergy(cell : Cell): Energy = ee.embodiedEnergy(ratedPower(cell), potential(cell)*Hours(365*24))
  def netYearlyProduction(cell : Cell): Energy = potential(cell)*Hours(365*24) - embodiedEnergy(cell)
  
  
}

