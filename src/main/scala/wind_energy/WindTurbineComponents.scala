package wind_energy

import squants.energy._
import squants.mass._
import squants.space._
import squants.motion._
import utils._
import construction._
import construction.Transport
import squants.energy.Power.apply
import squants.mass.Mass.apply
import squants.motion.Velocity.apply
import squants.space.Length.apply

/**
 * The list of components of a wind turbines, mainly : Tower, Foundation, Rotor and Nacelle
 * Also some specifications
 */

abstract class AbstractWindTurbineComponents extends Product {
  val ratedPower: Power
  val hubHeight, diameter: Length
  val cutInSpeed, ratedSpeed, cutOutSpeed: Velocity
  val foundation, tower, nacelle, rotor: Product
  
  // TODO IIER : 20.460 GJ (Electricity) = 5.683.333 kWh / 2,6 MW 
  override def energyInput = KilowattHours(3283000)
  
  // According to VESTAS - lca of 1,65 MW wind turbine
  // IIER : 9.883 GJ (Diesel)
  override def transportEnergyInput =
    Transport.truckTransport(tower.weight, Kilometers(700)) +
      Transport.truckTransport(nacelle.weight, Kilometers(1000)) + Transport.shipTransport(nacelle.weight, Kilometers(15566))+
      Transport.truckTransport(rotor.weight, Kilometers(1000)) + Transport.shipTransport(rotor.weight, Kilometers(15566))+
      Transport.truckTransport(foundation.weight, Kilometers(200))

  override def toString = "Wind Turbine " + ratedPower + ", hub height : " + hubHeight + ", rotor diameter : " + diameter
}

object OffshoreWindTurbineComponents extends AbstractWindTurbineComponents {
  val ratedPower = Megawatts(5); val hubHeight = Meters(90); val diameter = Meters(126)
  val cutInSpeed = MetersPerSecond(3.5); val ratedSpeed = MetersPerSecond(11.4); val cutOutSpeed = MetersPerSecond(30);
  val foundation = Product()
  val rotor = Product((Tonnes(60), Steel), (Tonnes(50), GlassReinforcedPlastic))
  val nacelle = Product((Tonnes(197), Steel), (Tonnes(8), Aluminium), (Tonnes(32), Copper), (Tonnes(2), GlassReinforcedPlastic))
  val tower = Product((Tonnes(330), Steel), (Tonnes(5), Aluminium), (Tonnes(4), Electronics), (Tonnes(4), Polyethylene), (Tonnes(2), Copper), (Tonnes(2), LubricatingOil))

  val components = foundation.components ++ tower.components ++ nacelle.components
}

class WindTurbineComponents(val sheetName: String) extends AbstractWindTurbineComponents {

  val sheet = Helper.xlsSheet("ressources/windTurbines.xls", sheetName)

  val componentsWithName: List[(String, Mass, Material)] = {
    val start = Helper.toInt(sheet.getRow(0), 0) - 1;
    val end = Helper.toInt(sheet.getRow(0), 1) - 1;
    (start to end).map(r => {
      val row = sheet.getRow(r)
      val mass = Mass(Helper.toDouble(row, 1).toString() + Helper.toString(row, 2)).get
      (Helper.toString(row, 3), mass, Materials.getOrNone(Helper.toString(row, 0)))
    }).toList
  }
  
  val components = componentsWithName.map(i => (i._2, i._3))
  val rotor = findComponents("Rotor"); val nacelle = findComponents("Nacelle");
  val tower = findComponents("Tower"); val foundation = findComponents("Foundation");
  def findComponents(name: String) = new Product {
    val components = componentsWithName.filter(_._1.equals(name)).map(i => (i._2, i._3))
  }
       
  val spec = sheet.getRow(2)
  val ratedPower = Power(Helper.toString(spec, 0)).get
  val hubHeight = Length(Helper.toString(spec, 1)).get
  val diameter = Length(Helper.toString(spec, 2)).get
  val cutInSpeed = Velocity(Helper.toString(spec, 3)).get
  val ratedSpeed = Velocity(Helper.toString(spec, 4)).get
  val cutOutSpeed = Velocity(Helper.toString(spec, 5)).get

}

/**
 * From : Assessing the Life Cycle Environmental Impacts of Offshore
 * Wind Power Generation and Power Transmission in the North Sea - Christine Birkeland
 */

class DefaultWindTurbineComponents(val ratedPower: Power) {

  val nMW = ratedPower.toMegawatts

  val hub = Product((Kilograms(38791.7 / 5), CastIron), (Kilograms(2041.7 / 5), Aluminium)) * nMW
  val blades = Product((Kilograms(2056.3 / 5), Steel), (Kilograms(39068.8 / 5), Fiberglass)) * nMW
  val nacelle = Product((Kilograms(26250.0 / 5), CastIron),
    (Kilograms((11400 + 8250 + 24575 + 63700 + 600) / 5.0), Steel),
    (Kilograms((650 + 6255) / 5.0), Aluminium),
    (Kilograms((650 + 7000 + 3750 + 420) / 5.0), Copper)) * nMW
  val tower = Product((Kilograms(2.8 * 2400 / 5), Concrete), (Kilograms(339406.7 / 5), Steel)) * nMW

  val foundations = Product((Kilograms(560000 / 5.0), Steel), (Kilograms(1300 * 2400 / 5), Concrete)) * nMW // TODO , Kilograms(5151000/5,Gravel))

  val replacementsParts = Product((Kilograms(8138 / 5.0), CastIron),
    (Kilograms((186 + 157976 + 7618 + 2558) / 5.0), Steel),
    (Kilograms((1612 + 1939) / 5.0), Aluminium),
    (Kilograms((2170 + 1612 + 1301163) / 5.0), Copper),
    (Kilograms((155 + 116) / 5.0), Silica)) * nMW
}