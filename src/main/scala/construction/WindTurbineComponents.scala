package construction

import squants.energy._
import squants.mass._
import squants.space._
import squants.time.Hours
import utils._
import squants.motion.Velocity
import org.apache.poi.ss.usermodel.Sheet
import org.apache.poi.ss.usermodel.Row

/**
 * A component is just a list of materials and mass
 * So we can evaluate the embodied energy associated
 */

abstract class Components {
  val components: List[(Mass, Material)]

  def weight = components.map(_._1).foldLeft(Tonnes(0))(_ + _)
  def constructionEnergy: Energy = components.map(c => c._2.energyIntensity * c._1).foldLeft(Gigajoules(0))(_ + _)
  def transportEnergy: Energy = components.map(c => c._2.transportEnergy(c._1)).foldLeft(Gigajoules(0))(_ + _)

  def embodiedEnergy = constructionEnergy + transportEnergy
  def energyIntensity: SpecificEnergy = embodiedEnergy / weight

  override def toString = "Components : total weight : " + weight + ",total energy embodied : " + embodiedEnergy + ", =>" + energyIntensity.to(GigajoulesPerton) + "GJ/t"

  def *(d: Double): Components = Components(components.map(i => (i._1 * d, i._2)))

}

object Components {
  def apply(comp: (Mass, Material)*): Components = apply(comp.toList)
  def apply(list: List[(Mass, Material)]): Components = new Components {
    val components = list
  }
}
/**
 * The list of components of a wind turbines, mainly : Tower, Foundation, Rotor and Nacelle
 * Also some specifications
 */

abstract class AbstractWindTurbineComponents extends Components {
  val ratedPower: Power
  val hubHeight, diameter: Length
  val cutInSpeed, ratedSpeed, cutOutSpeed: Velocity
  val foundation, tower, nacelle, rotor: Components

  override def transportEnergy = super.transportEnergy +
    Transport.truckTransport(tower.weight, Kilometers(1100)) + Transport.shipTransport(tower.weight, Kilometers(8050)) +
    Transport.truckTransport(nacelle.weight, Kilometers(1025)) +
    Transport.truckTransport(rotor.weight, Kilometers(600)) +
    Transport.truckTransport(foundation.weight, Kilometers(50))
   
   override def embodiedEnergy  = Gigajoules(10000*ratedPower.toMegawatts)
   
   override def toString = "Wind Turbine "+ ratedPower + ", hub height : "+ hubHeight +", rotor diameter : " +diameter
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
  def findComponents(name: String) = new Components {
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

  val hub = Components((Kilograms(38791.7 / 5), CastIron), (Kilograms(2041.7 / 5), Aluminium)) * nMW
  val blades = Components((Kilograms(2056.3 / 5), Steel), (Kilograms(39068.8 / 5), Fiberglass)) * nMW
  val nacelle = Components((Kilograms(26250.0 / 5), CastIron),
    (Kilograms((11400 + 8250 + 24575 + 63700 + 600) / 5.0), Steel),
    (Kilograms((650 + 6255) / 5.0), Aluminium),
    (Kilograms((650 + 7000 + 3750 + 420) / 5.0), Copper)) * nMW
  val tower = Components((Kilograms(2.8 * 2400 / 5), Concrete), (Kilograms(339406.7 / 5), Steel)) * nMW

  val foundations = Components((Kilograms(560000/5.0),Steel),(Kilograms(1300*2400/5),Concrete))*nMW // TODO , Kilograms(5151000/5,Gravel))

  val replacementsParts = Components((Kilograms(8138 / 5.0), CastIron),
    (Kilograms((186 + 157976 + 7618 + 2558) / 5.0), Steel),
    (Kilograms((1612 + 1939) / 5.0), Aluminium),
    (Kilograms((2170 + 1612 + 1301163) / 5.0), Copper),
    (Kilograms((155 + 116) / 5.0), Silica)) * nMW
}