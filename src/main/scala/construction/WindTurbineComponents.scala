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
  def apply(comp: (Mass, Material)*) : Components = apply(comp.toList)
  def apply(list : List[(Mass, Material)]) : Components = new Components{
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
  
  
}

class WindTurbineComponents(val sheetName: String) extends AbstractWindTurbineComponents {

  val sheet = Helper.xlsSheet("ressources/windTurbines.xls", sheetName)

  val componentsWithName: List[(String, Mass, Material)] = {
    val start = Helper.toInt(sheet.getRow(0), 0) - 1;
    val end = Helper.toInt(sheet.getRow(0), 1) - 1;
    (start to end).map(r => {
      val row = sheet.getRow(r)
      val mass = Mass(Helper.toDouble(row, 1).toString() + Helper.toString(row, 2)).get
      (Helper.toString(row, 6), mass, Materials.getOrNone(Helper.toString(row, 0)))
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
 * 3MW : 1443.302 tons
 * 2MW : 962.844 tons
 * 850 kW : 597 tons
 *
 * From "Energy and CO2 life-cycle analyses of wind turbines- review and applications" ==> does not seem correct !!
 */
/*class DefaultWindTurbineComponents(val totWeight: Mass, val ratedPower: Power, val diameter: Length, val hubHeight: Length,
    val cutInSpeed: Velocity, val ratedSpeed: Velocity, val cutOutSpeed: Velocity) extends AbstractWindTurbineComponents {

  val blades = (totWeight * 0.027, Fiberglass)
  val hub = (totWeight * 0.035, Steel)
  val transmission = (totWeight * 0.052, Steel)
  val generator = (totWeight * 0.026, Copper)
  val nacelleComp = (totWeight * 0.003, Fiberglass)
  val towerComp = (totWeight * 0.233, Steel)
  val foundationComp = (totWeight * 0.603, Concrete)
  val electrical = (totWeight * 0.021, Copper)

  val components = List(blades, hub, transmission, generator, nacelleComp, towerComp, foundationComp, electrical)
  val rotor = Components(blades, hub)
  val nacelle = Components(transmission, generator, nacelleComp, electrical)
  val tower = Components(towerComp)
  val foundation = Components(foundationComp)
}

object DefaultWindTurbineComponents {
  def apply(sheetName: String) = {
    val sheet = Helper.xlsSheet("ressources/windTurbines.xls", sheetName)
    val spec = sheet.getRow(2)
    new DefaultWindTurbineComponents(Mass(Helper.toString(spec, 6)).get, Power(Helper.toString(spec, 0)).get, Length(Helper.toString(spec, 2)).get,
      Length(Helper.toString(spec, 1)).get, Velocity(Helper.toString(spec, 3)).get, Velocity(Helper.toString(spec, 4)).get, Velocity(Helper.toString(spec, 5)).get)
  }

}*/