package construction

import squants.energy._
import squants.mass._
import squants.space._
import squants.time.Hours
import utils._
import squants.motion.Velocity

/**
 * No matter how we calculate it, we just want to have the total weigth, and embodied energy !
 */
abstract class WindTurbineComponents {
  val components: List[(Mass, Material)]
  val ratedPower: Power
  val hubHeight: Length
  val diameter: Length
  val cutInSpeed: Velocity; val ratedSpeed: Velocity; val cutOutSpeed: Velocity;

  def weight = components.map(_._1).foldLeft(Tonnes(0))(_ + _)
  def embodiedEnergy: Energy = components.map(c => c._2.energyIntensity * c._1).foldLeft(Gigajoules(0))(_ + _)
  def energyIntensity: SpecificEnergy = embodiedEnergy / weight

  override def toString = "Wind turbine : total weight : " + weight + ",total energy embodied : " + embodiedEnergy + ", =>" + energyIntensity.to(GigajoulesPerton) + "GJ/t"
}

object WindTurbineComponents {
  def apply(sheetName: String): WindTurbineComponents = {
    val sheet = Helper.xlsSheet("ressources/windTurbines.xls", sheetName)
    val start = Helper.toInt(sheet.getRow(0), 0) - 1;
    val end = Helper.toInt(sheet.getRow(0), 1) - 1;
    new WindTurbineComponents {
      val components = (start to end).map(r => {
        val row = sheet.getRow(r)
        val mass = Mass(Helper.toDouble(row, 1).toString() + Helper.toString(row, 2)).get
        val intensity = Energy(Helper.toDouble(row, 3).toString + Helper.toString(row, 4)).get
        val perUnit = Mass(Helper.toString(row, 5)).get
        (mass, Material(Helper.toString(row, 0), intensity, perUnit))
      }).toList
      val spec = sheet.getRow(2)
      val ratedPower = Power(Helper.toString(spec, 0)).get
      val hubHeight = Length(Helper.toString(spec, 1)).get
      val diameter = Length(Helper.toString(spec, 2)).get
      val cutInSpeed = Velocity(Helper.toString(spec, 3)).get
      val ratedSpeed = Velocity(Helper.toString(spec, 4)).get
      val cutOutSpeed = Velocity(Helper.toString(spec, 5)).get
    }
  }
}

/**
 * 3MW : 1443.302 tons
 * 2MW : 962.844 tons
 * 850 kW : 597 tons
 *
 * From "Energy and CO2 life-cycle analyses of wind turbines- review and applications"
 */
class DefaultWindTurbineComponents(val totWeight: Mass, val ratedPower: Power, val diameter: Length, val hubHeight: Length,
    val cutInSpeed: Velocity, val ratedSpeed: Velocity, val cutOutSpeed: Velocity, val min: Boolean = false, val max: Boolean = false) extends WindTurbineComponents {

  val blades = (totWeight * 0.027, Material("Glass fibre, epoxy,PVC", Megajoules(61.8 - (if (min) 35.7 else 0) + (if (max) 35.7 else 0)), Kilograms(1)))
  val hub = (totWeight * 0.035, Material("Steel", Megajoules(36.8 - (if (min) 18.5 else 0) + (if (max) 18.5 else 0)), Kilograms(1)))
  val transmission = (totWeight * 0.052, Material("Steel", Megajoules(36.8 - (if (min) 18.5 else 0) + (if (max) 18.5 else 0)), Kilograms(1)))
  val generator = (totWeight * 0.026, Material("Copper", Megajoules(86.2 - (if (min) 65.5 else 0) + (if (max) 65.5 else 0)), Kilograms(1)))
  val nacelle = (totWeight * 0.003, Material("Glass fibre", Megajoules(61.8 - (if (min) 35.7 else 0) + (if (max) 35.7 else 0)), Kilograms(1)))
  val tower = (totWeight * 0.233, Material("Steel", Megajoules(36.8 - (if (min) 18.5 else 0) + (if (max) 18.5 else 0)), Kilograms(1)))
  val foundations = (totWeight * 0.603, Material("Concrete", Megajoules(3.2 - (if (min) 1.9 else 0) + (if (max) 1.9 else 0)), Kilograms(1)))
  val electrical = (totWeight * 0.021, Material("Copper", Megajoules(86.2 - (if (min) 65.5 else 0) + (if (max) 65.5 else 0)), Kilograms(1)))

  val components = List(blades, hub, transmission, generator, nacelle, tower, foundations, electrical)

}