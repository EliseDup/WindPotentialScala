package construction

import squants.energy._
import squants.mass._
import squants.time.Hours
import utils._
/**
 * No matter how we calculate it, we just want to have the total weigth, and embodied energy !
 */
abstract class AbstractWindTurbineComponents {
  val weight : Mass
  val embodiedEnergy : Energy
  def energyIntensity : SpecificEnergy = embodiedEnergy / weight
  
  override def toString = "Wind turbine : total weight : " + weight + ",total energy embodied : " + embodiedEnergy + ", =>" + energyIntensity.to(GigajoulesPerton) + "GJ/t"

}
/**
 * 3MW : 1443.302 tons
 * 2MW : 962.844 tons
 * 850 kW : 596.822 tons
 * 
 * From "Energy and CO2 life-cycle analyses of wind turbines- review and applications
 */
class DefaultWindTurbineComponents(val weight: Mass)  extends AbstractWindTurbineComponents {
  val blades = (weight*0.027, Material("Glass fibre, epoxy,PVC", Megajoules(61.8), Kilograms(1)))
  val hub = (weight*0.035, Material("Steel", Megajoules(36.8), Kilograms(1)))
  val transmission = (weight*0.052, Material("Steel", Megajoules(36.8), Kilograms(1)))
  val generator = (weight*0.026, Material("Copper", Megajoules(86.2), Kilograms(1)))
  val nacelle = (weight*0.003, Material("Glass fibre", Megajoules(61.8), Kilograms(1)))
  val tower = (weight*0.233, Material("Steel", Megajoules(36.8), Kilograms(1)))
  val foundations = (weight*0.603, Material("Concrete", Megajoules(3.2), Kilograms(1)))
  val electrical = (weight*0.021, Material("Copper", Megajoules(86.2), Kilograms(1)))
  
  val components = List(blades,hub,transmission,generator,nacelle,tower,foundations,electrical)
  val embodiedEnergy = components.map(c => c._2.energyIntensity*c._1).foldLeft(Gigajoules(0))(_ + _)
}

/**
 * To get the embodied energy in wind turbine due to the manufacturing of the components
 */
class WindTurbineComponents(val ratedPower: Power,
  val foundation: WindTurbineComponent,
  val tower: WindTurbineComponent,
  val nacelle: WindTurbineComponent,
  val rotor: WindTurbineComponent)

    extends WindTurbineComponent(foundation.components ++ tower.components ++ nacelle.components ++ rotor.components) {
}

object WindTurbineComponents {
  def apply(power: Power): WindTurbineComponents = apply(power.toMegawatts)

  def apply(mw: Double): WindTurbineComponents = {
    if (mw == 0.85) new WindTurbine850kWComponents
    else if (mw == 2) new WindTurbine2MWComponents
    else if (mw == 3) new WindTurbine3MWComponents
    else throw new IllegalArgumentException("No value for" + mw + "MW wi,d turbine")
  }
}

class WindTurbineComponent(val components: List[(Mass, String)] = List(), val materials: Materials = Materials()) extends AbstractWindTurbineComponents {
  val weight = components.map(_._1).foldLeft(Tonnes(0))(_ + _)
  val embodiedEnergy = components.map(m => materials(m._2).getOrElse(Material("", Joules(0), Tonnes(1))).energyIntensity * m._1).foldLeft(Gigajoules(0))(_ + _)
}

object WindTurbineComponent {
  def apply(values: (Double, String)*) = new WindTurbineComponent(values.map(v => (Tonnes(v._1), v._2)).toList)
  }
/**
 *
 * Examples of the composition of actual wind turbines
 *
 */
class WindTurbine850kWComponents extends WindTurbineComponents(Kilowatts(850),
  foundation = WindTurbineComponent((480.0, "Concrete"), (15.0, "Steel")),
  tower = WindTurbineComponent((69.07, "Steel"), (0.93, "Paint")),
  nacelle = WindTurbineComponent((3.35 + 2.41 + 1.47 + 4.21 + 0.26 + 0.26 + 6.08 + 1 + 0.26 + 0.894, "Steel"), (0.37 + 0.062 + 0.24 + 0.357, "Copper"), (0.062 + 0.18 + 0.357, "Aluminium")), //(0.18,Plastic)
  rotor = WindTurbineComponent((4.8, "Steel"), (3.01, "GlassFibre"), (2.01, "EpoxyResin"), (0.18, "Steel")))

class WindTurbine2MWComponents extends WindTurbineComponents(Megawatts(2),
  foundation = WindTurbineComponent((700, "Concrete"), (25, "Iron"), (15, "Steel")),
  tower = WindTurbineComponent((143, "Steel")),
  nacelle = WindTurbineComponent((6.1 + 3.3 + 4.29 + 8, "Steel"), (1.5 + 2, "Copper"), (0.149 + 0.195, "Silica"), (10.5 + 8, "Iron"), (0.8, "GlassFibre"), (1.2, "EpoxyResin")),
  rotor = WindTurbineComponent((7.8 + 0.124, "GlassFibre"), (11.7 + 0.186, "EpoxyResin"), (14, "Iron")))

class WindTurbine3MWComponents extends WindTurbineComponents(Megawatts(3),
  foundation = WindTurbineComponent((1140.0, "Concrete"), (36.0, "Steel")),
  tower = WindTurbineComponent((158.76, "Steel"), (1.24, "Paint")),
  nacelle = WindTurbineComponent((13 + 9.33 + 5.71 + 1.02 + 23.58 + 3.87 + 1.02 + 3.47, "Steel"), (1.43 + 0.241 + 0.94 + 1.38, "Copper"), (0.241 + 0.69 + 1.38, "Aluminium")), //(0.7,Plastic)
  rotor = WindTurbineComponent((19.2, "Steel"), (12.04, "GlassFibre"), (8.03, "EpoxyResin"), (0.73, "Steel")))