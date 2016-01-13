package embodiedEnergy

import squants.energy._
import squants.mass._
import squants.mass.MassConversions
import org.joda.convert.ToString
import calculation._
import squants.time.Hours
import utils._

/**
 * To get the embodied energy in wind turbine due to the manufacturing of the components
 */
class WindTurbineComponent(val components: List[(Mass, String)] = List(), val materials : Materials = Materials()) {
  val weight = components.map(_._1).foldLeft(Tonnes(0))(_ + _)
  val embodiedEnergy = components.map(m => materials(m._2).getOrElse(Material("",Joules(0),Tonnes(0))).energyIntensity * m._1).foldLeft(Gigajoules(0))(_ + _)
  val energyIntensity = embodiedEnergy / weight
}

object WindTurbineComponent {
  def apply(values: (Double, String)*) = new WindTurbineComponent(values.map(v => (Tonnes(v._1), v._2)).toList)
 //  def newMaterials(values: (Double, Energy)*) = new WindTurbineComponent(values.map(v => (Tonnes(v._1), Material(v._2, Tonnes(v._1)))).toList)
}

class WindTurbineComponents(val ratedPower: Power,
  val foundation: WindTurbineComponent,
  val tower: WindTurbineComponent,
  val nacelle: WindTurbineComponent,
  val rotor: WindTurbineComponent)
    extends WindTurbineComponent(foundation.components ++ tower.components ++ nacelle.components ++ rotor.components) {
  override def toString = "Wind turbine power : " + ratedPower + ", total weight : " + weight + ",total energy embodied : " + embodiedEnergy + ", =>" + energyIntensity.to(GigajoulesPerton) + "GJ/t"
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
/**
 * Example of actual wind turbines
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