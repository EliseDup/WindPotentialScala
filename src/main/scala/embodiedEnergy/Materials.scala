package embodiedEnergy

import squants.energy._
import squants.mass._
import squants.mass.MassConversions
import org.joda.convert.ToString

object Test {
  def main(args: Array[String]): Unit = {
    println(new WindTurbine3MW)
    println(new WindTurbine850kW)
  }
}

case class Material(val energy: Energy, val unit: Mass) {
  val energyIntensity = energy / unit
}

object Concrete20MPa extends Material(Gigajoules(4.08), Kilograms(2400))
object Steel extends Material(Gigajoules(85.3), Tonnes(1))
object Aluminium extends Material(Gigajoules(252), Tonnes(1)) //54 MJ / kg average worldwide?
object Copper extends Material(Gigajoules(379), Tonnes(1)) // 25 MJ / kg average worldwide?
object GlassFibre extends Material(Gigajoules(168), Tonnes(1))
object Epoxy extends Material(Gigajoules(163), Tonnes(1))
object Paint extends Material(Gigajoules(0.096), Kilograms(10)) // 1m^2 ?

class WindTurbineComponent(val materials: List[(Mass, Material)] = List()) {
  val weight = materials.map(_._1).foldLeft(Tonnes(0))(_ + _)
  val embodiedEnergy = materials.map(m => m._2.energyIntensity * m._1).foldLeft(Gigajoules(0))(_ + _)
  val energyIntensity = embodiedEnergy / weight
}
object WindTurbineComponent {
  def apply(values: (Double, Material)*) = new WindTurbineComponent(values.map(v => (Tonnes(v._1), v._2)).toList)
}

class WindTurbineComposition(val power: Power,
  val foundation: WindTurbineComponent = new WindTurbineComponent(),
  val tower: WindTurbineComponent = new WindTurbineComponent(),
  val nacelle: WindTurbineComponent = new WindTurbineComponent(),
  val rotor: WindTurbineComponent = new WindTurbineComponent())
    extends WindTurbineComponent(foundation.materials ++ tower.materials ++ nacelle.materials ++ rotor.materials) {
  override def toString = "Wind turbine power : " + power + ", total weight : " + weight + ",total energy embodied : " + embodiedEnergy + ", =>" + energyIntensity
}

class WindTurbine850kW extends WindTurbineComposition(Kilowatts(850),
  foundation = WindTurbineComponent((480.0, Concrete20MPa), (15.0, Steel)),
  tower = WindTurbineComponent((69.07, Steel), (0.93, Paint)),
  nacelle = WindTurbineComponent((3.35 + 2.41 + 1.47 + 4.21 + 0.26 + 0.26 + 6.08 + 1 + 0.26 + 0.894, Steel), (0.37 + 0.062 + 0.24 + 0.357, Copper), (0.062 + 0.18 + 0.357, Aluminium)), //(0.18,Plastic)
  rotor = WindTurbineComponent((4.8, Steel), (3.01, GlassFibre), (2.01, Epoxy), (0.18, Steel)))

class WindTurbine3MW extends WindTurbineComposition(Megawatts(3),
  foundation = WindTurbineComponent((1140.0, Concrete20MPa), (36.0, Steel)),
  tower = WindTurbineComponent((158.76, Steel), (1.24, Paint)),
  nacelle = WindTurbineComponent((13 + 9.33 + 5.71 + 1.02 + 23.58 + 3.87 + 1.02 + 3.47 + 19.2 + 0.73, Steel), (1.43 + 0.241 + 0.94 + 1.38, Copper), (0.241 + 0.69, Aluminium)), //(0.7,Plastic)
  rotor = WindTurbineComponent((19.2, Steel), (12.04, GlassFibre), (8.03, Epoxy), (0.73, Steel)))