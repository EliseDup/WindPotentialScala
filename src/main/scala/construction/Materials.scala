package construction

import squants.energy._
import squants.mass._
import utils._

class Materials(val list: List[Material]) {
  def apply(name: String): Option[Material] = list.find(_.name.equals(name))
}

case class Material(name: String, energy: Energy, unit: Mass) {
  val energyIntensity = energy / unit
  def toArray = List(name, energy.value, energy.unit.symbol, unit.value, unit.unit.symbol).toArray
}

/**
 * A list of materials used in wind turbine manufacturing and their
 * energy efficiency [GJ/unit]
 * 
 * From J.Yang, B.Chen - Integrated evaluation of embodied energy, greenhouse gas emission and economic performance of a typical wind farm in China 
 */
object Materials {
  def apply() = {
    new Materials(List(
      Material("Concrete", Megajoules(6.03E3), Tonnes(2.4)),
      Material("Steel", Megajoules(3.26E4), Tonnes(1)),
      Material("Aluminium", Megajoules(2.08E5), Tonnes(1)),
      Material("Copper", Megajoules(1.64E5), Tonnes(1)),
      Material("GlassFibre", Megajoules(1.88E5), Tonnes(1)),
      Material("EpoxyResin", Megajoules(4.57E4), Tonnes(1)),
      Material("Iron", Megajoules(0), Kilograms(1)),
      Material("Silica", Megajoules(4.57E4), Tonnes(1)),
      Material("Polyester", Megajoules(3.06E4), Tonnes(1)),
      Material("Diesel", Megajoules(4.27E4),Tonnes(1)),
      Material("Gasoline", Megajoules(4.31E4),Tonnes(1)),
      Material("FreshWater", Megajoules(1.33E3),Tonnes(1))))
    
  }
}
