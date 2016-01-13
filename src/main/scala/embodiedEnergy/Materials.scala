package embodiedEnergy

import squants.energy._
import squants.mass._
import squants.mass.MassConversions
import org.joda.convert.ToString
import calculation._
import squants.time.Hours
import utils._

class Materials(val list: List[Material]) {
  def apply(name: String): Option[Material] = list.find(_.name.equals(name))
}

case class Material(name: String, energy: Energy, unit: Mass) {
  val energyIntensity = energy / unit
  def toArray = List(name,energy.value,energy.unit.symbol,unit.value,unit.unit.symbol).toArray
  }

/**
 * A list of materials used in wind turbine manufacturing and their
 * energy efficiency [GJ/unit]
 */
object Materials {
  def apply() = {
    new Materials(List(
      Material("Concrete", Gigajoules(3.6), Tonnes(1)),
      Material("Steel", Gigajoules(85.3), Tonnes(1)),
      Material("Aluminium", Gigajoules(252), Tonnes(1)),
      Material("Copper", Gigajoules(379), Tonnes(1)),
      Material("GlassFibre", Gigajoules(168), Tonnes(1)),
      Material("EpoxyResin", Gigajoules(163), Tonnes(1)),
      Material("Iron", Gigajoules(0.096), Kilograms(10)),
      Material("Silica", Gigajoules(0), Tonnes(1))))
  }
}
/*object Concrete extends Material(Megajoules(6.03E3), Kilograms(2400))
object Steel extends Material(Megajoules(3.26E4), Tonnes(1))
object Aluminium extends Material(Megajoules(2.08E5), Tonnes(1)) 
object Copper extends Material(Megajoules(1.64E5), Tonnes(1))
object GlassFibre extends Material(Megajoules(1.88E5), Tonnes(1))
object EpoxyResin extends Material(Megajoules(4.57E4), Tonnes(1))
object Paint extends Material(Megajoules(0), Kilograms(1))
object Iron extends Material(Megajoules(0), Tonnes(1))
object Silica extends Material(Megajoules(3.06E4), Tonnes(1))*/

/*object Concrete extends Material(Gigajoules(4.08), Kilograms(2400))
object Steel extends Material(Gigajoules(85.3), Tonnes(1))
object Aluminium extends Material(Gigajoules(252), Tonnes(1)) 
object Copper extends Material(Gigajoules(379), Tonnes(1))
object GlassFibre extends Material(Gigajoules(168), Tonnes(1))
object EpoxyResin extends Material(Gigajoules(163), Tonnes(1))
object Paint extends Material(Gigajoules(0.096), Kilograms(10))
object Iron extends Material(Gigajoules(0), Tonnes(1))
object Silica extends Material(Gigajoules(1), Tonnes(1))*/

