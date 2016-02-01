package construction

import squants.energy._
import squants.mass._
import utils._
import squants.space._

/**
 * Classification of energy intensities :
 *  - current value
 *  - best available technology
 *  - theoritical limit
 */

class EnergyIntensity(
  val current: Energy,
  val BAT: Energy,
  val limit: Energy,
  val unit: Mass)
object EnergyIntensity {
  def apply(energy: Energy, unit: Mass) = new EnergyIntensity(energy, energy, energy, unit)
  def apply(energy: Energy, bat: Energy, unit: Mass) = new EnergyIntensity(energy, bat, bat, unit)
}

case class Material(name: String, energy: EnergyIntensity, distanceFromFactory: Length = Kilometers(600)) {

  val energyIntensity = energy.current / energy.unit
  def transportEnergy(mass: Mass, isRoad: Boolean = true, isSea: Boolean = false, isRail: Boolean = false) = {
    Transport.transportEnergy(mass, distanceFromFactory, isRoad, isSea, isRail)
  }
  def toArray = List(name, energy.current.toGigajoules, "GJ", energy.unit.toTonnes, "t").toArray
}

/**
 * A list of materials used in wind turbine manufacturing and their
 * energy efficiency [GJ/unit]
 *
 * From J.Yang, B.Chen - Integrated evaluation of embodied energy, greenhouse gas emission and economic performance of a typical wind farm in China
 *
 *
 */

object Aluminium extends Material("Aluminium", EnergyIntensity(Megajoules(208), Kilograms(1)))
object CastIron extends Material("CastIron", EnergyIntensity(Megajoules(32.6), Kilograms(1))) // TODO
object Concrete extends Material("Concrete", EnergyIntensity(Megajoules(6.03), Kilograms(2.4)),Kilometers(50))
object Copper extends Material("Copper", EnergyIntensity(Megajoules(164), Kilograms(1)))
object Diesel extends Material("Diesel", EnergyIntensity(Megajoules(42.7), Kilograms(1)))
object EpoxyResin extends Material("EpoxyResin", EnergyIntensity(Megajoules(45.7), Kilograms(1)))
object Fiberglass extends Material("Fiberglass", EnergyIntensity(Megajoules(188), Kilograms(1)))
object Gasoline extends Material("Gasoline", EnergyIntensity(Megajoules(43.1), Kilograms(1)))
object Iron extends Material("Iron", EnergyIntensity(Megajoules(32.6), Kilograms(1))) // TODO
object Paint extends Material("Paint", EnergyIntensity(Megajoules(0), Kilograms(1))) // TODO
object Polyester extends Material("Polyester", EnergyIntensity(Megajoules(45.7), Kilograms(1)))
object Silica extends Material("Silica", EnergyIntensity(Megajoules(30.6), Kilograms(1)))
object Steel extends Material("Steel", EnergyIntensity(Megajoules(32.6), Kilograms(1)))
object SteelBar extends Material("SteelBar", EnergyIntensity(Megajoules(32.6), Kilograms(1)))
object Lead extends Material("Lead", EnergyIntensity(Megajoules(0), Kilograms(1))) // TODO

// Energy Use and Energy Intensity of the U.S. Chemical Industry
// Ernst Worrell, Dian Phylipsen, Dan Einstein, and Nathan Martin
object Polypropylene extends Material("Polypropylene", EnergyIntensity(Megajoules(10.5), Kilograms(1)))
object Polyethylene extends Material("Polyethylene", EnergyIntensity(Megajoules(9.3), Kilograms(1)))
object Polystyrene extends Material("Polystyrene", EnergyIntensity(Megajoules(9.3), Kilograms(1)))
object Plastic extends Material("Plastic", EnergyIntensity(Megajoules(10), Kilograms(1)))

// From industrial efficienct technology database 
object Cement extends Material("Cement", EnergyIntensity(Gigajoules(3.5), Gigajoules(1.1), Tonnes(1)))

object Materials {

  def list = List(Aluminium, CastIron, Concrete, Copper, Diesel,
    EpoxyResin, Fiberglass, Gasoline, Iron, Paint,
    Polyester, Silica, Steel, SteelBar, Lead,
    Polyester, Polyethylene, Polypropylene, Polystyrene)
  def apply(name: String): Option[Material] = list.find(_.name.equals(name))
  val none = Material("", EnergyIntensity(Joules(0), Kilograms(1)), Kilometers(1))
  def getOrNone(name: String): Material = apply(name).getOrElse(none)

}
