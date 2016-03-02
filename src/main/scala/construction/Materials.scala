package construction

import squants.energy._
import squants.mass._
import utils._
import squants.space._
import transportation.Transport

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
  def apply(e : Double) : EnergyIntensity = this(Megajoules(e), Kilograms(1))
  def apply(energy: Energy, unit: Mass) = new EnergyIntensity(energy, energy, energy, unit)
  def apply(energy: Energy, bat: Energy, unit: Mass) = new EnergyIntensity(energy, bat, bat, unit)
}

/**
 * 
 * The energy intensity is generally given by the Ecoinvent process.
 * It includes all the energy input for "CRADLE TO GATE" (i.e. extracion, transport to factory, transformation)
 * 
 * Distance from factory is the average distance from factory to the next place ? TODO !!
 * 
 * Assume 1000 km by truck for transportation of the large components from the supplier to Vestas' factories 
 * (VESTAS - LCA of 1.65 MW wind turbines)
 * 
 */
case class Material(name: String, energy: EnergyIntensity, distanceFromFactory: Length = Kilometers(1000)) {

  val energyIntensity = energy.current / energy.unit
  
  def productionEnergy(mass : Mass) = energyIntensity*mass
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
 * Main components : Steel - Concrete - Fiberglass 
 * 
 * 
 * ICE DATABASE !! Mean values
 * 
 */
// MAIN COMPONENTS OF WIND TURBINES

object Aluminium extends Material("Aluminium", EnergyIntensity(155))
object CastIron extends Material("CastIron", EnergyIntensity(25))
object Concrete extends Material("Concrete", EnergyIntensity(0.75))
object Copper extends Material("Copper", EnergyIntensity(42))
object Steel extends Material("Steel", EnergyIntensity(32.6))
object StainlessSteel extends Material("StainlessSteel", EnergyIntensity(74.8))
object Lead extends Material("Lead", EnergyIntensity(25.21)) // TODO
object Ballast extends Material("Ballast", EnergyIntensity(0)) // TODO
object Zinc extends Material("Zinc", EnergyIntensity(51)) 

// Glass Reinforced Plastic - GRP - Fibreglass
object EpoxyResin extends Material("EpoxyResin", EnergyIntensity(137))
object Fiberglass extends Material("Fiberglass", EnergyIntensity(28))
object GlassReinforcedPlastic extends Material("GlassReinforcedPlastic", EnergyIntensity(100))

object Insulation extends Material("Insulation", EnergyIntensity(45))
object Resin extends Material("Resin", EnergyIntensity(75))
// Energy Use and Energy Intensity of the U.S. Chemical Industry
// Ernst Worrell, Dian Phylipsen, Dan Einstein, and Nathan Martin ?

// Plastics
object Polypropylene extends Material("Polypropylene", EnergyIntensity(95.89))
object Polyethylene extends Material("Polyethylene", EnergyIntensity(83.1))
object Polystyrene extends Material("Polystyrene", EnergyIntensity(99.20))
object Plastic extends Material("Plastic", EnergyIntensity(80.5))
object PVC extends Material("PVC", EnergyIntensity(77.2))

// From industrial efficienct technology database 
object Cement extends Material("Cement", EnergyIntensity(5.2))
object Diesel extends Material("Diesel", EnergyIntensity(42.7))
object LubricatingOil extends Material("LubricatingOil",EnergyIntensity(42.7)) //TODO
object Electronics extends Material("Electronics",EnergyIntensity(42))

object SteelBar extends Material("SteelBar", EnergyIntensity(32.6))
object Gasoline extends Material("Gasoline", EnergyIntensity(43.1))
object Paint extends Material("Paint", EnergyIntensity(70)) // TODO
object Polyester extends Material("Polyester", EnergyIntensity(45.7))
object Silica extends Material("Silica", EnergyIntensity(30.6))

object Materials {

  def list = List(Aluminium, CastIron, Concrete, Copper, Diesel,
    EpoxyResin, Fiberglass, Gasoline, Paint,
    Polyester, Silica, Steel, SteelBar, Lead,
    Polyester, Polyethylene, Polypropylene, Polystyrene)
  def apply(name: String): Option[Material] = list.find(_.name.equals(name))
  val none = Material("", EnergyIntensity(Joules(0), Kilograms(1)), Kilometers(1))
  def getOrNone(name: String): Material = {
 //   if( !apply(name).isDefined) println(name)
    apply(name).getOrElse(none)
  }

}
