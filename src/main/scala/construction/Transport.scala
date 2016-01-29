package construction

import squants.energy._
import squants.mass._
import squants.space._

/**
 * Table 1
 *
 * Impact calculation coefficients for transport stage from production plant to building
 * site of 1 tonne
 *
 * 																					Lorry,road | Freight,rail | Transoceanic,freight ship
 *
 * Primary energy demand (MJeEq/km) 						3.266   | 0.751				| 0.170
 * Global Warming Potential (kg CO2eEq/km) 			0.193 	|	0.039 			| 0.011
 * Water demand (l/km) 													1.466		| 1.115				| 0.097
 */
object Transport {
 
  val road = Megajoules(3.266) / Kilometers(1) / Tonnes(1)
  val rail = Megajoules(0.751) / Kilometers(1) / Tonnes(1)
  val ship = Megajoules(0.170) / Kilometers(1) / Tonnes(1)

  def truckTransport(mass: Mass, distance: Length) = road*mass*distance
  def shipTransport(mass: Mass, distance: Length) = ship*mass*distance
  def trainTransport(mass: Mass, distance: Length) = rail*mass*distance
  
  def transportEnergy(mass: Mass, distance: Length, isRoad: Boolean = true, isSea: Boolean = false, isRail: Boolean = false): Energy = {
    val tr = if (isRoad) road else if (isRail) rail else ship
    tr * mass * distance
  }
}