package construction

import squants.energy.Megajoules
import squants.space.Kilometers
import squants.mass.Tonnes

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
  
}