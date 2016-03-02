package simulation

import squants.Quantity
import squants.space._
import squants.mass._
import squants.energy._

/**
 * A product and the process to create a unit of it
 * The functionnal unit should be defined 
 */
class Product[A <: Quantity[A]](val name : String, val process : Process[A]) {

 
}

