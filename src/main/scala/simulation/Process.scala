package simulation

import squants.mass._
import squants.energy._
import squants.Quantity

/**
 * A basic process only take energy as an input and returns a given quantity of a product
 */

/**
 * Energy per unit of product 
 * Inputs : n units needed per unit of the product of the process
 */

class Process[A <: Quantity[A]](val unit : A, val energyRequirement : Energy, 
    inputs: List[(Mass, Product[Mass])])