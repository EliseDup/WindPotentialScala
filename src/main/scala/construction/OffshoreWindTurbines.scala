package construction

import squants.mass._
import squants.space.Length
import utils.Helper.Point
import utils.Helper

/**
 * Foundations change with water depth
 *  - 0-30m / 1-2 MW : Monopile
 *  - 25-50m / 2-5 MW : Jacket / tripod
 *  - > 50 m / 5-10 MW : Floating structure
 *
 *  Distance to coast (km) 0–10 10–20 20–30 30–40 40–50 50–100 100–200 >200
 *  Depth (m) 					10–20 1 1.022 1.043 1.065 1.086 1.183 1.408 1.598
 *  										20–30 1.067 1.090 1.113 1.136 1.159 1.262 1.501 1.705
 *  										30–40 1.237 1.264 1.290 1.317 1.344 1.464 1.741 1.977
 *  										40–50 1.396 1.427 1.457 1.418 1.517 1.653 1.966 2.232
 * From :
 *
 * Inventory of location specific wind energy cost - May 2011 - WINDSPEED
 *
 * Jacket : 20m to 50m
 */
object OffshoreFoundations {

  def foundation(depth: Length) = {
    val d = depth.toMeters
    if (d <= 30) monopile(depth)
    else if (d <= 50) jacket(depth)
    else if (d <= 150) floatingTLP
    else floatingSpar
  }
  // Total = jacket + pile (constant = 241 tons)
  val jacketWeightFuntion = Helper.SecondOrderPolynomial(Point(20.0, 400.0), Point(40.0, 550.0), Point(60.0, 890.0))
  def jacket(depth: Length) = Components((Tonnes(jacketWeightFuntion(depth.toMeters) + 241.6), Steel))

  // Total = tower + transition piece
  val towerWeightFunction = Helper.FirstOrderPolynomial(Point(20, 305), Point(50, 325))
  val transitionWeightFunction = Helper.SecondOrderPolynomial(Point(20, 310), Point(40, 605), Point(50, 800))
  def monopile(depth: Length) = Components((Tonnes(towerWeightFunction(depth.toMeters) + transitionWeightFunction(depth.toMeters)), Steel))

  // Floating 
  val floatingSpar = Components((Tonnes(1300), Steel), (Tonnes(5250), Ballast))
  val floatingTLP = Components((Tonnes(1675), Steel))

  /*  
  val monopile = Components((Tonnes(650),Steel))
  val jacket = Components((Tonnes(759),Steel),(Tonnes(126),Concrete))
  val floating = Components((Tonnes(1258),Steel))
  */

}

object MultiplyingFactor {

  def factor(depth: Double, distance: Double) = {
    val i = distances.zipWithIndex.find(x => x._1._1 <= distance && x._1._2 > distance).get._2
    val j = depths.zipWithIndex.find(x => x._1._1 <= depth && x._1._2 > depth).get._2
    factors(j)(i)
  }

  val depths = List((0, 20), (20, 30), (30, 40), (40, 100000))
  val distances = List((-100000, 10), (10, 20), (20, 30), (30, 40), (40, 50), (50, 100), (100, 200), (200, 100000))
  val factors =
    Array(Array(1.0, 1.022, 1.043, 1.065, 1.086, 1.183, 1.408, 1.598),
      Array(1.067, 1.090, 1.113, 1.136, 1.159, 1.262, 1.501, 1.705),
      Array(1.237, 1.264, 1.290, 1.317, 1.344, 1.464, 1.741, 1.977),
      Array(1.396, 1.427, 1.457, 1.418, 1.517, 1.653, 1.966, 2.232))
}