package calculation
/**
 * 4 roughness classes :
 *  - 0 : z_0 = 0.0002 m (water areas, lakes) 
 *  - 1 : z_0 = 0.03 m (open areas with few windbreaks)
 *  - 2 : z_0 = 0.1 m (farm land with windbreaks)
 *  - 3 : z_0 = 0.4 m (urban districts, forests, and farm land with mny windbreaks)
 */

abstract class WindTurbine(val ratedPower: Double, val diameter: Double,
    val hubHeight: Double, val z0: Double) {
  val powerCurve: List[(Int, Int)]

  def power(windSpeed: Double, h0: Double): Double = power(windExtrapolation(windSpeed, h0, z0))
  def power(windSpeed: Double): Double = powerCurve.find(i => i._1 == Math.floor(windSpeed).toInt).getOrElse((0, 0))._2

  def windExtrapolation(v0: Double, h0: Double, z0: Double): Double = {
    v0 * (Math.log(hubHeight / z0) / Math.log(h0 / z0))
  }

}

class Enercon82_2000(hubHeight: Double, z0: Double) extends WindTurbine(2000, 82, hubHeight, z0) {
  val powerCurve = List(
    (0, 0),
    (1, 0),
    (2, 3),
    (3, 25),
    (4, 82),
    (5, 174),
    (6, 321),
    (7, 532),
    (8, 815),
    (9, 1180),
    (10, 1580),
    (11, 1810),
    (12, 1980),
    (13, 2050),
    (14, 2050),
    (15, 2050),
    (16, 2050),
    (17, 2050),
    (18, 2050),
    (19, 2050),
    (20, 2050),
    (21, 2050),
    (22, 2050),
    (23, 2050),
    (24, 2050),
    (25, 2050),
    (26, 0))
}