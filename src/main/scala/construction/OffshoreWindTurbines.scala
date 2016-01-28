package construction

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
 */
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

object OffshoreWind {
  // val floatingCables = 
}