package operation

import squants.space.Length
import squants.space.Area

/**
 * Array sizeTurbine spacing
 * Array efficiency(%)
 * 													4D  5D  6D  7D  8D  9D
 * 2x2 											81  87  91  93  95  96
 * 4x4 											65  76  82  87  90  92
 * 6x6 											57  70  78  83  87  90
 * 8x8 											52  66  75  81  85  88
 * 10x10 										49  63  73  79  84  87
 *
 * From : A methodological review to estimate techno-economical wind energy production - Julieta Schallenberg-Rodriguez
 */

object WakeEffect {

  // We have to fix a spacing and then we wan evalate the number of turbines in the area and the corresponding array efficieny
  // 5D corresponds to 4 2MW wind turbines of diameter 80 m / km^2 => 8MW /km^2
  // 9D corresponds to 4MW / km^2 with 2MW wind turbines
  val spacingIndex = 1 // 0 is 4D, 1 is 5D, .. and 5 is 9D

  val spacing = (4 to 9).toArray
  val n = Array(4, 16, 36, 64, 100)
  val array = Array(
    Array(0.81, 0.87, 0.91, 0.93, 0.95, 0.96),
    Array(0.65, 0.76, 0.82, 0.87, 0.9, 0.92),
    Array(0.57, 0.7, 0.78, 0.83, 0.87, 0.9),
    Array(0.52, 0.66, 0.75, 0.81, 0.85, 0.88),
    Array(0.49, 0.63, 0.73, 0.79, 0.84, 0.87))

  val nDiameters = spacing(spacingIndex)

  def nTurbines(x: Length, y: Length, d: Length) = (x / (nDiameters * d) * y / (nDiameters * d))
  def nTurbines(area : Area, d : Length) = area / ((nDiameters * d)*(nDiameters * d))
  
  def wakeEffect(nTurbines: Double) = {
    val nIndex =
      if (nTurbines <= n(0)) 0
      else if (nTurbines <= n(1)) 1
      else if (nTurbines <= n(2)) 2
      else if (nTurbines <= n(3)) 3
      else 4

    array(nIndex)(spacingIndex)
  }
}