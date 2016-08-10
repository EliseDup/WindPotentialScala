package windEnergy

import gridData.WorldGrid
import squants.space.Degrees
import squants.energy.Watts
import squants.motion.MetersPerSecond
import squants.time.Terahertz
import utils.Terawatts

object KineticEnergyExtraction {
  val world = new WorldGrid("results/worldGridWind.txt", Degrees(0.5))
  val grids = world.onshoreGrids
  
  def main(args: Array[String]): Unit = {
    
    val turbineDensity = (0.0 to 10.0 by 0.1).toList
    val dn_0 = grids.map(g => g.kineticEnergyDissipation*g.area).foldLeft(Watts(0))(_ + _) // / world.area(grids)
    val v_10_0 = MetersPerSecond(grids.map(g => g.windSpeed.toMetersPerSecond * g.area.toSquareMeters).sum / world.area(grids).toSquareMeters)
    val v_80_0 = MetersPerSecond(grids.map(g => g.windSpeedHub.toMetersPerSecond * g.area.toSquareMeters).sum / world.area(grids).toSquareMeters)
    
    println(dn_0.to(Terawatts) + "\t" + v_10_0 + "\t" +v_80_0)
    
    
  }

}