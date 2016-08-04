

import gridData.WorldGrid
import squants.space.Degrees
import utils.Terawatts
import utils.PlotHelper
import gridData.WindPotential
import squants.energy.Watts

object WindResults {
  def main(args: Array[String]): Unit = {

    val world = new WorldGrid("results/worldGridWind.txt", Degrees(0.5))
    val onshore = world.onshoreGrids
    
    val demand_2016 = Terawatts(15)
    
    val res = world.listEROIVSCumulatedPower(onshore.map( (_,1.0)), WindPotential)
    PlotHelper.plotXY(res._1,res._2)
  }
}