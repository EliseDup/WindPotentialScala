import construction.Materials
import landCover._
import squants.space._
import operation._
import operation.WindTurbineWithPower
import utils.PlotHelper
import squants.energy._
import squants.time._
import squants.mass._
import squants.thermal._
import squants.motion._
import construction._
import utils._
import java.io.File
import java.io.PrintStream
import java.io.FileOutputStream
import utils.Helper.Point

object WindPotentialSimulation {
  def main(args: Array[String]): Unit = {

    /*
    val list = List("November2001","February2002","May2002","August2002")     
    val winds = 
      (for(l <- list) yield {
      val w = new GridData("europe"+l, Degrees(0.5))
      (w.windSpeeds(w.onshoreGrids),l)
    })
*/
  
    val wind = new GridData("world5years", Degrees(0.5))
    val offshore = wind.offshoreConstrainedGrids
    wind.plotEROIVSCumulatedProduction(offshore,Megawatts(2),Joules(0))

  }

}