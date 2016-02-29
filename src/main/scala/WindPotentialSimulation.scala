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
    val onshore = new WindFarm(Megawatts(500))
    val offshore = new OffshoreWindFarm(Megawatts(500), Kilometers(50), Meters(50))
    
    println(onshore.embodiedEnergy.toGigajoules / 500.0)
 println(offshore.embodiedEnergy.toGigajoules / 500.0)
 
    val wind = new GridData("world5years", Degrees(0.5))
   
   wind.plotEROIVSCumulatedProduction(turbinePowerDensity=Megawatts(2))

  }

}