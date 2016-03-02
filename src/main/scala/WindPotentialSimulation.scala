import construction.Materials
import gridData._
import squants.space._
import windEnergy.WindTurbineWithPower
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
import windEnergy.WindFarm
import windEnergy.OffshoreWindFarm

object WindPotentialSimulation {
  def main(args: Array[String]): Unit = {
    println((Terawatts(15) * Hours(365 * 24)).to(Exajoules))
    val onshore = new WindFarm(Megawatts(500))
    val offshore = new OffshoreWindFarm(Megawatts(500), Kilometers(50), Meters(50))

    println(onshore.embodiedEnergy.toGigajoules / 500.0)
    println(offshore.embodiedEnergy.toGigajoules / 500.0)

    val potential = WindPotential

    val wind = new WorldGrid("world5years", Degrees(0.5))

    wind.plotEROIVSCumulatedProduction(wind.constrainedGrids(potential), potential)
    wind.plotEROIVSCumulatedProduction(wind.offshoreConstrainedGrids(potential), potential)
    wind.plotEROIVSCumulatedProduction(wind.onshoreConstrainedGrids(potential), potential)
  }
}