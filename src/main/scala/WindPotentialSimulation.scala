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

object WindPotentialSimulation {
  def main(args: Array[String]): Unit = {

    val energyGenerated = GigawattHours(3129)
    val energyConsumed = Megajoules(0.17)*energyGenerated.toKilowattHours
    
    println(energyConsumed.toGigajoules / 50 + " GJ /MW")
    
    println( Grams(1979*energyGenerated.toMegawattHours).to(Tonnes))
    
    val on = new WindTurbineWithPower("2MW")
    val off = new WindTurbineWithPower("5MWoffshore")
    val wind = new GridData("world5years", Degrees(0.5), on, off)

    val onshore = wind.onshoreGrids
    wind.plotEROIVSCumulatedProduction(wind.onshoreGrids)
    wind.plotEROIVSCumulatedProduction(wind.onshoreConstrainedGrids)
    def plotGrids(g: List[GridObject]) {
      val speed = g.map(_.loadHours.value)
      PlotHelper.cumulativeDensity(List((speed, "")), xLabel = "% sites", yLabel = "Load Hours")
    }
    println("--End--")
  }
  def plot(wind: GridData) {
    PlotHelper.cumulativeDensity(List((wind.windSpeeds(), "")), title = wind.name)
    PlotHelper.cumulativeDensity(List((wind.powerDensities(), "")), title = wind.name)
  }
  def plotEuropeWind() {
    val l = List("europe2002", "europeNovember2001", "europeFebruary2002", "europeMay2002", "europeAugust2002")
    val speed = l.map({ w =>
      val wind = new GridData("results/" + w, Degrees(0.125), new WindTurbineWithPower("2MW"), new WindTurbineWithPower("5MWoffshore"))
      (wind.windSpeeds(wind.onshoreGrids), w)
    })
    PlotHelper.cumulativeDensity(speed)
  }

}