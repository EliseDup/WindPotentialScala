import construction.Materials
import gridData._
import squants.space._
import windEnergy.WindTurbineWithPower
import utils.PlotHelper
import squants.energy._
import squants.space._
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
import windEnergy._
import scala.io.Source
import org.jfree.data.category.DefaultCategoryDataset
import org.apache.commons.math3.special.Gamma
import scala.collection.mutable.ListBuffer
import squants.radio.WattsPerSquareMeter

object WindPotentialSimulation {

  def main(args: Array[String]): Unit = {

    val wind = new WorldGrid("results/worldGridWind.txt", Degrees(0.5))
    wind.writeGrid("energyGenerated")
    val grids = wind.grids; val onshore = wind.onshoreGrids; val offshore = wind.offshoreGrids

    PlotHelper.cumulativeDensity(grids.map(_.windSpeed.toMetersPerSecond))
    
    val onshoreConstrained = wind.onshoreGrids.filter(WindPotential.suitabilityFactor(_) > 0)
    val offshoreConstrained = wind.offshoreGrids.filter(WindPotential.suitabilityFactor(_) > 0)
    val constrained = wind.grids.filter(WindPotential.suitabilityFactor(_) > 0)
    
      printMinEROIValues(onshoreConstrained, 5.0)
      printMinEROIValues(onshoreConstrained, 7.0)
      printMinEROIValues(onshoreConstrained, 10.0)
 
   // val a =  wind.listValueVSCumulated(onshore.map(g => (WindPotential.EROI(g), WindPotential.powerCaptured(g,1.0, topDown = false).to(Terawatts))))
   // val b =  wind.listValueVSCumulated(onshore.map(g => (WindPotential.EROI(g), WindPotential.powerCaptured(g,1.0, topDown = true).to(Terawatts))))
    
   // PlotHelper.plotXY( List((a._1,a._2,"Unconstrained"),(b._1,b._2,"Maximum Rate of Extraction")))
   // eroi(wind, onshore)  
   }

  def printMinEROIValues(gr: List[GridCell], eroi: Double) = {
    val sust = gr.filter(WindPotential.EROI(_) >= eroi)
    println("---")
    println("Power captured" + "\t" + sust.map(WindPotential.powerCaptured(_).to(Terawatts)).sum + "\t" + "TW")
    println("Power installed" + "\t" + sust.map(WindPotential.powerInstalled(_).to(Megawatts)).sum + "\t" + "MW")
    println("Area" + "\t" + sust.map(g => WindPotential.suitabilityFactor(g) * g.area.toSquareKilometers).sum + "\t" + "km2")

  }

  def plotEGenVSArea(wind: WorldGrid, gr: List[GridCell]) {
    val list = wind.listValueVSArea(gr.map(g => (WindPotential.energyGeneratedPerYear(g).to(TerawattHours), WindPotential.suitabilityFactor(g) * g.area)))
    PlotHelper.plotXY(List((list._1, list._2, "")), xLabel = "Area [millions km2]", yLabel = "Energy Generated [TWh/year]")

  }
  def plotSpeedVSArea(wind: WorldGrid, gr: List[GridCell]) {
    val vHub = wind.listValueVSArea(gr.map(g => (g.windSpeedHub.value, g.area)))
    val vHubGeo = wind.listValueVSArea(gr.map(g => (g.windSpeedHub.value, g.area * WindPotential.suitabilityFactor(g))))

    PlotHelper.plotXY(List((vHub._1, vHub._2, "Total"), (vHubGeo._1, vHubGeo._2, "Suitability Factor")), xLabel = "Area [millions km2]", yLabel = "Wind Speed [m/s]")
  }

  def eroi(wind: WorldGrid, gr: List[GridCell]) {
    val potential = WindPotential
    val all = wind.listEROIVSCumulatedPower(gr.map(g => (g, 1.0)), potential)
    val landUse = wind.listEROIVSCumulatedPower(gr.map(g => (g, potential.landUseFactor(g))), potential)
    val windRegime = wind.listEROIVSCumulatedPower(gr.map(g => (g, potential.landUseFactor(g) * potential.windRegimeFactor(g))), potential)

    PlotHelper.plotXY(List(
      (all._1, all._2, "Total"),
      (landUse._1, landUse._2, "Geographic potential"),
      (windRegime._1, windRegime._2, "Wind Regime Restrictions")),

      xLabel = "Puissance Moyenne [TW]", //Mean power captured [TW]",
      yLabel = "EROI") //, legend = true)
  }

  def technicalPotential(grids: List[GridCell], potential: EnergyGenerationPotential = WindPotential) {
    PlotHelper.histogram(grids.map(potential.energyGeneratedPerYear(_).to(TerawattHours)), n = 20, xLabel = "Technical potential in grid cell [TWh]", yLabel = "# Grid cells")
  }

  def areaRepartition(wind: WorldGrid, potential: EnergyGenerationPotential) {

    val out_stream = new PrintStream(new java.io.FileOutputStream("suitability"))
    wind.grids.map(g => out_stream.print(g.center.latitude.value.toString + "\t" + g.center.longitude.value.toString +
      "\t" + potential.suitabilityFactor(g) + "\t" + (if (g.windSpeed.toMetersPerSecond >= 3) "1.0" else "0.0") + "\n"))
    out_stream.close()

    printGrids(wind.grids)
    println("----------")
    printGrids(wind.onshoreGrids)
    println("----------")
    printGrids(wind.offshoreGrids)

    def print(name: String, list: List[GridCell]) {
      println(name + "\t" + list.size + "\t" + wind.area(list).toSquareKilometers)
    }

    def printGrids(grids: List[GridCell]) {
      print("Total", grids)
      print("protected", grids.filter(_.protectedArea))
      print("agricultural", grids.filter(_.lc.isAgricultural))
      print("openArea", grids.filter(_.lc.isOpenArea))
      print("forest", grids.filter(_.lc.isForest))
      print("water", grids.filter(_.lc.isWaterBodies))
      print("flooded ", grids.filter(_.lc.isFloodedArea))
      print("ice", grids.filter(_.lc.isIce))
      print("urban", grids.filter(_.lc.isUrban))
      print("wind regime", grids.filter(_.windSpeed.toMetersPerSecond >= 4))
      print("altitude", grids.filter(_.elevation.toMeters <= 2000))
      print("sea level", grids.filter(_.elevation.toMeters >= -200))
      println("WithUrbanFactor" + "\t" + grids.filter(_.lc.isUrban).map(g => g.urbanFactor * g.area.toSquareKilometers).sum)
    }
  }
}
