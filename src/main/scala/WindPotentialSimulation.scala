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

    val world = new WorldGrid("results/worldGridWind.txt", Degrees(0.5))
    val solar = world.grids.filter(_.irradiance.value > 0)
    // eroi(world, solar, SolarPotential)
    eroi(world, world.onshoreGrids, WindPotential)
  }

  def printMinEROIValues(gr: List[GridCell], eroi: Double) = {
    val sust = gr.filter(WindPotential.EROI(_) >= eroi)
    println("---")
    println("Power captured" + "\t" + sust.map(WindPotential.powerGenerated(_).to(Terawatts)).sum + "\t" + "TW")
    println("Power installed" + "\t" + sust.map(WindPotential.powerInstalled(_).to(Megawatts)).sum + "\t" + "MW")
    println("Area" + "\t" + sust.map(g => WindPotential.suitabilityFactor(g) * g.area.toSquareKilometers).sum + "\t" + "km2")

  }

  def plotEGenVSArea(world: WorldGrid, gr: List[GridCell]) {
    val list = world.listValueVSArea(gr.map(g => (WindPotential.energyGeneratedPerYear(g).to(TerawattHours), WindPotential.suitabilityFactor(g) * g.area)))
    PlotHelper.plotXY(List((list._1, list._2, "")), xLabel = "Area [millions km2]", yLabel = "Energy Generated [TWh/year]")

  }
  def plotSpeedVSArea(world: WorldGrid, gr: List[GridCell]) {
    val vHub = world.listValueVSArea(gr.map(g => (g.windSpeedHub.value, g.area)))
    val vHubGeo = world.listValueVSArea(gr.map(g => (g.windSpeedHub.value, g.area * WindPotential.suitabilityFactor(g))))

    PlotHelper.plotXY(List((vHub._1, vHub._2, "Total"), (vHubGeo._1, vHubGeo._2, "Suitability Factor")), xLabel = "Area [millions km2]", yLabel = "Wind Speed [m/s]")
  }

  def eroi(world: WorldGrid, gr: List[GridCell], potential: EnergyGenerationPotential) {
    val all = world.listEROIVSCumulatedProduction(gr.map(g => (g, 1.0)), potential)
    val landUse = world.listEROIVSCumulatedProduction(gr.map(g => (g, potential.suitabilityFactor(g))), potential)

    PlotHelper.plotXY(List(
      (all._1, all._2, "Total"),
      (landUse._1, landUse._2, "Geographic potential")),
      xLabel = "Mean Energy Generated [EJ]",
      yLabel = "EROI") //, legend = true)
  }

  def technicalPotential(grids: List[GridCell], potential: EnergyGenerationPotential = WindPotential) {
    PlotHelper.histogram(grids.map(potential.energyGeneratedPerYear(_).to(TerawattHours)), n = 20, xLabel = "Technical potential in grid cell [TWh]", yLabel = "# Grid cells")
  }

  def areaRepartition(world: WorldGrid, potential: EnergyGenerationPotential) {

    val out_stream = new PrintStream(new java.io.FileOutputStream("suitability"))
    world.grids.map(g => out_stream.print(g.center.latitude.value.toString + "\t" + g.center.longitude.value.toString +
      "\t" + potential.suitabilityFactor(g) + "\t" + (if (g.windSpeed.toMetersPerSecond >= 3) "1.0" else "0.0") + "\n"))
    out_stream.close()

    printGrids(world.grids)
    println("----------")
    printGrids(world.onshoreGrids)
    println("----------")
    printGrids(world.offshoreGrids)

    def print(name: String, list: List[GridCell]) {
      println(name + "\t" + list.size + "\t" + world.area(list).toSquareKilometers / 1E6 + "\t" + "Millions km^2")
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
