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

object WindPotentialSimulation {

  def main(args: Array[String]): Unit = {

    val potential = WindPotential
    val wind = new WorldGrid("worldData.txt", Degrees(0.5))

    val onshore = wind.onshoreGrids
    val offshore = wind.offshoreGrids

    eroi(onshore)
    eroi(offshore.filter(_.elevation.toMeters >= -200))

    def eroi(gr: List[GridCell]) {

      val all = wind.listEROIVSCumulatedPower(gr.map(g => (g, 1.0)), potential)
      val landUse = wind.listEROIVSCumulatedPower(gr.map(g => (g, potential.geographicFactor(g))), potential)
      val windRegime = wind.listEROIVSCumulatedPower(gr.map(g => (g, potential.geographicFactor(g) * potential.windRegimeFactor(g))), potential)

      PlotHelper.plotXY(List(
        (all._1, all._2, "Total"),
        (landUse._1, landUse._2, "Geographic potential"),
        (windRegime._1, windRegime._2, "Wind Regime Restrictions")),

        xLabel = "Mean power captured [TW]",
        yLabel = "EROI", legend = true)
    }
  }

  def technicalPotential(grids: List[GridCell], potential: EnergyGenerationPotential) {
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