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
import sun.awt.X11.XLabelPeer

object WindPotentialSimulation {

  def main(args: Array[String]): Unit = {
    val world = new WorldGrid("results/worldGridIrradiance.txt", Degrees(0.5))

    println("Forest " + world.area(world.grids.filter(_.lc.isForest)) / 1E6)
    println("Bare " + world.area(world.grids.filter(_.lc.isBarrenArea)) / 1E6)
    println("Shrubland " + world.area(world.grids.filter(_.lc.isShrubLand)) / 1E6)
    println("Savannah " + world.area(world.grids.filter(_.lc.isSavannah)) / 1E6)
    println("Grassland " + world.area(world.grids.filter(_.lc.isGrassLand)) / 1E6)

    println("Agricultural " + world.area(world.grids.filter(_.lc.isAgricultural)) / 1E6)
    println("Flooded " + world.area(world.grids.filter(_.lc.isFloodedArea)) / 1E6)
    println("Urban " + world.area(world.grids.filter(_.lc.isUrban)) / 1E6)

    val solarCells = world.grids.filter(SolarPotential.suitabilityFactor(_) > 0)
    val area = solarCells.map(c => c.area.toSquareKilometers * SolarPotential.suitabilityFactor(c)).sum
    println(area / 1E6 + "millions km2")

    // solarPerMonth(world.grids)
    //    plotEGenVSArea(world, solarCells, SolarPotential)
    eroi(world, solarCells, SolarPotential)

    /*
  val sortedCells = constrained.sortBy(WindPotential.EROI(_)).reverse
  val res = cumulated(sortedCells.map(c => (WindPotential.energyGeneratedPerYear(c).to(Exajoules), WindPotential.nTurbines(c) / 1E6)))    
  PlotHelper.plotXY(List((res.map(_._2), res.map(_._1), "")), yLabel = "Energy Generated [EJ/year]", xLabel = "Millions Turbines")
*/
  }
  def barPlotMaterialUse(grids: List[GridCell], energy: List[Int],
    materials: List[(Material, Mass)] = List((Steel, Tonnes(1500E6)), (Aluminium, Tonnes(44.4E6)), (Copper, Tonnes(34E6)))) {
    val dataset = new DefaultCategoryDataset()
    val res = energy.map(energ => (materials.map(mat => {
      val cellIt = grids.toIterator
      val f = cellIt.next()
      println("first" + f.center + "\t" + WindPotential.EROI(f))
      var e = Exajoules(0)
      var m = Tonnes(0)
      while (e < Exajoules(energ) && cellIt.hasNext) {
        val next = cellIt.next()
        e += WindPotential.energyGeneratedPerYear(next)
        m += WindPotential.weight(next, mat._1)
      }
      println(energ + "\t" + mat._1.name + "\t" + e + "\t " + m)
      dataset.addValue(m / mat._2,
        mat._1.name, energ.toString + "EJ/year")
    }))).flatten

    PlotHelper.barChart(dataset, yLabel = "# 2012 Production")

  }
  def plotMaterialUse(grids: List[GridCell], materials: List[(Material, Mass)]) {
    val sortedCells = grids.sortBy(WindPotential.EROI(_)).reverse
    val res =
      materials.map(mat => {
        val x = cumulated(sortedCells.map(c => (WindPotential.energyGeneratedPerYear(c).to(Exajoules), WindPotential.weight(c, mat._1) / mat._2)))
        (x.map(_._2), x.map(_._1), mat._1.name)
      })
    PlotHelper.plotXY(res, yLabel = "Energy Generated [EJ/year]", xLabel = "# 2012 production", legend = true)
  }

  def cumulated(values: List[(Double, Double)]) = {
    var cum1 = 0.0; var cum2 = 0.0;
    values.map(c => {
      cum1 += c._1; cum2 += c._2;
      (cum1, cum2)
    })
  }

  def printMinEROIValues(gr: List[GridCell], eroi: Double) = {
    val sust = gr.filter(WindPotential.EROI(_) >= eroi)
    println("---")
    println("Power captured" + "\t" + sust.map(WindPotential.powerGenerated(_).to(Terawatts)).sum + "\t" + "TW")
    println("Power installed" + "\t" + sust.map(WindPotential.powerInstalled(_).to(Megawatts)).sum + "\t" + "MW")
    println("Energy generated" + "\t" + sust.map(WindPotential.energyGeneratedPerYear(_).to(Exajoules)).sum + "\t" + "EJ/year")
    println("# Turbines" + "\t" + sust.map(WindPotential.nTurbines(_)).sum / 1E6)
    println("Area" + "\t" + sust.map(g => WindPotential.suitabilityFactor(g) * g.area.toSquareKilometers).sum / 1E6 + "\t" + "millions km2")

  }

  def plotEGenVSArea(world: WorldGrid, gr: List[GridCell], potential: EnergyGenerationPotential) {
    val list = world.listValueVSArea(gr.map(g => (potential.energyGeneratedPerYear(g).to(Exajoules), WindPotential.suitabilityFactor(g) * g.area)))
    PlotHelper.plotXY(List((list._1, list._2, "")), xLabel = "Area [millions km2]", yLabel = "Energy Generated [EJ/year]")

  }
  def plotSpeedVSArea(world: WorldGrid, gr: List[GridCell]) {
    val vHub = world.listValueVSArea(gr.map(g => (g.windSpeedHub.value, g.area)))
    val vHubGeo = world.listValueVSArea(gr.map(g => (g.windSpeedHub.value, g.area * WindPotential.suitabilityFactor(g))))

    PlotHelper.plotXY(List((vHub._1, vHub._2, "Total"), (vHubGeo._1, vHubGeo._2, "Suitability Factor")), xLabel = "Area [millions km2]", yLabel = "Wind Speed [m/s]")
  }

  def eroi(world: WorldGrid, gr: List[GridCell], potential: EnergyGenerationPotential) {
    //val all = world.listEROIVSCumulatedProduction(gr.map(g => (g, 1.0)), potential)
    val landUse = world.listEROIVSCumulatedProduction(gr.map(g => (g, potential.suitabilityFactor(g))), potential)

    PlotHelper.plotXY(List(
      //(all._1, all._2, "Total"),
      (landUse._1, landUse._2, "Geographic potential")),
      xLabel = "Energy Generated [EJ/year]",
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
      //  print("openArea", grids.filter(_.lc.isOpenArea))
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

  def solarPerMonth(grids: List[GridCell]) {
    def energyGenerated(month: Int) = grids.map(g => SolarPotential.energyGeneratedPerMonth(g, month).to(Exajoules)).sum
    val month = (0 until 12).toList
    PlotHelper.plotXY(month.map(_.toDouble), month.map(energyGenerated(_)))

  }
}
