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
    
   println( WindFarm.embodiedEnergy(Megawatts(1)).toGigajoules )
   println( OffshoreWindFarm.embodiedEnergy(Megawatts(1), Meters(50), Kilometers(10) ).toGigajoules)
    
    val world = new WorldGrid("results/worldGridWind.txt", Degrees(0.5))
    eroi(world,world.onshoreGrids, WindPotential)
    world.onshoreConstrainedGrids(WindPotential).map(g =>
      if (WindPotential.powerGenerated(g) > (WindPotential.powerDensityAtHub(g) * WindPotential.areaRotor(g) * WindPotential.nTurbines(g))) {
        println(g)
        println(WindPotential.powerGenerated(g) + "\t" + WindPotential.capacityFactor(g) + "\t" +g.windSpeedHub)
        println(WindPotential.powerDensity(g) + "\t"+  WindPotential.areaRotor(g) + "\t"+ WindPotential.nTurbines(g))
        println()
      })
   
      PlotHelper.plotXY(
      world.onshoreConstrainedGrids(WindPotential).map(g => WindPotential.capacityFactor(g)),
      world.onshoreConstrainedGrids(WindPotential).map(g => (WindPotential.powerGenerated(g) / (WindPotential.powerDensityAtHub(g) * WindPotential.areaRotor(g) * WindPotential.nTurbines(g)))))

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
     val all = world.listEROIVSCumulatedProduction(gr.map(g => (g, 1.0)), potential)
    val landUse = world.listEROIVSCumulatedProduction(gr.map(g => (g, potential.suitabilityFactor(g))), potential)
    //  val allEx = world.listEROIVSCumulatedProductionExergy(gr.map(g => (g, 1.0)), potential)
    // val landUseEx = world.listEROIVSCumulatedProductionExergy(gr.map(g => (g, potential.suitabilityFactor(g))), potential)

    //val wind = world.listEROIVSCumulatedProduction(gr.map(g => (g, potential.suitabilityFactor(g) * (if(g.windSpeed.toMetersPerSecond < 4) 0.0 else 1.0))), potential)
    PlotHelper.plotXY(List(
      (all._1, all._2, "Total"),
      (landUse._1, landUse._2, "Geographic")),
      // (allEx._1, allEx._2, "Total + Top-down"),
      //(landUseEx._1, landUseEx._2, "Geographic + Exergy")),
      //(wind._1, wind._2, "Wind Regime potential")),
      xLabel = "Energy Generated [EJ/year]",
      yLabel = "EROI", legend = true)
  }

  def plotSolarPotentialRepartition(world: WorldGrid, grids: List[GridCell]) {
    def listIrradianceVSArea(grids: List[GridCell], name: String) = {
      val res = world.listValueVSArea(grids.map(g => (g.irradiance.mean.toWattsPerSquareMeter, g.area * SolarPotential.suitabilityFactor(g))))
      (res._1, res._2, name)
    }
    PlotHelper.plotXY(List(listIrradianceVSArea(grids.filter(g => g.lc.croplands || g.lc.mosaicVegetationCropland), "Croplands"),
      listIrradianceVSArea(grids.filter(_.lc.bareAreas), "Bare areas"),
      listIrradianceVSArea(grids.filter(g => g.lc.grassland || g.lc.mosaicGrasslandForestShrubland), "Grassland"),
      listIrradianceVSArea(grids.filter(_.lc.shrubland), "Shrubland"),
      listIrradianceVSArea(grids.filter(_.lc.sparseVegetation), "Sparse Vegetation")), xLabel = "Suitable Area [million km2]",
      yLabel = "Irradiance [W/m2]", legend = true)

  }
  def technicalPotential(grids: List[GridCell], potential: EnergyGenerationPotential = WindPotential) {
    PlotHelper.histogram(grids.map(potential.energyGeneratedPerYear(_).to(TerawattHours)), n = 20, xLabel = "Technical potential in grid cell [TWh]", yLabel = "# Grid cells")
  }

  def areaRepartition(world: WorldGrid, potential: EnergyGenerationPotential) {

    printGrids(world.grids)
    println("----------")
    printGrids(world.onshoreGrids)
    println("----------")
    printGrids(world.offshoreGrids)

  }
  def printGrids(grids: List[GridCell]) {
    def print(name: String, list: List[GridCell]) {
      println(name + "\t" + list.size + "\t" + list.map(_.area).foldLeft(SquareKilometers(0))(_ + _).toSquareKilometers / 1E6 + "\t" + "Millions km2")
    }
    print("Total", grids)
    print("protected", grids.filter(_.protectedArea))

    print("WaterBodies", grids.filter(_.lc.waterBodies))
    print("Ice", grids.filter(_.lc.ice))
    print("BareAreas", grids.filter(_.lc.bareAreas))
    print("Grassland", grids.filter(_.lc.grassland))
    print("SparseVegetation", grids.filter(_.lc.sparseVegetation))
    print("Croplands", grids.filter(_.lc.croplands))
    print("Shrubland", grids.filter(_.lc.shrubland))
    print("Wetlands", grids.filter(_.lc.wetlands))
    print("MosaicNaturalCropland", grids.filter(_.lc.mosaicVegetationCropland))
    print("Flooded", grids.filter(_.lc.floodedAreas))
    print("MosaicGrasslandForest", grids.filter(_.lc.mosaicGrasslandForestShrubland))
    print("UrbanAreas", grids.filter(_.lc.urbanAreas))
    print("Forests", grids.filter(_.lc.forests))
    print("NoData", grids.filter(_.lc.noData))
  }
  def solarPerMonth(grids: List[GridCell]) {
    def energyGenerated(month: Int) = grids.map(g => SolarPotential.energyGeneratedPerMonth(g, month).to(Exajoules)).sum
    val month = (0 until 12).toList
    PlotHelper.plotXY(month.map(_.toDouble), month.map(energyGenerated(_)))

  }
}
