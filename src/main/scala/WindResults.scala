

import gridData.WorldGrid
import squants.space.Degrees
import utils.Terawatts
import utils.PlotHelper
import gridData.WindPotential
import squants.energy.Watts
import java.io.PrintStream
import utils.TonOilEquivalent
import windEnergy.WindFarm
import squants.energy.Megawatts
import windEnergy.OffshoreWindFarm
import squants.space.Kilometers
import squants.space.Meters
import squants.space.SquareKilometers
import gridData.GridCell
import org.jfree.data.xy.XYSeriesCollection
import utils.TerawattHours
import utils.Exajoules
import windEnergy.WindPowerTransmission

object WindResults {
  def main(args: Array[String]): Unit = {

    val world = new WorldGrid("results/worldWind5years.txt", Degrees(0.5))

    val offshore200m = world.offshoreGrids.filter(_.elevation.toMeters >= -200)
    val grids = world.onshoreGrids ++ offshore200m

    PlotHelper.repartition(world.grids.map(WindPotential.maxPowerDensity(_).toWattsPerSquareMeter))

    println("Total" + "\t"+ world.grids.map(WindPotential.maxExergy(_)).foldLeft(Watts(0))(_ + _) / world.area(world.grids))
    println("Suitable" + "\t"+grids.map(WindPotential.maxExergy(_)).foldLeft(Watts(0))(_ + _) / world.area(grids))
    println("Onshore" + "\t"+world.onshoreGrids.map(WindPotential.maxExergy(_)).foldLeft(Watts(0))(_ + _) / world.area(world.onshoreGrids))
    println("Offshore" + "\t"+offshore200m.map(WindPotential.maxExergy(_)).foldLeft(Watts(0))(_ + _) / world.area(offshore200m))

    def eroi(gr: List[GridCell]) {
      val all = world.listEROIVSCumulatedProduction(gr.map(g => (g, 1.0)), WindPotential)
      val geo = world.listEROIVSCumulatedProduction(gr.map(g => (g, WindPotential.suitabilityFactor(g))), WindPotential)

      PlotHelper.plotXY(List((all._1, all._2, "All"), (geo._1, geo._2, "Geo")), xLabel = "Wind Energy Potential [EJ/year]", yLabel = "EROI", save = true)
    }
    def eroiExergy(gr: List[GridCell]) {
      val all = world.listEROIVSCumulatedProduction(gr.map(g => (g, WindPotential.suitabilityFactor(g))), WindPotential)
      val ex = world.listEROIVSCumulatedProductionExergy(gr.map(g => (g, WindPotential.suitabilityFactor(g))), WindPotential)
      PlotHelper.plotXY(List((all._1, all._2, "Non-constrained"), (ex._1, ex._2, "Exergy")), xLabel = "Wind Energy Potential [EJ/year]", yLabel = "EROI", save = true)
    }

    //plotEROI(offshore200m, "Offshore")
    //plotEROI(world.onshoreGrids, "Onshore")
    //plotEROI(grids, "Total")

    def plotEROI(grids: List[GridCell], title: String = "") {
      val total = world.listEROIVSCumulatedProduction(grids.map(g => (g, 1.0)), WindPotential)
      val exergy = world.listEROIVSCumulatedProductionExergy(grids.map(g => (g, 1.0)), WindPotential)

      val geo = world.listEROIVSCumulatedProduction(grids.map(g => (g, WindPotential.suitabilityFactor(g))), WindPotential)
      val geo2 = world.listEROIVSCumulatedProductionExergy(grids.map(g => (g, WindPotential.suitabilityFactor(g))), WindPotential)

      PlotHelper.plotXY(List(
        (total._1, total._2, "Total"),
        (exergy._1, exergy._2, "Total + Density"),
        (geo._1, geo._2, "Suitability Factor"),
        (geo2._1, geo2._2, "Suitability Factor + Density ")), xLabel = "Wind Energy Potential [TWh/year]", yLabel = "EROI", save = true, legend = true, title = title)

    }
  }

}