

import gridData.WorldGrid
import squants.space._
import utils._
import gridData.WindPotential
import squants.energy._
import java.io.PrintStream
import windEnergy.WindFarm
import windEnergy.OffshoreWindFarm
import gridData.GridCell
import org.jfree.data.xy.XYSeriesCollection
import windEnergy.WindPowerTransmission
import squants.radio.Irradiance
import squants.time.Hours
import squants.radio.WattsPerSquareMeter
import squants.motion.MetersPerSecond

object WindResults {
  def main(args: Array[String]): Unit = {

    val world = new WorldGrid("results/worldWind5years.txt", Degrees(0.5))

    val offshore200m = world.offshoreGrids.filter(_.elevation.toMeters >= -200)
    val grids = world.onshoreGrids ++ offshore200m

    val a = world.listValueVSArea(world.onshoreGrids.map(g => (g.windSpeed.toMetersPerSecond, g.area)).toList)
    val b = world.listValueVSArea(world.onshoreGrids.map(g => (g.windSpeed.toMetersPerSecond, g.area * WindPotential.suitabilityFactor(g))).toList)
    val c = world.listValueVSArea(offshore200m.map(g => (g.windSpeed.toMetersPerSecond, g.area)).toList)
    val d = world.listValueVSArea(offshore200m.map(g => (g.windSpeed.toMetersPerSecond, g.area * WindPotential.suitabilityFactor(g))).toList)

    PlotHelper.plotXY(List((a._1, a._2, "Onshore"), (b._1, b._2, "Onshore Constrained"),
      (c._1, c._2, "Offshore < 200 m"), (d._1, d._2, "Offshore Constrained")), xLabel = "Cumulated Area [Millions km2]", yLabel = "Mean Wind Speed [m/s]", save = true)

    /*    val tot = world.listEROIVSCumulatedProduction(grids.map(g => (g, 1.0)), WindPotential)
    val totC = world.listEROIVSCumulatedProduction(grids.map(g => (g, WindPotential.suitabilityFactor(g))), WindPotential)
    val on = world.listEROIVSCumulatedProduction(world.onshoreGrids.map(g => (g, 1.0)), WindPotential)
    val onC = world.listEROIVSCumulatedProduction(world.onshoreGrids.map(g => (g, WindPotential.suitabilityFactor(g))), WindPotential)
    val off = world.listEROIVSCumulatedProduction(offshore200m.map(g => (g, 1.0)), WindPotential)
    val offC = world.listEROIVSCumulatedProduction(offshore200m.map(g => (g, WindPotential.suitabilityFactor(g))), WindPotential)
      
   PlotHelper.plotXY(
        List((tot._1,tot._2,"Total"),(on._1,on._2,"Onshore"),(off._1,off._2,"Offshore")))
 */
    def printMinEROIValues(eroi: Double, max: Option[Irradiance]) = {
      val a = grids.filter(g => WindPotential.EROI(g, density = max) >= eroi)
      val b = world.onshoreGrids.filter(g => WindPotential.EROI(g, density = max) >= eroi)
      val c = offshore200m.filter(g => WindPotential.EROI(g, density = max) >= eroi)
      println("EROI" + "\t" + eroi)
      println(
        Math.round(a.map(g => WindPotential.energyGeneratedPerYear(g, density = max).to(Exajoules)).sum) + "\t" +
          Math.round(b.map(g => WindPotential.energyGeneratedPerYear(g, density = max).to(Exajoules)).sum) + "\t" +
          Math.round(c.map(g => WindPotential.energyGeneratedPerYear(g, density = max).to(Exajoules)).sum))
    }

    // eroiExergy(grids, List(Some(WattsPerSquareMeter(3)), Some(WattsPerSquareMeter(4)), Some(WattsPerSquareMeter(10))))
    // eroiExergy(offshore200m, List(Some(WattsPerSquareMeter(2)), Some(WattsPerSquareMeter(5)), Some(WattsPerSquareMeter(8))))

    def eroi(gr: List[GridCell], density: Option[Irradiance]) {
      val all = world.listEROIVSCumulatedProduction(gr.map(g => (g, 1.0)), WindPotential, density)
      val geo = world.listEROIVSCumulatedProduction(gr.map(g => (g, WindPotential.suitabilityFactor(g))), WindPotential, density)
      PlotHelper.plotXY(List((all._1, all._2, "All"), (geo._1, geo._2, "Geo")), xLabel = "Wind Energy Potential [EJ/year]", yLabel = "EROI", save = true)
    }

    def eroiExergy(gr: List[GridCell], density: List[Option[Irradiance]]) {
      val tot = density.map(d => {
        val x = world.listEROIVSCumulatedProduction(gr.map(g => (g, WindPotential.suitabilityFactor(g))), WindPotential, d)
        val y = world.listEROIVSCumulatedProduction(gr.map(g => (g, WindPotential.suitabilityFactor(g))), WindPotential)
        List((x._1, x._2, "Total" + d.toString), (y._1, y._2, "Exergy" + d.toString))
      }).flatten
      PlotHelper.plotXY(tot, xLabel = "Potentiel Eolien [EJ/an]", yLabel = "EROI", save = true)
    }
    /*  def plotEROI(grids: List[GridCell], title: String = "") {
      val total = world.listEROIVSCumulatedProduction(grids.map(g => (g, 1.0)), WindPotential)
      val exergy = world.listEROIVSCumulatedProductionExergy(grids.map(g => (g, 1.0)), WindPotential)

      val geo = world.listEROIVSCumulatedProduction(grids.map(g => (g, WindPotential.suitabilityFactor(g))), WindPotential)
      val geo2 = world.listEROIVSCumulatedProductionExergy(grids.map(g => (g, WindPotential.suitabilityFactor(g))), WindPotential)

      PlotHelper.plotXY(List(
        (total._1, total._2, "Total"),
        (exergy._1, exergy._2, "Total + Density"),
        (geo._1, geo._2, "Suitability Factor"),
        (geo2._1, geo2._2, "Suitability Factor + Density ")), xLabel = "Wind Energy Potential [EJ/year]", yLabel = "EROI", save = true, legend = true, title = title)

    }*/
  }
}