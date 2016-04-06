package gridData

import scala.io.Source
import utils.Helper
import java.io.FileWriter
import au.com.bytecode.opencsv.CSVWriter
import utils.GeoPoint
import java.io.PrintStream
import squants.motion._
import squants.mass._
import squants.radio._
import squants.time._
import squants.energy._
import squants.space._
import construction._
import utils._

class WorldGrid(val name: String, val gridSize: Angle) {

  val grids: List[GridCell] = {
    val lines = Source.fromFile(name).getLines().toList
    lines.map(l => GridCell(l, this)).toList
  }

  def constrainedGrids(potential: EnergyGenerationPotential) = grids.filter(g => potential.suitabilityFactor(g) > 0)
  val onshoreGrids = grids.filter(_.onshore)
  def onshoreConstrainedGrids(potential: EnergyGenerationPotential) = onshoreGrids.filter(g => potential.suitabilityFactor(g) > 0)
  val offshoreGrids = grids.filter(_.offshore)
  def offshoreConstrainedGrids(potential: EnergyGenerationPotential) = offshoreGrids.filter(g => potential.suitabilityFactor(g) > 0)

  def area(gr: List[GridCell] = grids) = gr.map(_.area).foldLeft(SquareKilometers(0))(_ + _)

  def listEROIVSCumulatedProduction(gr: List[(GridCell, Double)], potential: EnergyGenerationPotential): (List[Double], List[Double]) = {
    val eroiPro = gr.map(g => (potential.EROI(g._1), potential.energyGeneratedPerYear(g._1, g._2))).sortBy(_._1).reverse
    var tot = 0.0
    val eroiCum = eroiPro.map(i => {
      tot = tot + i._2.to(TerawattHours)
      (i._1, tot)
    })
    (eroiCum.map(_._2), eroiCum.map(_._1))
  }
  def listEROIVSCumulatedPower(gr: List[(GridCell, Double)], potential: EnergyGenerationPotential): (List[Double], List[Double]) = {
    val eroiPro = gr.map(g => (potential.EROI(g._1), potential.powerInstalled(g._1, g._2) * potential.capacityFactor(g._1))).sortBy(_._1).reverse
    var tot = 0.0
    val eroiCum = eroiPro.map(i => {
      tot = tot + i._2.to(Terawatts)
      (i._1, tot)
    })
    (eroiCum.map(_._2), eroiCum.map(_._1))
  }
  def listEnergyGeneratedPerYearVSCumulatedProduction(gr: List[(GridCell, Double)], potential: EnergyGenerationPotential) = {
    val eroiPro = gr.map(g => (potential.energyGeneratedPerYear(g._1, g._2))).sortBy(_.toJoules).reverse
    var tot = 0.0
    val eroiCum = eroiPro.map(i => {
      val e = i.to(TerawattHours)
      tot = tot + e
      (e, tot)
    })
    (eroiCum.map(_._2), eroiCum.map(_._1))
  }

  def plotEROIVSCumulatedProduction(gr: List[(GridCell, Double)], potential: EnergyGenerationPotential) = {
    val list = listEROIVSCumulatedProduction(gr, potential)
    PlotHelper.plotXY(List((list._1, list._2, "")), xLabel = "Cumulated Annual Production [TWh]", yLabel = "EROI")
  }

  def writeGrid(name: String, gr: List[GridCell] = grids) {
    val out_stream = new PrintStream(new java.io.FileOutputStream(name))
    gr.map(g => {
      out_stream.print(g.center.latitude.value.toString + "\t" + g.center.longitude.value.toString +
        "\t" + g.windSpeed.value.toString +
        "\t" + g.irradiance.value.toString +
        "\t" + g.lc.code.toDouble.toString +
        "\t" + g.elevation.value.toString +
        "\t" + g.distanceToCoast.value.toString +
        "\t" + g.urbanFactor.toString +
        "\t" + WindPotential.geographicFactor(g) +
        "\t" + WindPotential.windRegimeFactor(g)*WindPotential.geographicFactor(g) +
        "\t" + WindPotential.bioReserveFactor(g) + 
        "\t" + WindPotential.energyGeneratedPerYear(g).to(TerawattHours) + "\n")
    })
    out_stream.close()
  }

  def gridSuperClasses = {
    List(("Water Bodies", area(grids.filter(g => g.lc.classes.waterBodies.contains(g.lc.code)))),
      ("Forests", area(grids.filter(g => g.lc.classes.forest.contains(g.lc.code)))),
      ("Open Areas", area(grids.filter(g => g.lc.classes.openAreas.contains(g.lc.code)))),
      ("Flooded Areas", area(grids.filter(g => g.lc.classes.floodedAreas.contains(g.lc.code)))),
      ("Agricultural Areas", area(grids.filter(g => g.lc.classes.agriculturalAreas.contains(g.lc.code)))),
      ("Urban Areas", area(grids.filter(g => g.lc.classes.urbanAreas.contains(g.lc.code)))),
      ("Snow And Ice", area(grids.filter(g => g.lc.classes.ice.contains(g.lc.code)))))
  }

}

