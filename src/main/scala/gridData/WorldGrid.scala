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

class WorldGrid(val name: String, val gridSize: Angle, val perMonth: Boolean = false) {

  val grids: List[GridCell] = {
    val lines = Source.fromFile(Helper.resultsPy + name).getLines().toList
    lines.map(l => if (perMonth) GridCell.applyDetails(l, this) else GridCell(l, this)).toList
  }

  def constrainedGrids(potential : RenewablePotential) = grids.filter(g => potential.suitabilityFactor(g) > 0)
  val onshoreGrids = grids.filter(_.onshore)
  def onshoreConstrainedGrids(potential : RenewablePotential) = onshoreGrids.filter(g => potential.suitabilityFactor(g) > 0)
  val offshoreGrids = grids.filter(_.offshore)
  def offshoreConstrainedGrids(potential : RenewablePotential) = offshoreGrids.filter(g => potential.suitabilityFactor(g) > 0)

  def area(gr: List[GridCell] = grids) = gr.map(_.area).foldLeft(SquareKilometers(0))(_ + _)

  def listEROIVSCumulatedProduction(gr: List[GridCell] = grids, potential: RenewablePotential) = {
    val eroiPro = ((gr.map(g => (potential.EROI(g), potential.energyGeneratedPerYear(g)))).toList :+ (0.0, Joules(0.0))).sortBy(_._1).reverse
    var tot = 0.0
    val eroiCum = eroiPro.map(i => {
      tot = tot + i._2.to(TerawattHours)
      (i._1, tot)
    })
    (eroiCum.map(_._2), eroiCum.map(_._1))
  }

  def plotEROIVSCumulatedProduction(gr: List[GridCell] = grids, potential: RenewablePotential) = {
    val eroiPro = gr.map(g => (potential.EROI(g), potential.energyGeneratedPerYear(g))).sortBy(_._1).reverse
    var tot = 0.0
    val eroiCum = eroiPro.map(i => {
      tot = tot + i._2.to(TerawattHours)
      (i._1, tot)
    })
    PlotHelper.plotXY(List((eroiCum.map(_._2), eroiCum.map(_._1), "")), xLabel = "Cumulated Annual Production [TWh]", yLabel = "EROI")
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
        "\t" + g.urbanFactor.toString + "\n")
    })
    out_stream.close()
  }
}

