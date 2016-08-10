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

  val grids: List[GridCell] = Helper.getLines(name).map(l => GridCell(l, gridSize)).toList

  def constrainedGrids(potential: EnergyGenerationPotential) = grids.filter(g => potential.suitabilityFactor(g) > 0)
  val onshoreGrids = grids.filter(g => g.onshore && g.center.latitude.toDegrees > -60)
  def onshoreConstrainedGrids(potential: EnergyGenerationPotential) = onshoreGrids.filter(g => potential.suitabilityFactor(g) > 0)
  val offshoreGrids = grids.filter(_.offshore)
  def offshoreConstrainedGrids(potential: EnergyGenerationPotential) = offshoreGrids.filter(g => potential.suitabilityFactor(g) > 0)

  val totalArea = area()
  def area(gr: List[DefaultGridCell] = grids) = gr.map(_.area).foldLeft(SquareKilometers(0))(_ + _)
  def suitableArea(gr: List[GridCell] = grids, potential: EnergyGenerationPotential) = gr.map(g => g.area * WindPotential.suitabilityFactor(g)).foldLeft(SquareKilometers(0))(_ + _)

  def country(c: Country) = grids.filter(_.country.equals(c))

  def listValueVSArea(gr: List[(Double, Area)]) = {
    listValueVSCumulated(gr.map(g => (g._1, g._2.to(SquareKilometers) / (1E6))))
  }

  def listEROIVSCumulatedProduction(gr: List[(GridCell, Double)], potential: EnergyGenerationPotential): (List[Double], List[Double]) = {
    listValueVSCumulated(gr.map(g => (potential.EROI(g._1, g._2), potential.energyGeneratedPerYear(g._1, g._2).to(TerawattHours))))
  }
  def listEROIVSCumulatedProductionExergy(gr: List[(GridCell, Double)], potential: EnergyGenerationPotential): (List[Double], List[Double]) = {
    listValueVSCumulated(gr.map(g => (potential.EROI(g._1, g._2), potential.energyGeneratedPerYearExergy(g._1, g._2).to(TerawattHours))))
  }
  def listEROIVSCumulatedPower(gr: List[(GridCell, Double)], potential: EnergyGenerationPotential): (List[Double], List[Double]) = {
    listValueVSCumulated(gr.map(g => (potential.EROI(g._1, g._2), potential.powerGenerated(g._1, g._2).to(Terawatts))))
  }
  def listEROIVSCumulatedPowerExergy(gr: List[(GridCell, Double)], potential: EnergyGenerationPotential): (List[Double], List[Double]) = {
    listValueVSCumulated(gr.map(g => (potential.EROI(g._1, g._2), potential.powerGeneratedExergy(g._1, g._2).to(Terawatts))))
  }
  def listEnergyGeneratedPerYearVSCumulatedProduction(gr: List[(GridCell, Double)], potential: EnergyGenerationPotential) = {
    val prod = gr.map(g => (potential.energyGeneratedPerYear(g._1, g._2)).to(TerawattHours))
    listValueVSCumulated(prod.map(i => (i, i)))
  }

  def listValueVSCumulated(values: List[(Double, Double)]): (List[Double], List[Double]) = {
    val sorted = values.sortBy(_._1).reverse
    (sorted.map(_._2).scanLeft(0.0)(_ + _), sorted.map(_._1) :+ 0.0)
  }
  
  def plotEROIVSCumulatedProduction(gr: List[(GridCell, Double)], potential: EnergyGenerationPotential) = {
    val list = listEROIVSCumulatedProduction(gr, potential)
    PlotHelper.plotXY(List((list._1, list._2, "")), xLabel = "Cumulated Annual Production [TWh]", yLabel = "EROI")
  }

  def writeGrid(name: String, gr: List[GridCell] = grids) {
    val out_stream = new PrintStream(new java.io.FileOutputStream(name))
    gr.map(g => {
      out_stream.print(g.center.latitude.value.toString + "\t" + g.center.longitude.value.toString +
        "\t" + WindPotential.energyGeneratedPerYear(g).to(TerawattHours).toString + 
        "\t" + g.windSpeedHub.toMetersPerSecond.toString +
        "\t" + WindPotential.EROI(g).toString +
        "\t" + WindPotential.capacityFactor(g).toString +"\n")
    })
    out_stream.close()
  }
  def writeTest() {
    val index = Helper.getLines("iewa_2030.txt").zipWithIndex.map(i => (i._1(0) -> i._2)).toMap

    val out_stream = new PrintStream(new java.io.FileOutputStream("country"))
    grids.map(g => {
      out_stream.print(g.center.latitude.value.toString + "\t" + g.center.longitude.value.toString +
        "\t" + (if (g.country.name == "NA" || !index.contains(g.country.name)) "-1.0" else index(g.country.name).toDouble.toString) + "\n")

    })
    out_stream.close()
  }
}

