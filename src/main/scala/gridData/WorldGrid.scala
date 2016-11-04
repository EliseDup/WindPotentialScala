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
import windEnergy.CapacityFactorCalculation

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

  def dissipation(gr: List[GridCell] = grids) = gr.map(g => g.kineticEnergyDissipation * g.area).foldLeft(Watts(0))(_ + _)

  def country(c: Country) = grids.filter(_.country.equals(c))

  def listValueVSArea(gr: List[(Double, Area)]) = listValueVSCumulated(gr.map(g => (g._1, g._2.to(SquareKilometers) / (1E6))))

  def listEROIVSCumulatedProduction(gr: List[(GridCell, Double)], potential: EnergyGenerationPotential, density: Option[Irradiance] = None): (List[Double], List[Double]) = {
    listValueVSCumulated(gr.map(g => (potential.EROI(g._1, Some(g._2), density), potential.energyGeneratedPerYear(g._1, Some(g._2), density).to(Exajoules))))
  }
  def listEROIVSCumulatedPower(gr: List[(GridCell, Double)], potential: EnergyGenerationPotential, density: Option[Irradiance] = None): (List[Double], List[Double]) = {
    listValueVSCumulated(gr.map(g => (potential.EROI(g._1, Some(g._2), density), potential.powerGenerated(g._1, Some(g._2), density).to(Terawatts))))
  }
  def listEnergyGeneratedPerYearVSCumulatedProduction(gr: List[(GridCell, Double)], potential: EnergyGenerationPotential) = {
    val prod = gr.map(g => (potential.energyGeneratedPerYear(g._1, Some(g._2))).to(TerawattHours))
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

  /**
   * Simple function to write values in a txt file to be able to plot them afterwards
   * ( Function ../WindPotentialPy/Plot.py )
   * ! All should be double casted to String, otherwise it will not work
   */
  def writeGrid(name: String, gr: List[GridCell] = grids) {
    val out_stream = new PrintStream(new java.io.FileOutputStream(name))
    gr.filter(_.center.latitude.toDegrees > -60).map(g => {
      out_stream.print(g.center.latitude.value.toString + "\t" + g.center.longitude.value.toString + "\t" +
          WindPotential.suitabilityFactor(g) + "\t"+
          CapacityFactorCalculation.cubic(g).toString + "\t" +
          WindPotential.EROI1MW(g) + "\t" + 
        g.windSpeed.toMetersPerSecond.toString + "\t" +
        g.windSpeedHub.toMetersPerSecond.toString + "\t" +
        g.weibull.cHub.toMetersPerSecond.toString + "\t"+
        g.weibull.kHub.toString + "\t"+ 
        WindPotential.capacityDensity(g).toWattsPerSquareMeter.toString + "\t" +
        (if(g.protectedArea) "1.0" else "0.0") + "\t" +
        
        g.kineticEnergyDissipation.toWattsPerSquareMeter.toString +
        "\n")

    })
    out_stream.close()
  }
 
}

