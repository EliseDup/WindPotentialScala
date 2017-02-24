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
import windEnergy.SimpleWindFarm

class WorldGrid(val name: String, val gridSize: Angle) {

  val grids: List[GridCell] = Helper.getLines(name).map(GridCell(_,gridSize))
  
  // Sum v^2 * Area
  val totalDissipation = Terawatts(875.0/3)
  val totalSquareSpeedArea = grids.map(c => Math.pow(c.windSpeed.toMetersPerSecond,2) * c.area.toSquareMeters).sum
  
  def constrainedGrids(potential: EnergyGenerationPotential) = grids.filter(g => potential.suitabilityFactor(g) > 0)
  val onshoreGrids = grids.filter(g => g.onshore && g.center.latitude.toDegrees > -60)
  def onshoreConstrainedGrids(potential: EnergyGenerationPotential) = onshoreGrids.filter(g => potential.suitabilityFactor(g) > 0)
  val offshoreGrids = grids.filter(_.offshore)
  def offshoreGrids(maxDepth : Length):List[GridCell] = offshoreGrids.filter(_.waterDepth <= maxDepth)
  def offshoreConstrainedGrids(potential: EnergyGenerationPotential) = offshoreGrids.filter(g => potential.suitabilityFactor(g) > 0)

  val totalArea = Helper.area(grids)
  
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
    gr.map(g => {
      out_stream.print(g.center.longitude.value.toString + "\t" + g.center.latitude.value.toString + /* "\t" + 
          WindPotential.suitabilityFactor(g).toString + "\t" +
          g.landCovers.bareAreas.toString + "\t" +
          g.landCovers.waterBodies.toString + "\t" +
          g.landCovers.indexes.map(i => i._1*i._2).sum.toString + 
           CapacityFactorCalculation(g) + "\t"+
          g.area.toSquareKilometers.toString + "\t" + 
          (g.area.toSquareKilometers*WindPotential.suitabilityFactor(g)).toString + "\t" +
          WindPotential.diameterRotor(g).toKilometers.toString + "\t" +
          WindPotential.nominalPower(g).toMegawatts.toString + "\t" +
          SimpleWindFarm.embodiedEnergy(g).toMegawattHours.toString 
        //  + "\t" + g.country.toString*/
        "\n")

    })
    out_stream.close()
  }
 
}

