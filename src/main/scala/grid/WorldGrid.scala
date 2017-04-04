package grid

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
import wind_energy._

object WorldGrid {
  def apply() = new WorldGrid("../model_data/test_n", Degrees(0.75))
  def simple() = new WorldGrid("../model_data/data0_75", Degrees(0.75))
}
class WorldGrid(val name: String, val gridSize: Angle) {

  val grids: List[GridCell] = Helper.getLines(name).map(GridCell(_,gridSize))
  
  // Sum v^2 * Area
  val totalDissipation = Terawatts(875.0/3)
  val totalSquareSpeedArea = grids.map(c => Math.pow(c.wind100m.mean.toMetersPerSecond,2) * c.area.toSquareMeters).sum
  
  def dissipation(cell : GridCell) =
    if(cell.area.value > 0) totalDissipation / cell.area * ( Math.pow(cell.wind100m.mean.toMetersPerSecond, 2)* cell.area.toSquareMeters) / totalSquareSpeedArea
    else WattsPerSquareMeter(0)
    
  def constrainedGrids(potential: EnergyGenerationPotential) = grids.filter(g => potential.suitabilityFactor(g) > 0)
  val onshoreGrids = grids.filter(g => g.onshore && g.center.latitude.toDegrees > -60)
  def onshoreConstrainedGrids(potential: EnergyGenerationPotential) = onshoreGrids.filter(g => potential.suitabilityFactor(g) > 0)
  val offshoreGrids = grids.filter(_.offshore)
  def offshoreGrids(maxDepth : Length):List[GridCell] = offshoreGrids.filter(_.waterDepth <= maxDepth)
  def offshoreConstrainedGrids(potential: EnergyGenerationPotential) = offshoreGrids.filter(g => potential.suitabilityFactor(g) > 0)

  val totalArea = Helper.area(grids)
  
  def country(c: Country) = grids.filter(_.country.equals(c))
  def country(c: String) = grids.filter(_.country.name.contains(c))
  def countries(c: List[String]) = grids.filter(g => c.contains(g.country.name))
  
  def europe = grids.filter(g => g.center.longitude.toDegrees >= -18 && g.center.longitude.toDegrees <= 30.75 && g.center.latitude.toDegrees >= 27.75 && g.center.latitude.toDegrees <= 69.75)

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
      out_stream.print(g.center.latitude.value.toString + "\t" + g.center.longitude.value.toString +  "\t" +
        
         g.wind100m.c.value.toString + "\t" +g.wind100m.k.toString +
         "\t" + g.area.toSquareKilometers.toString + "\t" + g.suitableArea.toSquareKilometers.toString+
         "\t" + WindPotential.energyInputs(Megawatts(1), Megawatts(1)*0.2*Hours(365*24*25), g).toMegawattHours.toString +
      "\n")

    })
    out_stream.close()
  }
  /**
   *  lats = data[:, 0]; lon = data[:, 1]
   *  cfs = data[:, 2]; totalArea = data[:, 3]; suitableArea = data[:, 4];
   *  diam = data[:, 5]; rPower = data[:, 6]
   *  embodiedE = data[:, 7]
   */
  def optimizationInputs(name : String, gr: List[GridCell] = grids) {
    val out_stream = new PrintStream(new java.io.FileOutputStream(name))
    gr.map(g => {
      out_stream.print(g.center.latitude.value.toString + "\t" + g.center.longitude.value.toString +
           "\t"+ CapacityFactorCalculation(g).toString + "\t" + (if(g.waterDepth.toMeters > 1000) 0.0 else g.area.toSquareKilometers.toString) + 
           "\t" + g.suitableArea.toSquareKilometers.toString + "\t" +
          ( if(g.onshore) WindFarmEnergyInputs.onshoreEnergyInputs(Megawatts(1),Megawatts(1)*25*Hours(365*24)*CapacityFactorCalculation(g), g.distanceToCoast).to(MegawattHours) 
              else WindFarmEnergyInputs.offshoreEnergyInputs(Megawatts(1), Megawatts(1)*25*Hours(365*24)*CapacityFactorCalculation(g), g.waterDepth, g.distanceToCoast).to(MegawattHours)) +
           "\n")
    })
  out_stream.close()
  }
 
}

