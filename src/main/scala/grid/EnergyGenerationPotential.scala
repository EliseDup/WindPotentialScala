package grid

import squants.radio._
import squants.energy._
import squants.time._
import squants.motion._
import squants.space._
import utils._
import wind_energy._
import org.jfree.data.time.Year
import org.apache.commons.math3.special.Gamma
import construction.Material
import utils.TerawattHours

trait EnergyGenerationPotential {
  val lifeTimeYears: Double
  def lifeTime = lifeTimeYears * Hours(365 * 24)

  // [0.0 -> 1.0] a multiplicating factor for the available area, 
  // indicating the part of the grid cell that can be used for the renewable technology
  def suitabilityFactor(cell: GridCell): Double = landUseFactor(cell)

  def suitableArea(cell: GridCell) = cell.area * suitabilityFactor(cell)
  // % of area of the cell that is available for this technology
  def landUseFactor(cell: GridCell): Double

  // Power density = theoritical energy contained in the flux (wind, solar radiation, ...)
  def powerDensity(cell: GridCell): Irradiance

  def power(cell: GridCell, eroi_min: Double, suitable: Boolean): Power

  def energyPerYear(cell: GridCell, eroi_min: Double, suitable: Boolean): Energy = power(cell, eroi_min, suitable) * Hours(24 * 365)
  def energyLifeTime(cell: GridCell, eroi_min: Double, suitable: Boolean): Energy = lifeTimeYears * energyPerYear(cell, eroi_min, suitable)

  def netEnergyPerYear(cell: GridCell, eroi_min: Double, suitable: Boolean): Energy
  def netEnergyLifeTime(cell: GridCell, eroi_min: Double, suitable: Boolean): Energy = lifeTimeYears * netEnergyPerYear(cell, eroi_min, suitable)

  def eroi(cell: GridCell, eroi_min: Double, suitable: Boolean): Double

  def potential(eroi_min: Double, suitable: Boolean = true, grids: List[GridCell]): Energy = grids.map(g => energyPerYear(g, eroi_min, suitable)).foldLeft(Joules(0))(_ + _)
  def netPotential(eroi_min: Double, suitable: Boolean = true, grids: List[GridCell]): Energy = grids.map(g => netEnergyPerYear(g, eroi_min, suitable)).foldLeft(Joules(0))(_ + _)
  // Results for PLOTS
  def plotPotential(erois: List[Double], suitable: Boolean = true, grids: List[GridCell]) = PlotHelper.plotXY(List(potential_eroi(erois, suitable, grids)), xLabel = "Potential [EJ/year]", yLabel = "EROImin")
  def plotEROIFunction(cells: List[GridCell], e: Double, suitable: Boolean = true) = PlotHelper.plotXY(List(eroiFunction(cells, e, suitable)), xLabel = "Potential [EJ/year]", yLabel = "EROI")

  def potential_eroi(erois: List[Double], suitable: Boolean = true, grids: List[GridCell], title: String = ""): (List[Double], List[Double], String) =
    (erois.map(e => potential(e, true, grids).to(Exajoules)), erois, title)

  def eroiFunction(cells: List[GridCell], e: Double, suitable: Boolean = true, name: String = "") = {
    val res = Helper.listValueVSCumulated(cells.filter(g => energyPerYear(g, e, suitable).value > 0 && eroi(g, e, suitable) >= 1).map(g => (eroi(g, e, suitable), energyPerYear(g, e, suitable).to(Exajoules))))
    (res._1, res._2, name)
  }
  def eroiFunctionNet(cells: List[GridCell], e: Double, suitable: Boolean = true, name: String = "") = {
    val res = Helper.listValueVSCumulated(cells.filter(g => netEnergyPerYear(g, e, suitable).value > 0 && eroi(g, e, suitable) >= 1).map(g => (eroi(g, e, suitable), netEnergyPerYear(g, e, suitable).to(Exajoules))))
    (res._1, res._2, name)
  }
}