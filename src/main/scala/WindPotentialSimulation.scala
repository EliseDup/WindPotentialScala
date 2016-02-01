

import construction.Materials
import landCover.GridData
import squants.space.Degrees
import operation.WindTurbine2MW
import squants.space.SquareKilometers
import operation.WindTurbineWithPower
import utils.PlotHelper
import squants.energy.MegawattHours
import squants.energy.KilowattHours
import construction.WindTurbineComponents
import construction.WindTurbineComponents
import operation.WindTurbineWithPower
import squants.space.Kilometers
import construction.Transmission

object WindPotentialSimulation {
  def main(args: Array[String]): Unit = {

    val name = "world5years"
    val wind = new GridData("results/" + name, Degrees(0.25), new WindTurbineWithPower("2MW"),  new WindTurbineWithPower("5MWoffshore"))
    // wind.plotEROIVSCumulatedProduction()

  }
}