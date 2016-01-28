

import construction.Materials
import construction.WindFarmComponents
import landCover.GridData
import squants.space.Degrees
import operation.WindTurbine2MW
import squants.space.SquareKilometers
import operation.WindTurbineWithPower
import utils.PlotHelper
import squants.energy.MegawattHours
import squants.energy.KilowattHours
import construction.DefaultWindTurbineComponents
import construction.WindTurbineComponents

object WindPotentialSimulation {
  def main(args: Array[String]): Unit = {
    
    val farm = new WindFarmComponents()

    val offshore = new WindTurbineWithPower("5MWoffshore")
    val onshore = new WindTurbine2MW
 
    println(offshore.specs)
    println(onshore.specs)
    
    val name = "world5years"

    val wind = new GridData("results/" + name, Degrees(0.25), onshore, offshore, farm, farm)

    wind.plotEROIVSCumulatedProduction(wind.offshoreGrids())

  }
}