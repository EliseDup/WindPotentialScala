

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
import construction.WindTurbineComponents
import construction.WindTurbineComponents
import operation.WindTurbineWithPower

object WindPotentialSimulation {
  def main(args: Array[String]): Unit = {

    val pow = List("850KW", "1.65MW", "1500kW", "2MW", "3MW")
    for (p <- pow) println(print(new WindTurbineWithPower(p).specs))
    def print(comp: WindTurbineComponents) {
      val w = comp.weight - comp.foundation.weight
      println("Tower " + comp.tower.weight / w)
      println("Rotor " + comp.rotor.weight / w)
      println("Nacelle " + comp.nacelle.weight / w)

    }

    // val name = "world5years"
    // val wind = new GridData("results/" + name, Degrees(0.25), onshore, offshore, farm, farm)
    // wind.plotEROIVSCumulatedProduction(wind.offshoreGrids())

  }
}