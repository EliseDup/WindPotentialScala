

import utils.Helper
import java.io.PrintStream
import utils.PlotHelper
import squants.space.Degrees
import grid.WorldGrid
import wind_energy.CapacityFactorCalculation
import wind_energy.WindPotential
import squants.space.SquareKilometers
import squants.radio.WattsPerSquareMeter
import utils.Exajoules
import squants.energy._
import wind_energy.WindPowerTransmission
import grid.GlobCoverClasses
import java.io.FileOutputStream
import squants.radio.Irradiance
import utils._
import utils.TerawattHours
import utils.Terawatts
import squants.time.Hours
import squants.space.Area
import grid.GridCell
import squants.motion.Velocity
import squants.motion.MetersPerSecond
import utils._
import squants.space.Meters
import org.apache.commons.math3.special.Gamma
import wind_energy.WakeEffect
import wind_energy.WindProfile
import wind_energy.WindFarmEnergyInputs
import utils.PetawattHours
import wind_energy.NormalizedPowerCurve
import squants.space.Kilometers
import wind_energy.Weibull
import squants.space.Radians
import solar_energy.CSPParabolic
import solar_energy.CSPParabolicStorage12h
import solar_energy.CSPTowerStorage12h
import wind_solar.Grid

object Test {

  import Helper._
  import PlotHelper._
  def main(args: Array[String]): Unit = {
  
    val g = Grid()
    val countries = List("United States", "India", "Spain")
    val csps = List(CSPParabolic, CSPParabolicStorage12h, CSPTowerStorage12h)
    csps.map(csp => print(csp.name + "\t"))
     println()
    csps.map(csp => print(csp.embodiedEnergyArea(Megawatts(1), csp.default_aperture_area/1000.0).to(Megajoules) + "\t" ))
        println()
    countries.map(c => {
      val cells = g.country(c)
      print(c + "\t")
      csps.map(csp => {
        val cell = cells.filter(c => csp.suitabilityFactor(c) > 0)
        print(cell.map(k => (csp.capacityFactor(k, 1.0) * k.area.toSquareKilometers)).sum / area_grid(cell).toSquareKilometers + "\t")
      })
      println()
    })
  }

}