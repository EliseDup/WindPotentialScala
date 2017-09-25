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
import solar_energy.SolarPower._
object WorldGrid {

  def apply(name: String) = new WorldGrid(name, Degrees(0.75))
  def apply() = new WorldGrid("../Model_data/Wind_Optimization/1_20_by0_5", Degrees(0.75))
  def bottomUp() = new WorldGrid("../Model_data//Wind_Optimization/1_20_by0_5_bottom_up", Degrees(0.75))
  def simple() = new WorldGrid("../model_data/wind_solar_0_75", Degrees(0.75))
}

class WorldGrid(val name: String, val gridSize: Angle, val eroi_min: List[Double] = (2 until 40).map(_ * 0.5).toList) {

  val grids: List[GridCell] = Helper.getLines(name).map(GridCell(_, gridSize, eroi_min))

  // Sum v^2 * Area
  val totalDissipation = Terawatts(875.0 / 3.0)
  val totalSquareSpeedArea = grids.map(c => Math.pow(c.wind100m.mean.toMetersPerSecond, 2) * c.area.toSquareMeters).sum

  def dissipation(cell: GridCell) =
    if (cell.area.value > 0) totalDissipation / cell.area * (Math.pow(cell.wind100m.mean.toMetersPerSecond, 2) * cell.area.toSquareMeters) / totalSquareSpeedArea
    else WattsPerSquareMeter(0)

  def constrainedGrids(potential: EnergyGenerationPotential) = grids.filter(g => potential.suitabilityFactor(g) > 0)
  val onshoreGrids = grids.filter(g => g.onshore) // && g.center.latitude.toDegrees > -60)
  def onshoreConstrainedGrids(potential: EnergyGenerationPotential) = onshoreGrids.filter(g => potential.suitabilityFactor(g) > 0)
  val offshoreGrids = grids.filter(_.offshore)
  def offshoreGrids(maxDepth: Length): List[GridCell] = offshoreGrids.filter(_.waterDepth <= maxDepth)
  def offshoreConstrainedGrids(potential: EnergyGenerationPotential) = offshoreGrids.filter(g => potential.suitabilityFactor(g) > 0)

  val totalArea = Helper.area(grids)

  def country(c: Country) = grids.filter(_.country.isCountry(c.name))
  def country(c: String) = grids.filter(_.country.isCountry(c))
  def countries(c: List[String]) = grids.filter(_.country.isCountry(c))

  def europe = grids.filter(g => g.center.longitude.toDegrees >= -35.25 && g.center.longitude.toDegrees <= 34.5 && g.center.latitude.toDegrees >= 24.75 && g.center.latitude.toDegrees <= 69.75)

  val eu28countries = Helper.getLines("../model_data/EU28", "\t").map(_(0))
  def eu28 = grids.filter(g => eu28countries.contains(g.country.name))
  val ieaCountries = Helper.getLines("../model_data/ieaCountries", "\t").map(_(0))
  def iea = grids.filter(g => ieaCountries.contains(g.country.name))
  def minLatitude(grids: List[GridCell]) = grids.map(_.center.latitude.toDegrees).min
  def maxLatitude(grids: List[GridCell]) = grids.map(_.center.latitude.toDegrees).max
  def minLongitude(grids: List[GridCell]) = grids.map(_.center.longitude.toDegrees).min
  def maxLongitude(grids: List[GridCell]) = grids.map(_.center.longitude.toDegrees).max

  /**
   * Simple function to write values in a txt file to be able to plot them afterwards
   * ( Function ../WindPotentialPy/Plot.py )
   * ! All should be double casted to String, otherwise it will not work
   */
  def writeGrid(name: String, gr: List[GridCell] = grids, filter: Boolean = false) {

    val cells = if (filter) {
      val minLat = minLatitude(gr); val maxLat = maxLatitude(gr); val minLon = minLongitude(gr); val maxLon = maxLongitude(gr);
      grids.filter(g => (g.center.latitude.toDegrees >= minLat && g.center.latitude.toDegrees <= maxLat && g.center.longitude.toDegrees >= minLon && g.center.longitude.toDegrees <= maxLon))
    } else grids
    val out_stream = new PrintStream(new java.io.FileOutputStream(name))
    cells.map(g => {
      out_stream.print(g.center.latitude.value.toString + "\t" + g.center.longitude.value.toString)
      if (!filter || gr.contains(g)) {
        out_stream.print("\t" + g.yearlyClearnessIndex.toString + "\t" + g.irradiance.mean.toWattsPerSquareMeter.toString + "\t" + yearlyRadiation(g.center.latitude).toWattsPerSquareMeter.toString)
       // "\t" + (g.irradiance.mean.toWattsPerSquareMeter*8.76).toString + "\t" + (g.irradiance.month(0).toWattsPerSquareMeter*8.76).toString + "\t" + (g.irradiance.month(6).toWattsPerSquareMeter*8.76).toString)
      } else {
        out_stream.print("\t" + "0.0" + "\t" + "0.0"+ "\t" + "0.0")
      }
      out_stream.print("\n")

    })
    out_stream.close()
  }
  /*
 * latStart = lat-resolution
   lonStart = long-resolution
   arcsec = resolution / 15
   for i in range(0,15):
   for j in range(0,15):
   	latIt = latStart + i*arcsec
   	lonIt = lonStart + j*arcsec
              
 */
  def writeSubGrid(n: Int) {
    val out_stream = new PrintStream(new java.io.FileOutputStream("subGrid" + n))
    val step = 0.75 / n.toDouble
    out_stream.print("Latitude" + "\t" + "Longitude" + "\n")
    grids.map(g => {
      val lat = g.center.latitude.value; val lon = g.center.longitude.value
      if(lat!=90 && lat!=(-90) && lon!=180 && lon!=(-180)){
      val latStart = lat + 0.75 / 2.0 + step / 2.0; val lonStart = lon + 0.75 / 2.0 + step / 2.0
      for (i <- 1 to n) {
        for (j <- 1 to n) {
            out_stream.print((latStart - i * step).toString + "\t" + (lonStart - j * step).toString + "\n")
        }
      }
    
      }})
    out_stream.close()
  }
  /**
   *  lats = data[:, 0]; lon = data[:, 1]
   *  cfs = data[:, 2]; totalArea = data[:, 3]; suitableArea = data[:, 4];
   *  diam = data[:, 5]; rPower = data[:, 6]
   *  embodiedE = data[:, 7]
   */
  def optimizationInputs(name: String, gr: List[GridCell] = grids) {
    val out_stream = new PrintStream(new java.io.FileOutputStream(name))
    gr.map(g => {
      out_stream.print(g.center.latitude.value.toString + "\t" + g.center.longitude.value.toString + "\t" +
        g.wind100m.c.value.toString + "\t" + g.wind100m.k.toString + "\t" +
        g.area.toSquareKilometers.toString + "\t" + g.suitableArea(WindPotential()).toSquareKilometers.toString + "\t" +
        WindPotential().energyInputs(Megawatts(1), Joules(0), g).to(MegawattHours).toString + "\t" +
        (if (g.onshore) WindFarmEnergyInputs.onshoreOperation(MegawattHours(1)).to(MegawattHours).toString
        else WindFarmEnergyInputs.offshoreOperation(MegawattHours(1)).to(MegawattHours).toString) +
        "\t" + WindPotential().availabilityFactor(g).toString
        + "\t" + dissipation(g).toWattsPerSquareMeter.toString +
        "\t" + WindPower.airDensity(g.hubAltitude).toKilogramsPerCubicMeter.toString + "\n")

    })
    out_stream.close()
  }
  def writeEU28grids(name: String) {
    val eu = eu28
    val cells = {
      val minLat = minLatitude(eu28); val maxLat = maxLatitude(eu28); val minLon = minLongitude(eu28); val maxLon = maxLongitude(eu28);
      grids.filter(g => (g.center.latitude.toDegrees >= minLat && g.center.latitude.toDegrees <= maxLat && g.center.longitude.toDegrees >= minLon && g.center.longitude.toDegrees <= maxLon))
    }
    val out_stream = new PrintStream(new java.io.FileOutputStream(name))
    cells.map(g => {
      out_stream.print(g.center.latitude.value.toString + "\t" + g.center.longitude.value.toString + "\t")
      if (eu28.contains(g)) {
        out_stream.print(WindPotential().capacityDensity(g, 5, true).toWattsPerSquareMeter.toString + "\t" +
          WindPotential().capacityDensity(g, 8, true).toWattsPerSquareMeter.toString + "\t" + WindPotential().capacityDensity(g, 12, true).toWattsPerSquareMeter.toString)
      } else {
        out_stream.print("0.0" + "\t" + "0.0" + "\t" + "0.0")
      }
      out_stream.print("\n")
    })
  }
}

