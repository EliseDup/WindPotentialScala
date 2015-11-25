package historicalData

import java.io.ObjectOutputStream
import java.io.FileOutputStream
import utils.Helper

/**
 * Load all the data about wind and solar generation
 * total load on the grid
 *
 * make the appropriate calculation and save them in files
 */
object WindSolarLoadDataLoader {

  def main(args: Array[String]) = {
    Helper.saveResult("load", new LoadData)
    Helper.saveResult("wind", new WindEnergyData)
    Helper.saveResult("solar", new SolarEnergyData)
  }

}