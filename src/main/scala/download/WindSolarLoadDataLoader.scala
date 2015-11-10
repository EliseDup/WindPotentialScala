package download

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
Helper.saveObject("wind", new WindData)
   Helper.saveObject("solar", new SolarData)
    Helper.saveObject("load", new LoadData)
    
  }

}