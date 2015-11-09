package download

import java.io.ObjectOutputStream
import java.io.FileOutputStream

/**
 * Load all the data, make the appropriate calculation and save them in files
 */
object Loader {

  def main(args: Array[String]) = {
saveObject("wind", new WindData)
    saveObject("solar", new SolarData)
    saveObject("load", new LoadData)
    def saveObject(name: String, ob: Object) {
      val oos = new ObjectOutputStream(new FileOutputStream("results/" + name))
      oos.writeObject(ob)
      oos.close
    }
  }

}