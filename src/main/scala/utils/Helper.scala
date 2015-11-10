package utils

import java.io.ObjectOutputStream
import java.io.FileOutputStream

object Helper {
  
  def saveObject(name: String, ob: Object) {
      val oos = new ObjectOutputStream(new FileOutputStream("results/" + name))
      oos.writeObject(ob)
      oos.close
    }
}