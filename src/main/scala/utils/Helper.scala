package utils

import java.io.ObjectOutputStream
import java.io.FileOutputStream
import java.io.ObjectInputStream
import java.io.FileInputStream

object Helper {
  
  def saveResult(name: String, ob: Object) {
      val oos = new ObjectOutputStream(new FileOutputStream("results/" + name))
      oos.writeObject(ob)
      oos.close
    }
  
  def readResult(name : String) = { 
    new ObjectInputStream(new FileInputStream("results/"+ name)).readObject()
  }
}