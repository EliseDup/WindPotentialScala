package utils

import java.io.ObjectOutputStream
import java.io.FileOutputStream
import java.io.ObjectInputStream
import java.io.FileInputStream
import historicalData.MeteoData

object Helper {
  
  def saveResult(name: String, ob: Object) {
      val oos = new ObjectOutputStream(new FileOutputStream("results/" + name))
      oos.writeObject(ob)
      oos.close
    }
  
  def readResult(name : String) = { 
    new ObjectInputStream(new FileInputStream("results/"+ name)).readObject()
  }
  def meteo(city : String) = {
    new ObjectInputStream(new FileInputStream("results/meteo"+ city)).readObject().asInstanceOf[MeteoData]
  }
  /**
   * RMSE = SUM_i ((values(i)-predictions(i))^2) / N ?
   */
  def rmse(values : List[Double], predictions : List[Double]) : Double = {
    assert(values.size == predictions.size)
    (0 until values.size).map(i => Math.pow(values(i)-predictions(i),2)).sum / values.size.toDouble
  }
}