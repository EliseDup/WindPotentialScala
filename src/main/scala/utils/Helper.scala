package utils

import java.io.ObjectOutputStream
import java.io.FileOutputStream
import java.io.ObjectInputStream
import java.io.FileInputStream
import historicalData.MeteoData
import historicalData.Observation
import historicalData.PowerGenerationObservation
import org.apache.poi.hssf.usermodel.HSSFRow
import org.joda.time.DateTime
import java.util.Date
import scala.io.Source
import java.io.FileWriter
import au.com.bytecode.opencsv.CSVWriter
import java.io.File
import org.apache.poi.hssf.usermodel.HSSFWorkbook
import org.apache.poi.hssf.usermodel.HSSFSheet

object Helper {
  val ressourcesPy = "/Users/Elise/Documents/workspace/WindPotentialPY/ressources/"
  val ressources = "/Users/Elise/Documents/workspace/WindPotential/ressources/"

  def saveResult(name: String, ob: Object) {
    val oos = new ObjectOutputStream(new FileOutputStream("results/" + name))
    oos.writeObject(ob)
    oos.close
  }

  def readResult(name: String) = {
    new ObjectInputStream(new FileInputStream("results/" + name)).readObject()
  }
  def meteo(city: String) = {
    new ObjectInputStream(new FileInputStream("results/meteo" + city)).readObject().asInstanceOf[MeteoData]
  }
  /**
   * XLS Reading
   */
  def toString(row: HSSFRow, col: Int): String = {
    if (row.getCell(col) == null || row.getCell(col).getStringCellValue.isEmpty()) ""
    else row.getCell(col).getStringCellValue
  }
  def stringToDouble(row: HSSFRow, col: Int): Double = {
    if (row.getCell(col) == null || row.getCell(col).getStringCellValue.isEmpty()) 0.0
    else row.getCell(col).getStringCellValue.toDouble
  }
  def toDouble(row: HSSFRow, col: Int): Double = {
    if (row.getCell(col) == null) 0.0
    else row.getCell(col).getNumericCellValue()
  }
  def toInt(row: HSSFRow, col: Int): Int = row.getCell(col).getNumericCellValue.toInt
  def toDate(row: HSSFRow, col: Int): Date = row.getCell(col).getDateCellValue

  /**
   * RMSE = SUM_i ((values(i)-predictions(i))^2) / N ?
   */
  def rmse(values: List[PowerGenerationObservation], predictions: List[Observation]): Double = {
    val squares = for (i <- values; if (predictions.find(_.time.equals(i.time)).isDefined)) yield {
      val j = predictions.find(_.time.equals(i.time)).get
      (i.actual - j.value) * (i.actual - j.value)
    }
    squares.sum / squares.size
  }

  def txtToCSV(input: String, output: String) {
    val lines = Source.fromFile(input).getLines()
    val writer = new CSVWriter(new FileWriter(output))
    for (l <- lines) writer.writeNext(l.split("\t"))
    writer.close()
  }

  def xlsSheet(fileName: String, index: Int): HSSFSheet = {
    val inp = new FileInputStream(new File(fileName))
    val wb = new HSSFWorkbook(inp)
    wb.getSheetAt(index)
  }

  /**
   * Distance between to point in Latitude,Longitude decimal degrees
   */
  val earthRadius = 6371000 // metres
  def distance(p1: GeoPoint, p2: GeoPoint) : Double = {
    val phi1 = p1.latitude.toRadians
    val phi2 = p2.latitude.toRadians
    val deltaPhi = (p2.latitude - p1.latitude).toRadians
    val deltaLambda = (p2.longitude - p1.longitude).toRadians

    val a = Math.pow(Math.sin(deltaPhi / 2.0), 2) +
      Math.cos(phi1) * Math.cos(phi2) * Math.pow(Math.sin(deltaLambda / 2.0), 2)
    val c = 2 * Math.atan2(Math.sqrt(a), Math.sqrt(1 - a))
    earthRadius * c
  }

}
case class GeoPoint(val latitude: Double, val longitude: Double){
  override def toString() = "Point of latitude "+latitude +", longitude :"+longitude
}