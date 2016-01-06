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
import squants.Meters
import squants.space.SquareMeters
import squants.space.Degrees
import squants.space.Angle

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

  def txtToCSV(input: String, output: String, indexes : List[Int]=List(), indexesOnly:Boolean=false) {
    val lines = Source.fromFile(input).getLines()
    val writer = new CSVWriter(new FileWriter(output))
    for (l <- lines) {
      val str = l.split("\t")
      val res = if(indexesOnly) indexes.map(str(_)).toArray else str
      writer.writeNext(res)
    }
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
  val earthRadius = Meters(6371000)
  def distance(p1: GeoPoint, p2: GeoPoint) = {
    val phi1 = p1.latitude.toRadians
    val phi2 = p2.latitude.toRadians
    val deltaPhi = (p2.latitude - p1.latitude).toRadians
    val deltaLambda = (p2.longitude - p1.longitude).toRadians

    val a = Math.pow(Math.sin(deltaPhi / 2.0), 2) +
      Math.cos(phi1) * Math.cos(phi2) * Math.pow(Math.sin(deltaLambda / 2.0), 2)
    val c = 2 * Math.atan2(Math.sqrt(a), Math.sqrt(1 - a))
    earthRadius*c
  }
  /**
   * The area of the earth between a line of latitude and the north pole (the area of a spherical cap)
   * A = 2 PI R h
   * With h = R * (1-sin(lat))
   *
   * So the area between two line of latitude is
   * A = 2 PI R^2 (1 - sin(lat1)) - 2 PI R^2 (1 - sin(lat2)) = 2 PI R^2 |sin(lat1)-sin(lat2)|
   *
   * The are of the lat long rectangle is proportionnal to the difference between the 2 longitudes
   *
   * = > AreaRect = 2 PI R^2 *|sin(lat1)-sin(lat2)| * |lon1 - lon2| / 360
   */
  def areaRectangle(lowerLeftCorner: GeoPoint, upperRightCorner: GeoPoint) = {
     earthRadius*earthRadius*(1.0 / 180.0 * Math.PI *
      Math.abs(lowerLeftCorner.latitude.sin-upperRightCorner.latitude.sin) *
      Math.abs(lowerLeftCorner.longitude.toDegrees - upperRightCorner.longitude.toDegrees))
  }
}
case class GeoPoint(val latitude: Angle, val longitude: Angle) {
  override def toString() = "Point of latitude " + latitude + ", longitude :" + longitude
}