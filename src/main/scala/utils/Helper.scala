package utils

import java.io.ObjectOutputStream
import java.io.FileOutputStream
import java.io.ObjectInputStream
import java.io.FileInputStream
import historical_data.MeteoData
import historical_data.Observation
import historical_data.PowerGenerationObservation
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
import squants.space._
import org.apache.poi.ss.usermodel.Row
import squants.motion.Velocity
import grid._
import wind_energy.WindPotential
import squants.radio.Irradiance

object Helper {
  val ressourcesPy = "/Users/Elise/Documents/workspace/resources/"
  val resultsPy = "/Users/Elise/Documents/workspace/WindPotentialPy/results/"
  val ressources = "/Users/Elise/Documents/workspace/WindPotential/resources/"

  def getLines(file: String, delimiter: String = "\t") = Source.fromFile(file).getLines().toList.map(_.split(delimiter))

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
  def toString(row: Row, col: Int): String = {
    if (row.getCell(col) == null || row.getCell(col).getStringCellValue.isEmpty()) ""
    else row.getCell(col).getStringCellValue
  }
  def stringToDouble(row: Row, col: Int): Double = {
    if (row.getCell(col) == null || row.getCell(col).getStringCellValue.isEmpty()) 0.0
    else row.getCell(col).getStringCellValue.toDouble
  }
  def toDouble(row: Row, col: Int): Double = {
    if (row.getCell(col) == null) 0.0
    else row.getCell(col).getNumericCellValue()
  }
  def toInt(row: Row, col: Int): Int = row.getCell(col).getNumericCellValue.toInt
  def toDate(row: Row, col: Int): Date = row.getCell(col).getDateCellValue

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

  def txtToCSV(input: String, output: String, indexes: List[Int] = List(), indexesOnly: Boolean = false) {
    val lines = Source.fromFile(input).getLines()
    val writer = new CSVWriter(new FileWriter(output))
    for (l <- lines) {
      val str = l.split("\t")
      val res = if (indexesOnly) indexes.map(str(_)).toArray else str
      writer.writeNext(res)
    }
    writer.close()
  }

  def xlsSheet(fileName: String, index: Int): HSSFSheet = {
    val inp = new FileInputStream(new File(fileName))
    val wb = new HSSFWorkbook(inp)
    wb.getSheetAt(index)
  }
  def xlsSheet(fileName: String, sheetName: String): HSSFSheet = {
    val inp = new FileInputStream(new File(fileName))
    val wb = new HSSFWorkbook(inp)
    wb.getSheet(sheetName)
  }
 
  // Logarithmic Wind Profile
  def windSpeedAt(windSpeed0 : Velocity,h0: Length, z0 : Length,height: Length): Velocity = Math.log(height / z0) / Math.log(h0 / z0) * windSpeed0

  /**
   * y(x) = A (x-x1)(x-x2) + B (x-x1)(x-x3) + C (x-x2)(x-x3)
   *
   * y(x1) = y1 => C = y1 / (x1-x2)(x1-x3)
   * y(x2) = y2 => B = y2 / (x2-x1)(x2-x3)
   * y(x3) = y3 => A = y3 / (x3-x1)(x3-x2)
   *
   * y(x) = (A+B+C) x^2 - (Ax1+Ax2+Bx1+Bx3+Cx2+Cx3) x + (x1x2+x1x3+x2x3)
   */
  case class Point(val x: Double, val y: Double)
  class SecondOrderPolynomial(a: Double, b: Double, c: Double) {
    def apply(x: Double) = a * x * x + b * x + c
  }
  object SecondOrderPolynomial {
    def apply(p1: Point, p2: Point, p3: Point) = {
      val a = p3.y / ((p3.x - p1.x) * (p3.x - p2.x))
      val b = p2.y / ((p2.x - p1.x) * (p2.x - p3.x))
      val c = p1.y / ((p1.x - p2.x) * (p1.x - p3.x))
      new SecondOrderPolynomial(a + b + c, -(a * (p1.x + p2.x) + b * (p1.x + p3.x) + c * (p2.x + p3.x)), a * p1.x * p2.x + b * p1.x * p3.x + c * p2.x * p3.x)
    }
  }
  class FirstOrderPolynomial(p1: Point, p2: Point) {
    val a = (p1.y - p2.y) / (p1.x - p2.x)
    val b = p1.y - a * p1.x
    def apply(x: Double) = a * x + b
  }
  object FirstOrderPolynomial {
    def apply(p1: Point, p2: Point) = new FirstOrderPolynomial(p1, p2)
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
    earthRadius * c
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
  def areaRectangle(lowerLeftCorner: GeoPoint, upperRightCorner: GeoPoint): Area = {
    earthRadius * earthRadius * (1.0 / 180.0 * Math.PI *
      Math.abs(lowerLeftCorner.latitude.sin - upperRightCorner.latitude.sin) *
      Math.abs(lowerLeftCorner.longitude.toDegrees - upperRightCorner.longitude.toDegrees))
  }
  def areaRectangle(center: GeoPoint, resolution: Angle) : Area = {
    val lowerLeftCorner = GeoPoint(center.latitude - resolution/2.0, center.longitude - resolution/2.0)
    val upperRightCorner = GeoPoint(center.latitude + resolution/2.0, center.longitude + resolution/2.0)
    areaRectangle(lowerLeftCorner, upperRightCorner)
  }
  
  def area(gr: List[GridCell]) = gr.map(_.area).foldLeft(SquareKilometers(0))(_ + _)
  def suitableArea(gr: List[GridCell], potential: EnergyGenerationPotential) = gr.map(g => g.area * WindPotential.suitabilityFactor(g)).foldLeft(SquareKilometers(0))(_ + _)
  def mean(values: List[(GridCell, Double)]) = {
   val res = values.map(i => i._1.area*i._2).foldLeft(SquareKilometers(0))(_+_) / area(values.map(_._1))
   res
  }
  def listValueVSArea(gr: List[(Double, Area)]) = listValueVSCumulated(gr.map(g => (g._1, g._2.to(SquareKilometers) / (1E6))))

  def listEROIVSCumulatedProduction(gr: List[(GridCell, Double)], potential: EnergyGenerationPotential, density: Option[Irradiance] = None): (List[Double], List[Double]) = {
    listValueVSCumulated(gr.map(g => (potential.EROI(g._1, Some(g._2), density), potential.energyGeneratedPerYear(g._1, Some(g._2), density).to(Exajoules))))
  }
  def listEROIVSCumulatedPower(gr: List[(GridCell, Double)], potential: EnergyGenerationPotential, density: Option[Irradiance] = None): (List[Double], List[Double]) = {
    listValueVSCumulated(gr.map(g => (potential.EROI(g._1, Some(g._2), density), potential.powerGenerated(g._1, Some(g._2), density).to(Terawatts))))
  }
  def listEnergyGeneratedPerYearVSCumulatedProduction(gr: List[(GridCell, Double)], potential: EnergyGenerationPotential) = {
    val prod = gr.map(g => (potential.energyGeneratedPerYear(g._1, Some(g._2))).to(TerawattHours))
    listValueVSCumulated(prod.map(i => (i, i)))
  }

  def listValueVSCumulated(values: List[(Double, Double)]): (List[Double], List[Double]) = {
    val sorted = values.sortBy(_._1).reverse
    (sorted.map(_._2).scanLeft(0.0)(_ + _), sorted.map(_._1) :+ 0.0)
  }

  def plotEROIVSCumulatedProduction(gr: List[(GridCell, Double)], potential: EnergyGenerationPotential) = {
    val list = listEROIVSCumulatedProduction(gr, potential)
    PlotHelper.plotXY(List((list._1, list._2, "")), xLabel = "Cumulated Annual Production [TWh]", yLabel = "EROI")
  }
}
case class GeoPoint(val latitude: Angle, val longitude: Angle) {
  override def toString() = "Point of latitude " + latitude + ", longitude :" + longitude
}