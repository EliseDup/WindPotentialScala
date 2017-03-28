package historical_data

import java.io.FileInputStream
import java.io.File
import org.apache.poi.hssf.usermodel.HSSFWorkbook
import org.apache.poi.hssf.usermodel.HSSFRow
import utils.Helper

class MeteoStation(val city: String, val countryID: String,
    val stationID: String, val lat: Double,
    val lon: Double, val elev: Double) extends Serializable {
  def writeToXLS(row: HSSFRow) { row.createCell(0).setCellValue(toString()) }
  override def toString() = city + ", lat :" + lat + ", lon:" + lon + ", elev:" + elev
}

object MeteoStation {
  def apply(row: HSSFRow) = {
    new MeteoStation(
      Helper.toString(row, 0),
      Helper.toString(row, 1),
      Helper.toString(row, 2),
      Helper.toDouble(row, 3),
      Helper.toDouble(row, 4),
      Helper.toDouble(row, 5))
  }
}

class MeteoStations extends Serializable {
  val stations = {
    val wb = new HSSFWorkbook(new FileInputStream(new File("resources/wunderground_stations.xls")))
    val sheet = wb.getSheetAt(0)
    for (r <- 1 to sheet.getLastRowNum) yield MeteoStation(sheet.getRow(r))
  }
}