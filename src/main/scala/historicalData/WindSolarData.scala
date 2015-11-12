package historicalData

import java.io.File
import org.joda.time.DateTime
import org.apache.poi.hssf.usermodel.HSSFRow
import org.apache.poi.hssf.usermodel.HSSFWorkbook
import java.io.FileInputStream
import org.apache.poi.ss.usermodel.Workbook
import org.joda.time.format.DateTimeFormat
import org.joda.time.Days
import org.joda.time.Months

/**
 * Folder with monthly data
 *
 * Structure of the excel files generated from ELIA website :
 *
 * Wind Data :
 * Begins at row # 4
 *  - column 0 :DateTime -> TEXTCELL
 *  - column 1 :Day-ahead Forecast [MW]	 -> TEXTCELL
 *  - column 2 :Real-time Measured [MW]	 -> TEXTCELL
 *  - column 3 :Monitored Capacity [MW]	 -> TEXTCELL
 *  - column 4 :Decremental bid requested on wind farm(s) [yes/no] -> TEXTCELL
 *
 * Solar Data (http://www.elia.be/fr/grid-data/production/Solar-power-generation-data/Graph) :
 * Begins at row # 4
 *  - column 0 :DateTime	 -> TEXTCELL
 *  - column 1 :Day-Ahead forecast [MW]	 -> NUMERICCELL
 *  - column 2 :Intraday forecast [MW]	 -> NUMERICCELL
 *  - column 3 :Real-time Upscaled Measurement [MW]		 -> NUMERICCELL
 *  - column 4 :Corrected Upscaled Measurement [MW]		 -> NUMERICCELL
 *  - column 5 :Monitored Capacity [MWp]	 -> NUMERICCELL
 *
 */
class PowerGenerationObservation(time: DateTime, val forecast: Double, val actual: Double, val capacity: Double, name : String) extends Observation(time, actual, name) {
  // Capacity factor correspond to the power obtained to the capacity installed
  val capacityFactor = actual / capacity 
  // Given we have a power measurement every 15 minutes, the energy produced [MWh] is Power/4
  val energy = actual / 4.0
}

abstract class EliaData(val name: String, folderName: String, startRow: Int) extends HistoricalData[PowerGenerationObservation](name) {

  val meanFactor = observations.map(_.capacityFactor).sum / n
 
  val dailyAverages = {
    (for (i <- 0 until nDays) yield {
      val day = observations(0).time.withTimeAtStartOfDay.plusDays(i)
      val d = observations.filter(i => sameDay(day, i.time))
      new PowerGenerationObservation(day, d.map(_.forecast / 4.0).sum, d.map(_.actual / 4.0).sum, d.map(_.capacity / 4.0).sum, name)
    }).toList
  }
  val monthlyAverages = {
    (for (i <- 0 until nMonths) yield {
      val month = observations(0).time.withTimeAtStartOfDay.withDayOfMonth(1).plusMonths(i)
      val d = observations.filter(i => sameMonth(month, i.time))
      new PowerGenerationObservation(month, d.map(_.forecast / 4.0).sum, d.map(_.actual / 4.0).sum, d.map(_.capacity / 4.0).sum, name)
    }).toList
  }
  
  def createData: List[PowerGenerationObservation] = {
    val folder = new File(folderName)
    val files = folder.listFiles()
    val res = {
      for (f <- files; if (!f.getAbsolutePath.contains(".DS_Store"))) yield {
        val inp = new FileInputStream(f)
        val wb = new HSSFWorkbook(inp)
        val sheet = wb.getSheetAt(0)
        for (r <- startRow to sheet.getLastRowNum; if (sheet.getRow(r).getCell(0) != null))
          yield createEntry(sheet.getRow(r))
      }
    }
    res.flatten.toList
  }
  def createEntry(row: HSSFRow): PowerGenerationObservation

  def toDouble(row: HSSFRow, col: Int): Double = {
    if (row.getCell(col) == null) 0.0
    else if (row.getCell(col).getStringCellValue.isEmpty()) 0.0
    else row.getCell(col).getStringCellValue.toDouble
  }
}
class WindData extends EliaData("Wind", "data/windData", 4) {
  def createEntry(row: HSSFRow): PowerGenerationObservation = {
    @transient val dateFormat = DateTimeFormat.forPattern("dd/MM/yyyy HH:mm")
    new PowerGenerationObservation(new DateTime(dateFormat.parseDateTime(row.getCell(0).getStringCellValue)),
      toDouble(row, 1), toDouble(row, 2), toDouble(row, 3), name)
  }
}

class SolarData extends EliaData("Solar", "data/solarData", 4) {
  def createEntry(row: HSSFRow): PowerGenerationObservation = {
    @transient val dateFormat = DateTimeFormat.forPattern("dd/MM/yyyy HH:mm")
    new PowerGenerationObservation(new DateTime(dateFormat.parseDateTime(row.getCell(0).getStringCellValue)),
      row.getCell(1).getNumericCellValue,
      row.getCell(3).getNumericCellValue,
      row.getCell(5).getNumericCellValue, name)
  }
}