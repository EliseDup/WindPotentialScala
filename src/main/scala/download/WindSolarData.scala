package download

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
class EliaEntry(val time: DateTime, val forecast: Double, val actual: Double, val capacity: Double) extends Serializable {
  val capacityFactor = actual / capacity
  override def toString() = "Date :" + time + ", forecast [kW] :" + forecast + ", actual [kW]" + actual + ", installed capacity [kW]:" + capacity
}
abstract class EliaData(val name: String, folderName: String, startRow: Int) extends Serializable {

  @transient val dateFormat = DateTimeFormat.forPattern("dd/MM/yyyy HH:mm")

  val data: List[EliaEntry] = createData
  val n = data.size
  val times = data.map(_.time).toList
  val actuals = data.map(_.actual).toList
  val meanFactor = data.map(_.capacityFactor).sum / n

  def dataYear(y: Int) = data.filter(_.time.getYear() == y)
  val nDays = Days.daysBetween(data(0).time, data(n - 1).time).getDays() + 1
  val nMonths = Months.monthsBetween(data(0).time, data(n - 1).time).getMonths() + 1

  val dailyAverages = {
    (for (i <- 0 until nDays) yield {
      val day = data(0).time.withTimeAtStartOfDay.plusDays(i)
      val d = data.filter(i => sameDay(day,i.time))
      new EliaEntry(day, d.map(_.forecast / 4.0).sum, d.map(_.actual / 4.0).sum, d.map(_.capacity / 4.0).sum)
    }).toList
  }
  val monthlyAverages = {
    (for (i <- 0 until nMonths) yield {
      val month = data(0).time.withTimeAtStartOfDay.withDayOfMonth(1).plusMonths(i)
      val d = data.filter(i => sameMonth(month,i.time))
      new EliaEntry(month, d.map(_.forecast / 4.0).sum, d.map(_.actual / 4.0).sum, d.map(_.capacity / 4.0).sum)
    }).toList
  }
  def sameDay(d1: DateTime, d2: DateTime): Boolean = {
    d1.getYear == d2.getYear && d1.dayOfYear == d2.dayOfYear
  }
  def sameMonth(d1: DateTime, d2: DateTime): Boolean = {
    d1.getYear == d2.getYear && d1.monthOfYear == d2.monthOfYear
  }

  def createData: List[EliaEntry] = {
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
  def createEntry(row: HSSFRow): EliaEntry

  def toDouble(row: HSSFRow, col: Int): Double = {
    if (row.getCell(col) == null) 0.0
    else if (row.getCell(col).getStringCellValue.isEmpty()) 0.0
    else row.getCell(col).getStringCellValue.toDouble
  }
}
class WindData extends EliaData("Wind", "windData", 4) {
  def createEntry(row: HSSFRow): EliaEntry = {
    new EliaEntry(new DateTime(dateFormat.parseDateTime(row.getCell(0).getStringCellValue)),
      toDouble(row, 1), toDouble(row, 2), toDouble(row, 3))
  }
}

class SolarData extends EliaData("Solar", "solarData", 4) {
  def createEntry(row: HSSFRow): EliaEntry = {
    new EliaEntry(new DateTime(dateFormat.parseDateTime(row.getCell(0).getStringCellValue)),
      row.getCell(1).getNumericCellValue,
      row.getCell(3).getNumericCellValue,
      row.getCell(5).getNumericCellValue)
  }
}