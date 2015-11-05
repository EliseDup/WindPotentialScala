package download

import java.io.File
import org.joda.time.DateTime
import org.apache.poi.hssf.usermodel.HSSFRow
import org.apache.poi.hssf.usermodel.HSSFWorkbook
import java.io.FileInputStream
import org.apache.poi.ss.usermodel.Workbook
import org.joda.time.format.DateTimeFormat

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
class EliaEntry(val time: DateTime, val forecast: Double, val actual: Double, val capacity: Double) {
  val capacityFactor = actual / capacity
}
abstract class EliaData(val name : String, folderName: String, startRow: Int) {
  val folderLocation = "/Users/Elise/Documents/workspace/data/" + folderName
  val dateFormat = DateTimeFormat.forPattern("dd/MM/yyyy HH:mm")

  val data: List[EliaEntry] = createData
  val n = data.size
  val times = data.map(_.time).toList
  val actuals = data.map(_.actual).toList
  val meanFactor = data.map(_.capacityFactor).sum / n
  
  
  // Annual production :
  val prod2014 = data.filter(_.time.getYear()==2014).map(_.actual).sum/4.0

  def createData: List[EliaEntry] = {
    val folder = new File(folderLocation)
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