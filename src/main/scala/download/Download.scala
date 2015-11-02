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
 */
class WindData {
  
  val windData = createWindData
  val n = windData.size
  
  def createWindData: List[EliaData] = {
    val folder = new File("/Users/Elise/Documents/workspace/data/windData")
    val files = folder.listFiles()
    val res = {
      for (f <- files) yield {
        val inp = new FileInputStream(f)
        val wb = new HSSFWorkbook(inp)
        val sheet = wb.getSheetAt(0)
        for (r <- 4 to sheet.getLastRowNum; if (sheet.getRow(r).getCell(0) != null))
          yield createWindEntry(sheet.getRow(r))
      }
    }
    res.flatten.toList
  }

  def createWindEntry(row: HSSFRow): EliaData = {
    val dateFormat = DateTimeFormat.forPattern("dd/MM/yyyy HH:mm")
    println(row.getCell(0).getStringCellValue)
     new EliaData(new DateTime(dateFormat.parseDateTime(row.getCell(0).getStringCellValue)),
      toDouble(row, 1), toDouble(row, 2), toDouble(row, 3))
  }
  def toDouble(row: HSSFRow, col: Int): Double = {
    if (row.getCell(col) == null) 0.0
    else if(row.getCell(col).getStringCellValue.isEmpty()) 0.0
    else row.getCell(col).getStringCellValue.toDouble
  }
}