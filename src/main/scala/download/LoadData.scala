package download

import org.joda.time.DateTime
import java.io.File
import java.io.FileInputStream
import org.apache.poi.hssf.usermodel.HSSFWorkbook
import org.joda.time.format.DateTimeFormat

// Load in KW for a quarter, convert for kWh
class LoadEntry(val time: DateTime, val load: Double){
  val consumption = load / 4.0
}

class LoadData {
  val dateFormat = DateTimeFormat.forPattern("dd/MM/yyyy HH:mm")
  val dayFormat = DateTimeFormat.forPattern("dd/MM/yyyy")

  val data: List[LoadEntry] = createData
  // replacer tous les 0 par un moyenne ?
  val correctedData = {
    val mean = data.filter(_.load > 0).map(_.load).sum / data.filter(_.load > 0).size
    println("Mean laod 2014-2015" + "\t" + mean)
    for(i <- data) yield {if(i.load > 0) i else new LoadEntry(i.time, mean)}
  }

  def createData: List[LoadEntry] = {
    val folder = new File("/Users/Elise/Documents/workspace/data/loadData")
    val files = folder.listFiles()
    val startRow = 2
    val startCol = 1
    val endCol = startCol + 24 * 4

    val res = {
      for (f <- files; if (!f.getAbsolutePath.contains(".DS_Store"))) yield {
        val inp = new FileInputStream(f)
        val wb = new HSSFWorkbook(inp)
        val sheet = wb.getSheetAt(0)
        (for (r <- startRow until sheet.getLastRowNum) yield {
          // Col = Hour
          val row = sheet.getRow(r)
          val day = new DateTime(row.getCell(0).getDateCellValue())
          for (c <- startCol until endCol) yield {
            new LoadEntry(day.plusMinutes(15*(c-startCol)), row.getCell(c).getNumericCellValue)
          }
        }).flatten
      }
    }
    res.flatten.toList
  }
}