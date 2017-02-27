package historical_data

import org.joda.time.DateTime
import java.io.File
import java.io.FileInputStream
import org.apache.poi.hssf.usermodel.HSSFWorkbook

// Load in KW for a quarter, convert for kWh
class LoadObservation(time: DateTime, value: Double) extends Observation(time, value, "Load") {
  val consumption = value / 4.0
}

class LoadData extends HistoricalData[LoadObservation]("Load") {

  def createData: List[LoadObservation] = {
    val folder = new File("data/loadData")
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
            new LoadObservation(day.plusMinutes(15 * (c - startCol)), row.getCell(c).getNumericCellValue)
          }
        }).flatten
      }
    }
    res.flatten.toList
  }

  def mean(t: DateTime, list: List[LoadObservation]) = new LoadObservation(t, list.map(_.value / 4.0).sum)

  // replacer tous les 0 par une moyenne ?
  val correctedData = {
    val mean = observations.filter(_.value > 0).map(_.value).sum / observations.filter(_.value > 0).size
    println("Mean laod 2014-2015" + "\t" + mean)
    for (i <- observations) yield { if (i.value > 0) i else new LoadObservation(i.time, mean) }
  }
}