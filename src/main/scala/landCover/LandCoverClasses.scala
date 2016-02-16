package landCover

import utils.Helper
import org.apache.poi.hssf.usermodel.HSSFRow
import squants.space._

class LandCoverClass(val code: Int, val label: String, val z0: Length, val classes: LandCoverClasses) {

  override def toString() = "Land Cover Class " + code + " : " + label + "," + z0
  def isUrban = classes.urbanAreas.contains(code)
}

abstract class LandCoverClasses(val name: String, legendFileName: String, codeIndex: Int = 0, labelIndex: Int = 1, z0Index: Int = 2) {

  val noData: List[Int]
  val urbanAreas: List[Int] = List()
  
  val classes: Map[Int, LandCoverClass] = {
    val sheet = Helper.xlsSheet(Helper.ressourcesPy + "/landCover/" + legendFileName + ".xls", 0)
    (1 to sheet.getLastRowNum).map(r => {
      val c = landCoverClass(sheet.getRow(r))
      (c.code -> c)
    }).toMap
  }
  def landCoverClass(row: HSSFRow): LandCoverClass = new LandCoverClass(Helper.toInt(row, codeIndex), Helper.toString(row, labelIndex), Meters(Helper.toDouble(row, z0Index)), this)
  def apply(c: Int) = classes(c)
}

class GlobCoverClasses extends LandCoverClasses("GlobCover2009", "globCover/GlobCover2009_Legend", z0Index = 5) {
  val noData = List(230)
  override val urbanAreas = List(190)
}

class ModisCoverClasses extends LandCoverClasses("Modis", "modis/modisLegend") {
  val noData = List()
  override val urbanAreas = List(13)

}

/////
class CorineLandCoverClasses extends LandCoverClasses("CorineLandCover", "clc/clc2000legend", codeIndex = 0, labelIndex = 12, z0Index = 10) {
  val noData = List(0, 48, 49, 50, 255)
}
