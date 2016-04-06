package gridData

import utils.Helper
import org.apache.poi.hssf.usermodel.HSSFRow
import squants.space._

class LandCoverClass(val code: Int, val label: String, val z0: Length, val classes: LandCoverClasses) {

  override def toString() = "Land Cover Class " + code + " : " + label + "," + z0
  def isUrban = classes.urbanAreas.contains(code)
  def isForest = classes.forest.contains(code)
  def isIce = classes.ice.contains(code)
  def isAgricultural = classes.agriculturalAreas.contains(code)
  def isOpenArea = classes.openAreas.contains(code)
  def isWaterBodies = classes.waterBodies.contains(code)
  def isFloodedArea = classes.floodedAreas.contains(code)

}
// A 'super' class will englobe several land cover class for a more general classification (i.e. forest, water bodies, ...)
class LandCoverSuperClass(val name: String, val indexes: List[Int]) {
  def is(index: Int) = indexes.contains(index)
}

abstract class LandCoverClasses(val name: String, legendFileName: String, codeIndex: Int = 0, labelIndex: Int = 1, z0Index: Int = 2) {

  val noData: List[Int]
  val urbanAreas: List[Int]
  val ice: List[Int]
  val forest: List[Int]
  val agriculturalAreas: List[Int]
  val waterBodies: List[Int]
  val floodedAreas: List[Int]
  val openAreas: List[Int]

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

object GlobCoverClasses extends LandCoverClasses("GlobCover2009", "globCover/GlobCover2009_Legend", z0Index = 5) {
  val noData = List(230)
  val urbanAreas = List(190)
  val ice = List(220)
  val forest = List(40, 50, 60, 70, 90, 100, 110, 120)
  val agriculturalAreas = List(11, 14, 20, 30)
  val waterBodies = List(210)
  val openAreas = List(130, 140, 150, 200)
  val floodedAreas = List(160, 170, 180)
}

object ModisCoverClasses extends LandCoverClasses("Modis", "modis/modisLegend") {
  val noData = List()

  val urbanAreas = List(13)
  val ice = List(15)
  val forest = List(1, 2, 3, 4, 5, 6, 8, 9)
  val waterBodies = List(0)
  val agriculturalAreas = List(12, 14)
  val openAreas = List(7, 10, 16)
  val floodedAreas = List(11)
}

/**
 * Do not use Corine land cover anymore as it is only available in Europe ..
 */
/*object CorineLandCoverClasses extends LandCoverClasses("CorineLandCover", "clc/clc2000legend", codeIndex = 0, labelIndex = 12, z0Index = 10) {
  val noData = List(0, 48, 49, 50, 255)
}*/
