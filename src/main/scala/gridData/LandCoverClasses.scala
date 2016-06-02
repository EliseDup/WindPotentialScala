package gridData

import utils.Helper
import org.apache.poi.hssf.usermodel.HSSFRow
import squants.space._

object LandCover {
  def landCover(corine: String, globCover: String, modis: String) =
    if (!(globCover.equals("NA") || GlobCoverClasses.noData.contains(globCover.toInt))) GlobCoverClasses(globCover.toInt)
    else ModisCoverClasses(modis.toInt)
}
class LandCoverClass(val code: Int, val label: String, val z0: Length, val classes: LandCoverClasses) {

  override def toString() = "Land Cover Class " + code + " : " + label + "," + z0

  def isUrban = classes.urbanAreas.contains(code)
  def isForest = classes.forest.contains(code)
  def isIce = classes.ice.contains(code)
  def isAgricultural = classes.agriculturalAreas.contains(code)

  def isWaterBodies = classes.waterBodies.contains(code)
  def isFloodedArea = classes.floodedAreas.contains(code)

  def isSavannah = classes.savannah.contains(code)
  def isGrassLand = classes.grassLand.contains(code)
  def isShrubLand = classes.shrubLand.contains(code)
  def isBarrenArea = classes.barrenAreas.contains(code)

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

  val barrenAreas: List[Int]
  val grassLand: List[Int]
  val shrubLand: List[Int]
  val savannah: List[Int]

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

  val agriculturalAreas = List(11, 14, 20, 30)
  val forest = List(40, 50, 60, 70, 90, 100)
  val shrubLand = List(110, 130)
  val grassLand = List(120, 140, 150)
  val savannah = List()
  val floodedAreas = List(160, 170, 180)
  val urbanAreas = List(190)
  val barrenAreas = List(200)
  val waterBodies = List(210)
  val ice = List(220)
  val noData = List(230)
 
}

object ModisCoverClasses extends LandCoverClasses("Modis", "modis/modisLegend") {
  val noData = List()

  val waterBodies = List(0)
  val forest = List(1, 2, 3, 4, 5)
  val shrubLand = List(6, 7)
  val savannah = List(8, 9)
  val grassLand = List(10)
  val floodedAreas = List(11)
  val agriculturalAreas = List(12, 14)
  val urbanAreas = List(13)
  val ice = List(15)
  val barrenAreas = List(16)

}

/**
 * Do not use Corine land cover anymore as it is only available in Europe ..
 */
/*object CorineLandCoverClasses extends LandCoverClasses("CorineLandCover", "clc/clc2000legend", codeIndex = 0, labelIndex = 12, z0Index = 10) {
  val noData = List(0, 48, 49, 50, 255)
}*/
