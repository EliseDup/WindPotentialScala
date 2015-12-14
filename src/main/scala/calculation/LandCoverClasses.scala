package calculation

import utils.Helper
import org.apache.poi.hssf.usermodel.HSSFRow

abstract class LandCoverClass(val code: Int, val label: String, val hubHeigthConversionRatio: Double) {
  override def toString() = "Land Cover Class :" + label + "(" + code + ")"
}
abstract class LandCoverClasses[C <: LandCoverClass](legendFileName: String) {
  val classes: Map[Int, C] = {
    val sheet = Helper.xlsSheet(Helper.ressourcesPy + "/landCover/" + legendFileName + ".xls", 0)
    (1 to sheet.getLastRowNum).map(r => {
      val c = landCoverClass(sheet.getRow(r))
      (c.code -> c)
    }).toMap
  }
  def landCoverClass(row: HSSFRow): C
  def apply(c: Int) = classes(c)

  val waterIndexes: List[Int]
  val openSpaces: List[Int]
  val agriculturalAreas: List[Int]

}

/**
 * Clc_code	Land cover code		varchar(3)	0
 * Grid_code			int(4)	0
 * Label1	Land cover nomenclature labels at level 1		varchar(255)	0
 * Label2	Land cover nomenclature labels at level 2		varchar(255)	0
 * Label3	Land cover nomenclature labels at level 3		varchar(255)	0
 * Level1	Land cover code at level 1		varchar(255)	0
 * Level2	Land cover code at level 2		varchar(255)	0
 * Level3	Land cover code at level 3		varchar(255)	0
 * RGB	RGB color code		varchar(15)	0
 *
 */
class CorineLandCoverClass(val grid_code: Int,
  val level1: Int, val level2: Int, val level3: Int, val clc_code: Int,
  val label1: String, val label2: String, val label3: String, val grb: String,
  hubHeigthConversionRatio: Double) extends LandCoverClass(grid_code, label1 + label2 + label3, hubHeigthConversionRatio)

object CorineLandCoverClass {
  def apply(row: HSSFRow) = {
    new CorineLandCoverClass(Helper.toInt(row, 0), Helper.toInt(row, 1), Helper.toInt(row, 2),
      Helper.toInt(row, 3), Helper.toInt(row, 4), Helper.toString(row, 5),
      Helper.toString(row, 6), Helper.toString(row, 7), Helper.toString(row, 8),
      Helper.toDouble(row, 10))
  }
}

class CorineLandCoverClasses extends LandCoverClasses[CorineLandCoverClass]("clc/clc2000legend") {
  def landCoverClass(row: HSSFRow) = CorineLandCoverClass(row)
  val waterIndexes = (40 to 44).toList ++ List(48, 49, 50, 255)
  val openSpaces = (30 to 34).toList
  val agriculturalAreas = (12 to 22).toList
}

class GlobalLandCoverClass(code: Int, label: String, ratio: Double) extends LandCoverClass(code, label, ratio)
object GlobalLandCoverClass {
  def apply(row: HSSFRow) = new GlobalLandCoverClass(Helper.toInt(row, 0), Helper.toString(row, 1), Helper.toDouble(row, 5))
}
class GlobalLandCoverClasses extends LandCoverClasses[GlobalLandCoverClass]("glc/Global_Legend") {
  def landCoverClass(row: HSSFRow) = GlobalLandCoverClass(row)
  val waterIndexes = (20 to 23).toList
  val openSpaces = List(11, 12)
  val agriculturalAreas = List(16)
}