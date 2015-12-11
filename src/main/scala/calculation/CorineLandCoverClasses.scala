package calculation

import utils.Helper
import org.apache.poi.hssf.usermodel.HSSFRow

abstract class LandCoverClass(val code: Int, val label : String, val hubHeigthConversionRatio : Double){
  override def toString() = "Land Cover Class :" +label + "("+code+")"
}
abstract class LandCoverClasses[C <:LandCoverClass] {
  val classes : Map[Int, C]
  def apply(c : Int) = classes(c)
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
    val hubHeigthConversionRatioAndClass: (Int, Double)) extends LandCoverClass(grid_code, label1+label2+label3, hubHeigthConversionRatioAndClass._2)

object CorineLandCoverClass {
  def apply(row: HSSFRow) = {
    new CorineLandCoverClass(Helper.toInt(row, 0),
      Helper.toInt(row, 1), Helper.toInt(row, 2), Helper.toInt(row, 3), Helper.toInt(row, 4),
      Helper.toString(row, 5), Helper.toString(row, 6), Helper.toString(row, 7), Helper.toString(row, 8),
      (Helper.toInt(row, 9), Helper.toDouble(row, 10)))
  }
}

class CorineLandCoverClasses extends LandCoverClasses[CorineLandCoverClass] {

  val classes: Map[Int, CorineLandCoverClass] = {
    val sheet = Helper.xlsSheet(Helper.ressourcesPy + "/landCover/clc2000legend.xls", 0)
    (1 to sheet.getLastRowNum).map(r => {
      val c = CorineLandCoverClass(sheet.getRow(r))
      (c.grid_code -> c)
    }).toMap
  }
  override def apply(i: Int) = if (i == 0) classes(50) else super.apply(i)
}
/**
 * Value	Label
 * 0	Water
 * 1 Evergreen Needleleaf forest
 * 2 Evergreen Broadleaf forest
 * 3 Deciduous Needleleaf forest
 * 4	Deciduous Broadleaf forest
 * 5	Mixed forest
 * 6	Closed shrublands
 * 7	Open shrublands
 * 8	Woody savannas
 * 9	Savannas
 * 10	Grasslands
 * 11	Permanent wetlands
 * 12	Croplands
 * 13	Urban and built-up
 * 14	Cropland/Natural vegetation mosaic
 * 15	Snow and ice
 * 16	Barren or sparsely vegetated
 * 254	Unclassified
 * 255	Fill Value
 */
class GlobalLandCoverClass(code: Int, label: String, ratio : Double) extends LandCoverClass(code, label, ratio)
object GlobalLandCoverClass {
  def apply(pair:(Int,String)) = new GlobalLandCoverClass(pair._1,pair._2, 1.3)
}
class GlobalLandCoverClasses extends LandCoverClasses[GlobalLandCoverClass] {
  val pair = Array((0, "Water"),
    (1, "Evergreen Needleleaf forest"),
    (2, "Evergreen Broadleaf forest"),
    (3, "Deciduous Needleleaf forest"),
    (4, "Deciduous Broadleaf forest"),
    (5, "Mixed forest"),
    (6, "Closed shrublands"),
    (7, "Open shrublands"),
    (8, "Woody savannas"),
    (9, "Savannas"),
    (10, "Grasslands"),
    (11, "Permanent wetlands"),
    (12, "Croplands"),
    (13, "Urban and built-up"),
    (14, "Cropland/Natural vegetation mosaic"),
    (15, "Snow and ice"))
  val classes = pair.map(i => (i._1 -> GlobalLandCoverClass(i))).toMap
}