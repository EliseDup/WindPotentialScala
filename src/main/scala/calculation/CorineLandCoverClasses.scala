package calculation

import utils.Helper
import org.apache.poi.hssf.usermodel.HSSFRow

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
    val hubHeigthConversionRatio: (Int, Double)) {

  override def toString() = "CLC Class " + clc_code + ":" + label1 + "," + label2 + "," + label3
}

object CorineLandCoverClass {
  def apply(row: HSSFRow) = {
    new CorineLandCoverClass(Helper.toInt(row, 0),
      Helper.toInt(row, 1), Helper.toInt(row, 2), Helper.toInt(row, 3), Helper.toInt(row, 4),
      Helper.toString(row, 5), Helper.toString(row, 6), Helper.toString(row, 7), Helper.toString(row, 8),
      (Helper.toInt(row, 9), Helper.toDouble(row, 10)))
  }
}

class CorineLandCoverClasses {

  val classes: Map[Int, CorineLandCoverClass] = {
    val sheet = Helper.xlsSheet(Helper.ressourcesPy + "/clc/clc2000legend.xls", 0)
    (1 to sheet.getLastRowNum).map(r => {
      val c = CorineLandCoverClass(sheet.getRow(r))
      (c.grid_code -> c)
    }).toMap
  }
  def apply(i: Int) = if(i==0) classes(50) else classes(i)
}

