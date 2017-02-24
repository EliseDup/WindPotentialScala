package gridData

import utils.Helper
import org.apache.poi.hssf.usermodel.HSSFRow
import squants.space._

object LandCover {
  def landCover(globCover: String, modis: String) =
    if (!(globCover.equals("NA") || GlobCoverClasses.noData.contains(globCover.toInt))) GlobCoverClasses(globCover.toInt)
    else ModisCoverClasses(modis.toInt)
}

class LandCoverClass(val code: Int, val label: String, val z0: Length, val classes: LandCoverClasses) {

  override def toString() = "Land Cover Class " + code + " : " + label + "," + z0

  def noData = classes.noData.contains(code)
  def waterBodies = classes.waterBodies.contains(code); 
  def ice = classes.ice.contains(code); 
  def bareAreas = classes.bareAreas.contains(code)
  def grassland = classes.grassland.contains(code); 
  def sparseVegetation = classes.sparseVegetation.contains(code); 
  def croplands = classes.croplands.contains(code); 
  def shrubland = classes.shrubland.contains(code); 
  def wetlands = classes.wetlands.contains(code)
  def mosaicVegetationCropland = classes.mosaicVegetationCropland.contains(code); 
  def floodedAreas = classes.floodedAreas.contains(code); 
  def mosaicGrasslandForestShrubland = classes.mosaicGrasslandForestShrubland.contains(code);
  def urbanAreas = classes.urbanAreas.contains(code)
  def forests = classes.forests.contains(code)

} 

// A 'super' class will englobe several land cover class for a more general classification (i.e. forest, water bodies, ...)
class LandCoverSuperClass(val name: String, val indexes: List[Int]) {
  def is(index: Int) = indexes.contains(index)
}
// For each grid cell we divided into a subgrid to have a more precise image of the land cover
class DetailedLandCover(val indexes : List[(Double, Int)]){
  
  def forests = indexes.filter(i => GlobCoverClasses.forests.contains(i._2)).map(_._1).sum
  def waterBodies = indexes.filter(i => GlobCoverClasses.waterBodies.contains(i._2)).map(_._1).sum
  def ice = indexes.filter(i => GlobCoverClasses.ice.contains(i._2)).map(_._1).sum
  def bareAreas = indexes.filter(i => GlobCoverClasses.bareAreas.contains(i._2)).map(_._1).sum
  def grassland = indexes.filter(i => GlobCoverClasses.grassland.contains(i._2)).map(_._1).sum
  def sparseVegetation = indexes.filter(i => GlobCoverClasses.sparseVegetation.contains(i._2)).map(_._1).sum
  def croplands = indexes.filter(i => GlobCoverClasses.croplands.contains(i._2)).map(_._1).sum
  def shrubland = indexes.filter(i => GlobCoverClasses.shrubland.contains(i._2)).map(_._1).sum
  def wetlands = indexes.filter(i => GlobCoverClasses.wetlands.contains(i._2)).map(_._1).sum
  def mosaicVegetationCropland = indexes.filter(i => GlobCoverClasses.mosaicVegetationCropland.contains(i._2)).map(_._1).sum
  def floodedAreas = indexes.filter(i => GlobCoverClasses.floodedAreas.contains(i._2)).map(_._1).sum
  def mosaicGrasslandForestShrubland = indexes.filter(i => GlobCoverClasses.mosaicGrasslandForestShrubland.contains(i._2)).map(_._1).sum
  def urbanAreas = indexes.filter(i => GlobCoverClasses.urbanAreas.contains(i._2)).map(_._1).sum
  def noData = indexes.filter(i => GlobCoverClasses.noData.contains(i._2)).map(_._1).sum
  
}
abstract class LandCoverClasses(val name: String, legendFileName: String, codeIndex: Int = 0, labelIndex: Int = 1, z0Index: Int = 2) {

  val noData: List[Int]
  val waterBodies: List[Int]; val ice: List[Int]; val bareAreas: List[Int]
  val grassland: List[Int]; val sparseVegetation: List[Int]; val croplands: List[Int]; val shrubland: List[Int]; val wetlands: List[Int]
  val mosaicVegetationCropland: List[Int]; val floodedAreas: List[Int]; val mosaicGrasslandForestShrubland: List[Int];
  val urbanAreas: List[Int]
  val forests: List[Int]

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

  val indexes = Array(11,14,20,30,40,50,60,70,90,100,110,120,130,140,150,160,170,180,190,200,210,220,230)
  
  val noData = List(230)
  val waterBodies = List(210); val ice = List(220); val bareAreas = List(200)
  val grassland = List(140); val sparseVegetation = List(150); val croplands = List(11, 14); val shrubland = List(130); val wetlands = List(180)
  val mosaicVegetationCropland = List(20, 30); val floodedAreas = List(160, 170); val mosaicGrasslandForestShrubland = List(110,120);
  val urbanAreas = List(190)
  val forests = List(40, 50, 60, 70, 90, 100)
  
}

object ModisCoverClasses extends LandCoverClasses("Modis", "modis/modisLegend") {
  val noData = List()

  val waterBodies = List(0); val ice = List(15); val bareAreas = List(16)
  val grassland = List(10); val sparseVegetation = List(); val croplands = List(12); val shrubland = List(6, 7); val wetlands = List(11)
  val mosaicVegetationCropland = List(14); val floodedAreas = List(); val mosaicGrasslandForestShrubland = List(9);
  val urbanAreas = List(13)
  val forests = List(1, 2, 3, 4, 5, 8)

}

/**
 * Do not use Corine land cover anymore as it is only available in Europe ..
 */
/*object CorineLandCoverClasses extends LandCoverClasses("CorineLandCover", "clc/clc2000legend", codeIndex = 0, labelIndex = 12, z0Index = 10) {
  val noData = List(0, 48, 49, 50, 255)
}*/
