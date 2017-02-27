package grid

import utils.Helper
import org.apache.poi.hssf.usermodel.HSSFRow
import squants.space._

// For each grid cell we divided into a subgrid to have a more precise image of the land cover
class DetailedLandCover(val indexes: List[(Double, Int)]) {
  val types = indexes.filter(_._1 > 0).map(i => (i._1, GlobCoverClasses.landCoverType(i._2)))
  val suitabilityFactorWind = types.map(i => i._1 * i._2.windFactor.mean).sum
  val suitabilityFactorSolar = types.map(i => i._1 * i._2.solarFactor.mean).sum
}

abstract class LandCoverClasses(val name: String, legendFileName: String, codeIndex: Int = 0, labelIndex: Int = 1, z0Index: Int = 2) {
  def landCoverType(index: Int): LandCoverType = NoData

  def classes: Map[Int, LandCoverClass] = {
    val sheet = Helper.xlsSheet(Helper.ressourcesPy + "/landCover/" + legendFileName + ".xls", 0)
    (1 to sheet.getLastRowNum).map(r => {
      val c = landCoverClass(sheet.getRow(r))
      (c.code -> c)
    }).toMap
  }
  def landCoverClass(row: HSSFRow): LandCoverClass = new LandCoverClass(Helper.toInt(row, codeIndex), Helper.toString(row, labelIndex), Meters(Helper.toDouble(row, z0Index)), this)
  def apply(c: Int) = classes(c)
}
class LandCoverClass(val code: Int, val label: String, val z0: Length, val classes: LandCoverClasses) {
  override def toString() = "Land Cover Class " + code + " : " + label + "," + z0
}

object GlobCoverClasses extends LandCoverClasses("GlobCover2009", "globCover/GlobCover2009_Legend", z0Index = 5) {

  val indexes = Array(11, 14, 20, 30, 40, 50, 60, 70, 90, 100, 110, 120, 130, 140, 150, 160, 170, 180, 190, 200, 210, 220, 230)
val types = List(CropLands,SparseVegetation,Grassland,BareAreas,Shrubland,MosaicGrasslandForestShrubland,
    MosaicVegetationCroplands, Forests, UrbanAreas, NoData, WaterBodies, Ice, FloodedAreas)
    
  override def landCoverType(index: Int) = {

    if (index == 14) CropLands
    else if (List(20, 30).contains(index)) MosaicVegetationCroplands
    else if (List(40, 50, 60, 70, 90, 100).contains(index)) Forests
    else if (List(110, 120).contains(index)) MosaicGrasslandForestShrubland
    else if(List(11,160,170,180).contains(index)) FloodedAreas
    else if(index == 130) Shrubland
    else if(index == 140) Grassland
    else if(index == 150) SparseVegetation
    else if(index == 190) UrbanAreas
    else if(index == 200) BareAreas
    else if(index == 210) WaterBodies
    else if(index == 220) Ice
    else NoData
    
  }
  
  /*  
  val noData = List(230)
  val waterBodies = List(210); val ice = List(220); val bareAreas = List(200)
  val grassland = List(140); val sparseVegetation = List(150); val croplands = List(11, 14); val shrubland = List(130); val wetlands = List(180)
  val mosaicVegetationCropland = List(20, 30); val floodedAreas = List(160, 170); val mosaicGrasslandForestShrubland = List(110,120);
  val urbanAreas = List(190)
  val forests = List(40, 50, 60, 70, 90, 100)
*/
}

object ModisCoverClasses extends LandCoverClasses("Modis", "modis/modisLegend") {
  val noData = List()

  val waterBodies = List(0); val ice = List(15); val bareAreas = List(16)
  val grassland = List(10); val sparseVegetation = List(); val croplands = List(12); val shrubland = List(6, 7); val wetlands = List(11)
  val mosaicVegetationCropland = List(14); val floodedAreas = List(); val mosaicGrasslandForestShrubland = List(9);
  val urbanAreas = List(13)
  val forests = List(1, 2, 3, 4, 5, 8)

}