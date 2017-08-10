package grid

/**
 if (cell.onshore) {
        cell.landCovers.croplands * 0.7 +
          (cell.landCovers.grassland + cell.landCovers.sparseVegetation + cell.landCovers.bareAreas) * 0.9 +
          cell.landCovers.shrubland * 0.5 +
          cell.landCovers.mosaicGrasslandForestShrubland * (1.0/3*(0.1+0.5+0.8)) +
          cell.landCovers.mosaicVegetationCropland * (0.5 * 0.5 + 0.5 * 0.8)+
          cell.landCovers.forests * 0.1
      } 
*/

case class LandUseFactor(val min : Double, val max : Double, val mean : Double)
object LandUseFactor {
  def apply(mean :Double) = new LandUseFactor(mean,mean,mean)
}

abstract class LandCoverType(val name : String, val windFactor : LandUseFactor = LandUseFactor(0.0), val solarFactor : LandUseFactor = LandUseFactor(0.0))

object CropLands extends LandCoverType("Croplands", LandUseFactor(0.7), LandUseFactor(0.01))
object SparseVegetation extends LandCoverType("Sparse Vegetation", LandUseFactor(0.9), LandUseFactor(0.05))
object Grassland extends LandCoverType("Grassland", LandUseFactor(0.9), LandUseFactor(0.01))
object BareAreas extends LandCoverType("Bare Areas", LandUseFactor(0.9), LandUseFactor(0.05))
object Shrubland extends LandCoverType("Shrubland", LandUseFactor(0.5), LandUseFactor(0.01))
object MosaicGrasslandForestShrubland extends LandCoverType("Mosaic Grassland / Forest / Shrubland", LandUseFactor((1.0/3*(0.1+0.5+0.9))), LandUseFactor(0.01))
object MosaicVegetationCroplands extends LandCoverType("Mosaic Vegetation / Croplands", LandUseFactor((0.5 * 0.5 + 0.5 * 0.7)), LandUseFactor(0.01))
object Forests extends LandCoverType("Forest", LandUseFactor(0.1), LandUseFactor(0))
object UrbanAreas extends LandCoverType("Urban Areas")
object NoData extends LandCoverType("No Data")
object WaterBodies extends LandCoverType("Water Bodies")
object Ice extends LandCoverType("Ice")
object FloodedAreas extends LandCoverType("Flooded Areas / Wetlands")