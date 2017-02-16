

import gridData.WorldGrid
import squants.space.Degrees
import gridData.GridCell
import utils.PlotHelper
import org.jfree.data.category.DefaultCategoryDataset
import gridData.GlobCoverClasses
import gridData.WindPotential
import squants.space.SquareKilometers
import squants.radio.WattsPerSquareMeter
import utils.Thermodynamics
import squants.energy.Watts
import squants.space.NauticalMiles
import utils.TerawattHours

object LandCoverTest {

  def main(args: Array[String]): Unit = {
    val world = new WorldGrid("../WindPotentialPy/results/10years_grid", Degrees(0.75))
    val grids = world.onshoreGrids ++ world.grids.filter(_.offshore).filter(_.waterDepth.toMeters <= 1000)
    val be = grids.filter(_.country.name.contains("Belgium"))
    
    println(be.map(WindPotential.energyGeneratedPerYear(_)).foldLeft(TerawattHours(0))(_ + _).to(TerawattHours))
    
   println(grids.map(_.area.toSquareKilometers).sum / grids.size)
    println(grids.map(_.meanLatDistance.toKilometers).sum / grids.size)
    println(grids.map(_.meanLonDistance.toKilometers).sum / grids.size)
    
    println(world.area(grids)/world.totalArea)
    println( grids.map(g => WindPotential.suitabilityFactor(g) * g.area).foldLeft(SquareKilometers(0))(_ + _) / world.area(grids))
    println(world.area(world.offshoreGrids).toSquareKilometers / 1E6)
    val offshore = world.grids.filter(_.offshore).filter(_.waterDepth.toMeters <= 1000)
    
    val a = offshore.filter(_.distanceToCoast.toNauticalMiles <= 5)
    val b = offshore.filter(g => g.distanceToCoast.toNauticalMiles > 5 && g.distanceToCoast.toNauticalMiles <= 20)
    val c = offshore.filter(_.distanceToCoast.toNauticalMiles > 20)
    println(NauticalMiles(5).toKilometers)
    println(NauticalMiles(20).toKilometers)
    
    printRatio("< 5 nmi",a)
    printRatio("5 -20 nmi",b)
    printRatio(" > 20 nmi",c)
    printRatio("Protected", offshore.filter(_.protectedArea))
    printRatio("Total",offshore)
    def printRatio(name: String, grids: List[GridCell]) {
        val thisArea = world.area(grids)
        println(name + "\t" +
          thisArea.toSquareKilometers / 1E6 + "\t" +
          grids.map(g => WindPotential.suitabilityFactor(g) * g.area).foldLeft(SquareKilometers(0))(_ + _).toSquareKilometers / 1E6 + "\t" +
          grids.map(g => WindPotential.landUseFactor(g) * g.area / thisArea).sum + "\t" +
          grids.map(g => WindPotential.suitabilityFactor(g) * g.area / thisArea).sum)
          
    }
    def ressourcesByDepth(minDepth : Int, maxDepth : Int) = {
     val gr =  offshore.filter(g => g.waterDepth.toMeters  >= minDepth && g.waterDepth.toMeters < maxDepth)
     val area = world.area(gr)
     gr.map(g => Thermodynamics.powerDensity(g.windSpeed, g.altitude) * g.area).foldLeft(Watts(0))(_+_) / area
    }
     def ressourcesByDistance(min : Int, max : Int) = {
     val gr =  offshore.filter(g => g.distanceToCoast.toKilometers >= min && g.distanceToCoast.toKilometers < max)
     val area = world.area(gr)
     gr.map(g => g.windSpeed.toMetersPerSecond * g.area).foldLeft(SquareKilometers(0))(_+_) / area
    }
    val depth = ( 0 to 2688).toList
    val i = (0 until depth.size-1).toList
    PlotHelper.plotXY( 
        i.map(depth(_).toDouble).toList,
        i.map(j => (ressourcesByDistance(depth(j), depth(j+1)))).toList
        )
    
    def onshoreRepartition() {
      val onshore = world.grids.filter(_.onshore).filter(_.center.latitude.toDegrees >= -60)
      val antarctica = world.grids.filter(_.onshore).filter(_.center.latitude.toDegrees < -60)

      val area = world.area(onshore)
      val sparse = onshore.filter(g => g.lc.sparseVegetation || g.lc.bareAreas || g.lc.grassland)
      val forests = onshore.filter(_.lc.forests)
      val cropland = onshore.filter(_.lc.croplands)
      val mosaicGrasslandForestShrubland = onshore.filter(_.lc.mosaicGrasslandForestShrubland)
      val mosaicVegetationCropland = onshore.filter(_.lc.mosaicVegetationCropland)
      val other = onshore.filter(g => g.lc.floodedAreas || g.lc.ice || g.lc.urbanAreas || g.lc.waterBodies || g.lc.wetlands)
      val shrubland = onshore.filter(_.lc.shrubland)

      printRatio("Total", onshore)
      printRatio("Bio", onshore.filter(_.protectedArea))
      printRatio("Sparse Vegetation", sparse)
      printRatio("Forests", forests)
      printRatio("Mosaic Vegetation/Cropland", mosaicVegetationCropland)
      printRatio("Cropland", cropland)
      printRatio("Shrubland", shrubland)
      printRatio("Mosaic Grassland/Forest/Shruland", mosaicGrasslandForestShrubland)
      printRatio("Other", other)
      printRatio("Antarctica", antarctica)
      printRatio("Altitude", onshore.filter(_.elevation.toMeters > 2000))
      
    }
  }

}