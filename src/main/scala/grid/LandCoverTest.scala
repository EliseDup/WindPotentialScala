package grid

import squants.space.Degrees
import utils.PlotHelper
import org.jfree.data.category.DefaultCategoryDataset
import squants.space.SquareKilometers
import squants.radio.WattsPerSquareMeter
import utils.Thermodynamics
import squants.energy.Watts
import squants.space.NauticalMiles
import utils.TerawattHours
import squants.space.Area
import utils.Helper
import wind_energy.WindPotential

object LandCoverTest {

  def main(args: Array[String]): Unit = {

    val world = WorldGrid.simple()
    val grids = world.grids
   // world.writeGrid("simple")
    val onshore = world.onshoreGrids
    val offshore = world.offshoreGrids
    val eez = offshore.filter(_.EEZ)
    
    println(grids.map(g => g.area*WindPotential().suitabilityFactor(g)).foldLeft(SquareKilometers(0))(_ + _).toSquareKilometers/1E6)
    println(Helper.area(offshore).toSquareKilometers/1E6 + "\t" +Helper.area(eez).toSquareKilometers/1E6)
    
    println("Offshore suitable " + offshore.map(g => g.area*WindPotential().suitabilityFactor(g)).foldLeft(SquareKilometers(0))(_ + _).toSquareKilometers/1E6)
    
    println(" < 5 NM " + Helper.area(eez.filter(_.distanceToCoast.toNauticalMiles < 5)).toSquareKilometers/1E6)
    println(" 5 - 20 NM " +Helper.area(eez.filter(g => g.distanceToCoast.toNauticalMiles >= 5 && g.distanceToCoast.toNauticalMiles <= 20)).toSquareKilometers/1E6)
    println(" > 20 NM " +Helper.area(eez.filter(_.distanceToCoast.toNauticalMiles > 20)).toSquareKilometers/1E6)
    println("Depth > 1000 m " +Helper.area(eez.filter(_.waterDepth.toMeters > 1000)).toSquareKilometers/1E6)
        
    
    println(Helper.area(onshore).toSquareKilometers/1E6 + "\t" + Helper.area(offshore).toSquareKilometers/1E6 + "\t" +Helper.area(offshore.filter(_.EEZ)).toSquareKilometers/1E6)
    println("Sparse " + onshore.map(g=> g.area(SparseVegetation)+g.area(Grassland)+g.area(BareAreas)).foldLeft(SquareKilometers(0))(_ + _).toSquareKilometers/1E6)
    println("Forest " + onshore.map(g=> g.area(Forests)).foldLeft(SquareKilometers(0))(_ + _).toSquareKilometers/1E6)
    println("Croplands "+ onshore.map(g=> g.area(CropLands)).foldLeft(SquareKilometers(0))(_ + _).toSquareKilometers/1E6)
    println("Shrubland " + onshore.map(g=> g.area(Shrubland)).foldLeft(SquareKilometers(0))(_ + _).toSquareKilometers/1E6)
    println("MosaicVegetationCroplands "+ onshore.map(g=> g.area(MosaicVegetationCroplands)).foldLeft(SquareKilometers(0))(_ + _).toSquareKilometers/1E6)
    println("MosaicGrasslandForestShrubland" + onshore.map(g=> g.area(MosaicGrasslandForestShrubland)).foldLeft(SquareKilometers(0))(_ + _).toSquareKilometers/1E6)
    println("Urban " + onshore.map(g=> g.area(UrbanAreas)).foldLeft(SquareKilometers(0))(_ + _).toSquareKilometers/1E6)
    println("Flooded + Waters + Ice "+ onshore.map(g=> g.area(FloodedAreas)+g.area(WaterBodies)+ g.area(Ice)).foldLeft(SquareKilometers(0))(_ + _).toSquareKilometers/1E6)
    
    println("Protected "+onshore.map(g=> g.protectedArea*g.area).foldLeft(SquareKilometers(0))(_ + _).toSquareKilometers/1E6)
    val excludedCountries = List("NA", "Antarctica", "Greenland", "French Southern & Antarctic Lands")
    
   // println("Altitude "+onshore.filter(g => !excludedCountries.contains(g.country.name) && g.altitude.toMeters > 2000).map(_.area).foldLeft(SquareKilometers(0))(_ + _).toSquareKilometers/1E6)
    println("Excluded Countries "+onshore.filter(cell => excludedCountries.contains(cell.country.name) || cell.country.name.contains("Is.") || cell.country.name.contains("Islands")).map(_.area).foldLeft(SquareKilometers(0))(_ + _).toSquareKilometers/1E6)
    
    println("Excluded Total " +onshore.filter(g => WindPotential().suitabilityFactor(g) == 0).map(_.area).foldLeft(SquareKilometers(0))(_ + _).toSquareKilometers/1E6)
    println("Suitable Total " +onshore.map(_.suitableArea(WindPotential())).foldLeft(SquareKilometers(0))(_ + _).toSquareKilometers/1E6)
  
    
    
    
  }
  def printRatio(name: String, grids: List[GridCell]) {
    val thisArea = Helper.area(grids)
    println(name + "\t" +
      thisArea.toSquareKilometers / 1E6 + "\t" +
      grids.map(g => WindPotential().suitabilityFactor(g) * g.area).foldLeft(SquareKilometers(0))(_ + _).toSquareKilometers / 1E6 + "\t" +
      grids.map(g => WindPotential().landUseFactor(g) * g.area / thisArea).sum + "\t" +
      grids.map(g => WindPotential().suitabilityFactor(g) * g.area / thisArea).sum)

  }
  def ressourcesByDepth(offshore: List[GridCell], minDepth: Int, maxDepth: Int) = {
    val gr = offshore.filter(g => g.waterDepth.toMeters >= minDepth && g.waterDepth.toMeters < maxDepth)
    val area = Helper.area(gr)
    gr.map(g => Thermodynamics.windPowerDensity(g.wind125m.mean, g.altitude) * g.area).foldLeft(Watts(0))(_ + _) / area
  }
  def ressourcesByDistance(offshore: List[GridCell], min: Int, max: Int) = {
    val gr = offshore.filter(g => g.distanceToCoast.toKilometers >= min && g.distanceToCoast.toKilometers < max)
    val area = Helper.area(gr)
    gr.map(g => g.wind125m.mean.toMetersPerSecond * g.area).foldLeft(SquareKilometers(0))(_ + _) / area
  }
  def onshoreRepartition(world : WorldGrid) {
    val onshore = world.grids.filter(_.onshore)
    val antarctica = world.grids.filter(_.onshore).filter(_.country.name.equals("Antarctica"))

    val area = Helper.area(onshore)
    printRatio("Total", onshore)
    // printRatio("Bio", onshore.filter(_.protectedArea))
    def printRes(name: String, area: Area, totalArea: Area) {
      println(name + "\t" + area.toSquareKilometers / 1E6 + "\t" + area / totalArea + "\t " + "%")
    }
    printRatio("Antarctica", antarctica)
    printRatio("Altitude", onshore.filter(_.elevation.toMeters > 2000))

  }

}