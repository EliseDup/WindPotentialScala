package runs_history

import utils._
import squants.energy._
import squants.time.Hours
import squants.space._
import squants.radio._
import utils._
import grid._
import scala.io.Source
import java.io.PrintStream
import solar_energy.SolarTechnology

object AreaRepartition {
import utils.Helper._
  def main(args: Array[String]): Unit = {
    val list = Source.fromFile("../WindPotentialPython/landCover_0_1_coast_country").getLines().toList
    val cells = list.map(SimpleCell(_, Degrees(0.1))).toList
    printArea("Total", cells, 1/1E6,"\t" ,true)
    val countries =getLines("countries_students").map(_(0))
    countries.map(c => printArea(c, country(c,cells),1,"\t" ,false))
    
  }
  def country(c : String, cells : List[SimpleCell]) = cells.filter(_.country.equalsIgnoreCase(c))
  
  def printArea(name : String, list: List[SimpleCell], factor: Double = 1 / 1E6, sep: String, header: Boolean) {
    if (header)
      println("" + "\t" +"Total" + "\t" + "Sparse " + "\t" + "Forest" + "\t" + "Croplands" + "\t" +
        "Shrubland" + "\t" + "MosaicVegetationCroplands" + "\t" + "MosaicGrasslandForestShrubland" + "\t" + "Urban" + "\t" + "Flooded + Ice"+ "\t"+"Offshore")
    pr(name,sep)
    pr(list.map(i => i.area.toSquareKilometers).sum * factor, sep)
    pr(list.map(g => g.area(SparseVegetation) + g.area(Grassland) + g.area(BareAreas)).foldLeft(SquareKilometers(0))(_ + _).toSquareKilometers * factor, sep)
    pr(list.map(g => g.area(Forests)).foldLeft(SquareKilometers(0))(_ + _).toSquareKilometers * factor, sep)
    pr(list.map(g => g.area(CropLands)).foldLeft(SquareKilometers(0))(_ + _).toSquareKilometers * factor, sep)
    pr(list.map(g => g.area(Shrubland)).foldLeft(SquareKilometers(0))(_ + _).toSquareKilometers * factor, sep)
    pr(list.map(g => g.area(MosaicVegetationCroplands)).foldLeft(SquareKilometers(0))(_ + _).toSquareKilometers * factor, sep)
    pr(list.map(g => g.area(MosaicGrasslandForestShrubland)).foldLeft(SquareKilometers(0))(_ + _).toSquareKilometers * factor, sep)
    pr(list.map(g => g.area(UrbanAreas)).foldLeft(SquareKilometers(0))(_ + _).toSquareKilometers * factor, sep)
    pr(list.map(g => g.area(FloodedAreas) + g.area(WaterBodies) + g.area(Ice)).foldLeft(SquareKilometers(0))(_ + _).toSquareKilometers * factor, sep)
    pr(list.map(g => g.area(WaterBodies)).foldLeft(SquareKilometers(0))(_ + _).toSquareKilometers * factor, sep)
  
    println()
  }
    def pr(s: String, sep: String){print(s + sep)}
  def pr(s: Double, sep: String){print(s + sep)}
}

class SimpleCell(val center :GeoPoint, val resolution: Angle,val landCover: LandCoverType, val distanceToCoast: Length,
    val country: String){
    val onshore = distanceToCoast.value <= 0
    val offshore_EEZ = !onshore && !country.equals("NA")
    
      val area = Helper.areaRectangle(center, resolution)
  def area(lcType: LandCoverType): Area = if (lcType.equals(landCover)) area else SquareKilometers(0)

  }

object SimpleCell {
  def apply(line : String, resolution : Angle) =  {
    val i = line.split("\t")
    new SimpleCell(GeoPoint(Degrees(i(1).toDouble), Degrees(i(0).toDouble)), resolution,
      GlobCoverClasses.landCoverType(i(2).toDouble.toInt), Kilometers(i(3).toDouble),i(4).toString)
  }
}