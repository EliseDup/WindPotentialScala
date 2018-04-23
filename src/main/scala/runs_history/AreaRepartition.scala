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
    println("Start")
    val cells = Source.fromFile("../resources/landcover/landCover_0_1_final_sea").getLines().map(_.split("\t")).filter(i => i(4) != "NA").toList
    println("File Loaded -- " + cells.size)
  
    println("Model Built")
  
    val countries = getLines("countries_students").map(_(0))
    countries.map(c => printArea(c, country(c, cells), 1, "\t", false))
  }

  def country(c: String, cells: List[Array[String]]) = cells.filter(_(4).equalsIgnoreCase(c)).map(i => SimpleCell(i, Degrees(0.1)))

  def printArea(name: String, list: List[SimpleCell], factor: Double = 1 / 1E6, sep: String, header: Boolean) {
    if (header)
      println("" + "\t" + "Total" + "\t" + "Sparse " + "\t" + "Forest" + "\t" + "Croplands" + "\t" +
        "Shrubland" + "\t" + "MosaicVegetationCroplands" + "\t" + "MosaicGrasslandForestShrubland" + "\t" + "Urban" + "\t" + "Flooded + Ice" + "\t" + "Offshore")
    pr(name, sep)
    pr(list.map(i => i.area.toSquareKilometers).sum * factor, sep)
    pr(list.filter(_.onshore).map(i => i.area.toSquareKilometers).sum * factor, sep)
    pr(list.filter(_.offshore_EEZ).map(i => i.area.toSquareKilometers).sum * factor, sep)
    
    pr(list.map(g => g.area(SparseVegetation) + g.area(Grassland) + g.area(BareAreas)).foldLeft(SquareKilometers(0))(_ + _).toSquareKilometers * factor, sep)
    pr(list.map(g => g.area(Forests)).foldLeft(SquareKilometers(0))(_ + _).toSquareKilometers * factor, sep)
    pr(list.map(g => g.area(CropLands)).foldLeft(SquareKilometers(0))(_ + _).toSquareKilometers * factor, sep)
    pr(list.map(g => g.area(Shrubland)).foldLeft(SquareKilometers(0))(_ + _).toSquareKilometers * factor, sep)
    pr(list.map(g => g.area(MosaicVegetationCroplands)).foldLeft(SquareKilometers(0))(_ + _).toSquareKilometers * factor, sep)
    pr(list.map(g => g.area(MosaicGrasslandForestShrubland)).foldLeft(SquareKilometers(0))(_ + _).toSquareKilometers * factor, sep)
    pr(list.map(g => g.area(UrbanAreas)).foldLeft(SquareKilometers(0))(_ + _).toSquareKilometers * factor, sep)
    pr(list.map(g => g.area(FloodedAreas) + g.area(Ice)).foldLeft(SquareKilometers(0))(_ + _).toSquareKilometers * factor, sep)
    pr(list.map(g => g.area(WaterBodies)).foldLeft(SquareKilometers(0))(_ + _).toSquareKilometers * factor, sep)
    val offshore_200m = list.filter(_.offshore_EEZ).filter(i => math.abs(i.elevation.toMeters) <= 200)
    pr(offshore_200m.filter(_.distanceToCoast.toNauticalMiles <= 5).map(_.area).foldLeft(SquareKilometers(0))(_ + _).toSquareKilometers * factor, sep)
    pr(offshore_200m.filter(i => i.distanceToCoast.toNauticalMiles > 5 && i.distanceToCoast.toNauticalMiles <= 20).map(_.area).foldLeft(SquareKilometers(0))(_ + _).toSquareKilometers * factor, sep)
    pr(offshore_200m.filter(_.distanceToCoast.toNauticalMiles > 20).map(_.area).foldLeft(SquareKilometers(0))(_ + _).toSquareKilometers * factor, sep)
    val offshore_1000m = list.filter(_.offshore_EEZ).filter(i => math.abs(i.elevation.toMeters) <= 1000)
    pr(offshore_1000m.filter(_.distanceToCoast.toNauticalMiles <= 5).map(_.area).foldLeft(SquareKilometers(0))(_ + _).toSquareKilometers * factor, sep)
    pr(offshore_1000m.filter(i => i.distanceToCoast.toNauticalMiles > 5 && i.distanceToCoast.toNauticalMiles <= 20).map(_.area).foldLeft(SquareKilometers(0))(_ + _).toSquareKilometers * factor, sep)
    pr(offshore_1000m.filter(_.distanceToCoast.toNauticalMiles > 20).map(_.area).foldLeft(SquareKilometers(0))(_ + _).toSquareKilometers * factor, sep)
   
    println()
  }
  def pr(s: String, sep: String) { print(s + sep) }
  def pr(s: Double, sep: String) { print(s + sep) }
}

class SimpleCell(val center: GeoPoint, val resolution: Angle, val landCover: LandCoverType, val distanceToCoast: Length,
    val country: String, val elevation :Length) {
  val onshore = distanceToCoast.value <= 0
  val offshore_EEZ = !onshore && !country.equals("NA")

  val area = Helper.areaRectangle(center, resolution)
  def area(lcType: LandCoverType): Area = if (lcType.equals(landCover)) area else SquareKilometers(0)

}

object SimpleCell {
  def apply(i: Array[String], resolution: Angle) = {
    new SimpleCell(GeoPoint(Degrees(i(1).toDouble), Degrees(i(0).toDouble)), resolution,
      GlobCoverClasses.landCoverType(i(2).toDouble.toInt), Kilometers(i(3).toDouble), i(4).toString, Meters(i(5).toDouble))
  }
}