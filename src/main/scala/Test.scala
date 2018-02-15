

import utils.PlotHelper
import grid.WorldGrid
import solar_energy.SolarPotential
import solar_energy.SolarPower
import squants.energy._
import utils._
import squants.time.Hours
import squants.space._
import squants.radio._
import solar_energy.EmbodiedEnergyPV
import grid._
import scala.io.Source
import java.io.PrintStream

object Test {
  import DayMonth._
  import SolarPower._
  import PlotHelper._
  def main(args: Array[String]): Unit = {

    val l = Source.fromFile("../data_solar/solar_countries_0_05deg").getLines().toList
   
    val list = l.map(SolarCell(_)).toList
    println(list.size)
   
    println(area(list).toSquareKilometers/1E6)
    println(list.map(_.suitableArea.toSquareKilometers).sum/1E6)
    println("Sparse " + list.map(g=> g.area(SparseVegetation)+g.area(Grassland)+g.area(BareAreas)).foldLeft(SquareKilometers(0))(_ + _).toSquareKilometers/1E6)
    println("Forest " + list.map(g=> g.area(Forests)).foldLeft(SquareKilometers(0))(_ + _).toSquareKilometers/1E6)
    println("Croplands "+ list.map(g=> g.area(CropLands)).foldLeft(SquareKilometers(0))(_ + _).toSquareKilometers/1E6)
    println("Shrubland " + list.map(g=> g.area(Shrubland)).foldLeft(SquareKilometers(0))(_ + _).toSquareKilometers/1E6)
    println("MosaicVegetationCroplands "+ list.map(g=> g.area(MosaicVegetationCroplands)).foldLeft(SquareKilometers(0))(_ + _).toSquareKilometers/1E6)
    println("MosaicGrasslandForestShrubland" + list.map(g=> g.area(MosaicGrasslandForestShrubland)).foldLeft(SquareKilometers(0))(_ + _).toSquareKilometers/1E6)
    println("Urban " + list.map(g=> g.area(UrbanAreas)).foldLeft(SquareKilometers(0))(_ + _).toSquareKilometers/1E6)
    println("Flooded + Waters + Ice "+ list.map(g=> g.area(FloodedAreas)+g.area(WaterBodies)+ g.area(Ice)).foldLeft(SquareKilometers(0))(_ + _).toSquareKilometers/1E6)
    
    
    def area(list : List[SolarCell]) = list.map(_.area).foldLeft(SquareKilometers(0))(_ + _)
    def suitableArea(list : List[SolarCell]) = list.map(_.suitableArea).foldLeft(SquareKilometers(0))(_ + _)
  
  
   def plotEROI(w: WorldGrid, g: List[GridCell]) {

      PlotHelper.cumulativeDensity(List((g.map(_.irradiance.month(0).toWattsPerSquareMeter), "January"), (g.map(_.irradiance.month(6).toWattsPerSquareMeter), "July")), legend = true,
        xLabel = "% Sites", yLabel = "Monthly Irradiance [W/m²]")
      w.writeGrid("jan_ju")

      val eroiMax1 = Math.ceil(g.map(i => SolarPotential.eff_17.eroi(i, 1, true)).max).toInt
      val eroiMax2 = Math.ceil(g.map(i => SolarPotential.eff_24.eroi(i, 1, true)).max).toInt

      PlotHelper.plotXY(List(
        SolarPotential.eff_17.potential_eroi((2 to 2 * eroiMax1).map(_ * 0.5).toList, true, g, "0.17"),
        SolarPotential.eff_24.potential_eroi((2 to 2 * eroiMax2).map(_ * 0.5).toList, true, g, "0.24")),
        xLabel = "Solar Potential [EJ/year]", yLabel = "EROI",
        legend = true)

    }
   
  }

}

class SolarCell(val center: GeoPoint, val ghi: Irradiance, val dni: Irradiance, val landCover: LandCoverType, val country : String) {
  val area = Helper.areaRectangle(center, Degrees(0.05))
  val suitableArea = landCover.solarFactor.mean *area
  def area(lcType: LandCoverType): Area = if (lcType.equals(landCover)) area else SquareKilometers(0)
}

object SolarCell {
  def apply(line: String) = {
    val i = line.split("\t")
    new SolarCell(GeoPoint(Degrees(i(1).toDouble), Degrees(i(0).toDouble)),
      WattsPerSquareMeter(i(2).toDouble / 24 * 1000), WattsPerSquareMeter(i(3).toDouble / 24 * 1000), GlobCoverClasses.landCoverType(i(4).toInt), i(5).toString)
  }
}

class Technology(val efficiency: Double, val performanceRatio: Double, val occupationRatio: Double)