import wind_energy.WindPotential
import grid.WorldGrid
import utils.Helper
import utils.TerawattHours
import squants.radio.WattsPerSquareMeter
import wind_energy.CapacityFactorCalculation
import utils.PlotHelper
import squants.space.Degrees
import utils.Exajoules
import utils.PetawattHours

object WindTest {

  def main(args: Array[String]): Unit = {
  
    println(TerawattHours(586750).to(Exajoules))
  println(PetawattHours(755.48).to(Exajoules))
  val e = (2 to 40).map(_*0.5).toList
  val w = WorldGrid()
  val g = w.grids.filter(_.EEZ)
PlotHelper.plotXY(List(WindPotential().potential_eroi(e, true, g, "Cp,max=0.5"),
    WindPotential(16.0/27).potential_eroi(e, true, g, "Betz Limit")), xLabel = "Wind Potential [EJ/year]", yLabel="EROImin")
  /*
   *   val countries = Helper.getLines("../countries_test").map(i => i(0).toString)
    for (c <- countries) {
      val cells = w.country(c).filter(_.onshore)
      println(c + "\t" + 100 * Helper.mean(cells.map(c => (c, CapacityFactorCalculation(c)))))
    }
    for (c <- countries) {
      val cells = w.country(c).filter(_.onshore).filter(CapacityFactorCalculation(_) >= 0.15)
      // println(c + "\t" + Helper.mean(cells.map(c=>(c,CapacityFactorCalculation(c))))
      //+ "\t" + Helper.suitableArea(cells, WindPotential).toSquareKilometers + "\t" + potentialFixedDensity(WattsPerSquareMeter(6.25), 1.0, cells, true, false).to(TerawattHours) )
    }*/
  }
}