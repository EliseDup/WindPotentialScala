import wind_energy.WindPotential
import grid.WorldGrid
import utils.Helper
import utils.TerawattHours
import squants.radio.WattsPerSquareMeter
import wind_energy.CapacityFactorCalculation
import utils.PlotHelper

object WindTest {
  import WindPotential._

  def main(args: Array[String]): Unit = {

    val w = WorldGrid()
    PlotHelper.plotXY(List(potential_eroi((1 to 20).map(_.toDouble).toList, true, w.grids, "")),
        xLabel="Energie Brute Produite [EJ/an]", yLabel = "TRE")
    val countries = Helper.getLines("../countries_test").map(i => i(0).toString)
    for(c <- countries) {
      val cells = w.country(c).filter(_.onshore)
      println(c + "\t" + 100*Helper.mean(cells.map(c=>(c,CapacityFactorCalculation(c)))))
      }
    for(c <- countries) {
      val cells = w.country(c).filter(_.onshore).filter(CapacityFactorCalculation(_) >= 0.15)
     // println(c + "\t" + Helper.mean(cells.map(c=>(c,CapacityFactorCalculation(c))))
//+ "\t" + Helper.suitableArea(cells, WindPotential).toSquareKilometers + "\t" + potentialFixedDensity(WattsPerSquareMeter(6.25), 1.0, cells, true, false).to(TerawattHours) )
    }
  }
}