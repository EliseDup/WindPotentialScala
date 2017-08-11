import wind_energy.WindPotential
import grid.WorldGrid
import utils.Helper
import utils.TerawattHours
import squants.radio.WattsPerSquareMeter
import wind_energy.CapacityFactorCalculation
import utils.PlotHelper
import squants.space.Degrees

object WindTest {
  import WindPotential._

  def main(args: Array[String]): Unit = {
  
    val e = (0 until 10).map(_ * 2.0 + 1).toList
    val w = new WorldGrid("../model_data/Wind_optimization/1_19_by2_dissipation", Degrees(0.75), e)
   // val w2 = new WorldGrid("../model_data/Wind_optimization/1_19_by2_double_dissipation", Degrees(0.75), e)
  //  val w3 = new WorldGrid("../model_data/Wind_optimization/1_19_by2_1Wem2", Degrees(0.75), e)
    val w4 = new WorldGrid("../model_data/Wind_optimization/1_19_by2_1_5_dissipation", Degrees(0.75), e)
    
    //  w.optimizationInputs("optimization_inputs")
    val p = WindPotential()

    PlotHelper.plotXY(List(p.potential_eroi(e, true, w.grids, "KE Dissipation"),
      p.potential_eroi(e, true, w4.grids, "1.5 * Dissipation")),
     // p.potential_eroi(e, true, w2.grids, "Double KE Dissipation"),
     // p.potential_eroi(e, true, w3.grids, "1 We/m2")),
      xLabel = "Gross Potential [EJ/year]", yLabel = "EROI", legend = true)

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