

import utils.PlotHelper
import grid.WorldGrid
import solar_energy.SolarPotential
import squants.energy._
import utils._
import squants.time.Hours
import squants.space.SquareMeters
import squants.radio.WattsPerSquareMeter
import squants.space.SquareKilometers
import solar_energy.EmbodiedEnergyPV

object Test {
  def main(args: Array[String]): Unit = {

    println((EmbodiedEnergyPV.inputs(Gigawatts(1), Gigawatts(1) * 25 * Hours(365 * 24) * 0.15)).to(Petajoules))
    val w = WorldGrid.simple()
     val g = w.grids.filter(i => i.EEZ)
     
    PlotHelper.cumulativeDensity(List( (g.map(_.irradiance.month(0).toWattsPerSquareMeter),"January") ,(g.map(_.irradiance.month(6).toWattsPerSquareMeter),"July")),legend=true,
        xLabel = "% Sites", yLabel="Monthly Irradiance [W/m²]")
    
        w.writeGrid("jan_ju")
        
        
   

    println( g.map(_.keDissipation.toWattsPerSquareMeter*1.5).sum / g.size)
    
    val ke = (0 to 500).map(_ * 0.01).toList
    val res = (0 until ke.size).map(i =>
      if (i < ke.size - 1) w.grids.filter(g => (g.keDissipation.toWattsPerSquareMeter*1.5 > ke(i) && g.keDissipation.toWattsPerSquareMeter*1.5 <= ke(i + 1))).size.toDouble
      else w.grids.filter(g => g.keDissipation.toWattsPerSquareMeter*1.5 > ke(i)).size.toDouble).toList

    PlotHelper.plotXY(List( (ke, res, "")), xLabel = "Dissipation [W/m²]", yLabel= "Count of Grid Cells")

    PlotHelper.cumulativeDensity(
      List((w.grids.map(_.keDissipation.toWattsPerSquareMeter), "Total"),
        (g.map(_.keDissipation.toWattsPerSquareMeter), "Onshore + Offshore < 1000m")),
      yLabel = "Estimated KE Dissipation [W/m²]", xLabel = "% Areas", legend = true)

    val eroiMax1 = Math.ceil(g.map(i => SolarPotential.eff_17.eroi(i, 1, true)).max).toInt
    val eroiMax2 = Math.ceil(g.map(i => SolarPotential.eff_24.eroi(i, 1, true)).max).toInt

    PlotHelper.plotXY(List(
      SolarPotential.eff_17.potential_eroi((2 to 2 * eroiMax1).map(_ * 0.5).toList, true, g, "0.17"),
      SolarPotential.eff_24.potential_eroi((2 to 2 * eroiMax2).map(_ * 0.5).toList, true, g, "0.24")),
      xLabel = "Solar Potential [EJ/year]", yLabel = "EROI",
      legend = true)

  }
}