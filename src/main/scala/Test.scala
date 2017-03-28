

import wind_energy._
import squants.motion._
import squants.space._
import grid._
import squants.time.Hours
import org.apache.commons.math3.special.Gamma
import org.jfree.chart.plot.FastScatterPlot
import org.jfree.chart.axis.NumberAxis
import org.jfree.chart.ChartFactory
import squants.energy._
import utils._
import squants.radio.WattsPerSquareMeter
import org.junit.experimental.theories.PotentialAssignment
import squants.radio.Irradiance
import java.io.PrintStream
import org.joda.time.Years

object Test {
  def main(args: Array[String]): Unit = {
  
    
  }

  def plotPotentialVSCapacity(grids: List[GridCell], erois: List[Double]) {

    val cd = (1 to 20).map(_.toDouble).toList
    val tot = cd.map(c => grids.map(g => (WindPotential.energyGeneratedPerYear(g, None, Some(WattsPerSquareMeter(c))))).foldLeft(Joules(0))(_ + _).to(Exajoules))
    val resEroi = erois.map(eroi => (cd, cd.map(c => grids.filter(WindPotential.EROI(_, None, Some(WattsPerSquareMeter(c))) >= eroi).map(g => (WindPotential.energyGeneratedPerYear(g, None, Some(WattsPerSquareMeter(c))))).foldLeft(Exajoules(0))(_ + _).to(Exajoules)), "EROI >" + eroi.toString))

    val gcm =
      cd.map(c => grids.map(g => (
        Math.min(
          (WattsPerSquareMeter(1.0) * WindPotential.suitabilityFactor(g) * g.area * Hours(365 * 24)).to(Exajoules),
          WindPotential.energyGeneratedPerYear(g, None, Some(WattsPerSquareMeter(c))).to(Exajoules)))).sum)

    PlotHelper.plotXY(List((cd, tot, "World"), (cd, gcm, "GCM")) ++ resEroi, legend = true, xLabel = "Installed Capacity Density [MW/km2]", yLabel = "Wind Potential [EJ/year]")
  }

  // From space between turbine and grid cell -> find density = Power / (n*D)^2
  def ndensity(cell: GridCell, n: Int) = WattsPerSquareMeter(WindPotential.nominalPower(cell).toWatts / Math.pow(WindPotential.diameterRotor(cell).toMeters * n, 2))

  def potentialVSCapacity(grids: List[GridCell], eroiMin: Double) = {
    val d = (1 to 100).map(_ * 0.1).toList
    d.map(i => {
      val gr = grids.filter(g => WindPotential.EROI(g, density = Some(WindPotential.capacityDensity(g, i))) >= eroiMin)
      gr.map(g => WindPotential.energyGeneratedPerYear(g, density = Some(WindPotential.capacityDensity(g, i)))).foldLeft(Joules(0))(_ + _).to(Exajoules)
    })
  }

  // PlotHelper.plotXY(List((d, potentialVSCapacity(3), "3"), (d, potentialVSCapacity(5), "5"), (d, potentialVSCapacity(7), "7")))
  /*
  def EROImin(grids : List[GridCell], eroi: Double) {
    // Minimum wind speed to have this EROI ( assuming a weibull with k= 2.0, c = windSpeed / Gamma(1+1/k) )
    // EROI = (CF * 1 MW * 20 years) / 15.OOO GJ => CF_min = EROI_min * 15.OOO GJ / ( 1 MW * 20 years )
    val CFmin = eroi * Gigajoules(15000) / (Megawatts(1) * Hours(20 * 365 * 24))
    println("EROI min : " + eroi + "\t" + "CF min :" + CFmin + "\t" + "v min : " + CapacityFactorCalculation.meanSpeed(CFmin))
    val gr = grids.filter(WindPotential.EROI(_) >= eroi)
    val onshoreGr = world.onshoreGrids.filter(WindPotential.EROI(_) >= eroi)
    val offshoreGr = offshore1000m.filter(WindPotential.EROI(_) >= eroi)
    println("Potential" + "\t" + gr.map(WindPotential.energyGeneratedPerYear(_)).foldLeft(Joules(0))(_ + _).to(Exajoules))
    println("Potential Onshore" + "\t" + onshoreGr.map(WindPotential.energyGeneratedPerYear(_)).foldLeft(Joules(0))(_ + _).to(Exajoules))
    println("Potential Offshore" + "\t" + offshoreGr.map(WindPotential.energyGeneratedPerYear(_)).foldLeft(Joules(0))(_ + _).to(Exajoules))
  }

  def list(grids : List[GridCell],d: Option[Irradiance]) = {
    (world.listEROIVSCumulatedPower(grids.map(g => (g, 1.0)), WindPotential, d), d.toString)
  }
  def listSuitable(d: Option[Irradiance]) = {
    (world.listEROIVSCumulatedPower(grids.map(g => (g, WindPotential.suitabilityFactor(g))), WindPotential, d), d.toString + " - Land Use")
  }

  def plotEROI(world : gridData, grids : List[GridCell], cd: Double = 2.0) {
    PlotHelper.plotXY(
      List(
        (world.listEROIVSCumulatedProduction(grids.map(g => (g, WindPotential.suitabilityFactor(g))), WindPotential, Some(WattsPerSquareMeter(cd))), "Total Constrained"),
        (world.listEROIVSCumulatedProduction(world.onshoreGrids.map(g => (g, WindPotential.suitabilityFactor(g))), WindPotential, Some(WattsPerSquareMeter(cd))), "Onshore Constrained"),
        (world.listEROIVSCumulatedProduction(world.offshoreGrids(Meters(1000)).map(g => (g, WindPotential.suitabilityFactor(g))), WindPotential, Some(WattsPerSquareMeter(cd))), "Offshore Constrained")),
      xLabel = "Wind Potential [EJ/year]", yLabel = "EROI")
  }
*/
  def windSpeedAt(windSpeed: Velocity, height: Length, z0: Length): Velocity = Math.log(height / z0) / Math.log(Meters(10) / z0) * windSpeed

  def productionVSInstalledCapacityDensity(grids: List[GridCell], world: WorldGrid) {
    val cd = (1 to 1500).map(_ * 0.01).toList
    val vke = cd.map(c => grids.map(g => (WindPotential.energyGeneratedPerYear(g, None, Some(WattsPerSquareMeter(c))))).foldLeft(Exajoules(0))(_ + _).to(Exajoules))

    PlotHelper.plotXY(List((cd, vke, "")), xLabel = "Capacity Density [MW/km2]", yLabel = "Wind Potential [EJ/year]")
  }

  def lambda(nD: Double) = Math.PI / (4 * nD * nD)
  def plotGustavson {
    val n = (1 to 2999).map(_ * 0.01).toList
    PlotHelper.plotXY(
      List((n, n.map(m => GustavsonWakeEffect.arrayEfficiency(5, lambda(m))), "5x5"),
        (n, n.map(m => GustavsonWakeEffect.arrayEfficiency(50, lambda(m))), "10x10"),
        (n, n.map(m => GustavsonWakeEffect.arrayEfficiency(200, lambda(m))), "50x50"),
        (n, n.map(m => GustavsonWakeEffect.arrayEfficiency(500, lambda(m), inf = true)), "Infinite")),
      xLabel = "Turbine Spacing [nD]", yLabel = "Array Efficiency")
  }
  def testOptimaleCD {
    val cd = (1 to 300).map(_ * 0.1).toList

    val a = SquareKilometers(1000)
    val d = Meters(80)
    val cfs = List(0.2, 0.3, 0.4, 0.5)
    val p = cfs.map(cf => {
      cd.map(c => {
        val nT = WattsPerSquareMeter(c) * a / Megawatts(2)
        val n = Math.sqrt(Megawatts(2) / (WattsPerSquareMeter(c) * (d * d)))
        nT * Megawatts(2) * cf * Hours(365 * 24) * GustavsonWakeEffect.arrayEfficiency(nT, Math.PI / (4 * n * n))
      })
    })
    PlotHelper.plotXY(p.zipWithIndex.map(i => (cd.map(_.toDouble), i._1.map(_.to(Megajoules)), cfs(i._2).toString)), legend = true)
  }

}