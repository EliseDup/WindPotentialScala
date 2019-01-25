package solar_energy

import utils.Helper
import squants.space.SquareKilometers
import squants.radio.WattsPerSquareMeter
import squants.energy.Watts
import utils.Terawatts
import squants.space.Area
import squants.radio.Irradiance
import squants.time.Hours
import utils.Exajoules
import utils._
import squants.energy.Joules
import squants.energy.Energy
import squants.energy.Megawatts

object RooftopPVPotential {

  import PlotHelper._
  import Helper._

  val grid = SolarGrid._0_5deg

  val rooftop_area = Helper.getLines("../resources/data_solar/rooftop_area", "\t").map(i =>
    (i(0).toString, SquareKilometers(i(1).toDouble), SquareKilometers(i(2).toDouble), i(3).toDouble, i(4).toDouble)).filter(i => grid.country(i._1).nonEmpty)

  val resources: List[(String, Irradiance, Area, Area, Double, Double)] = rooftop_area.map(c => {
    val cells = grid.country(c._1)
    (c._1, WattsPerSquareMeter(cells.map(_.ghi.toWattsPerSquareMeter).sum / cells.size), c._2, c._3, 0.25, 0.65)
  })
  def main(args: Array[String]): Unit = {

    resources.map(r => println(r._1 + "\t" + r._2.toWattsPerSquareMeter + "\t" + r._3.toSquareKilometers + "\t" + r._4.toSquareKilometers + "\t" + r._5 + "\t" + r._6))
    val area = (resources.map(i => i._3 * i._5 + i._4 * i._6).foldLeft(SquareKilometers(0))(_ + _))
    val meanGhi = (resources.map(_._2).foldLeft(WattsPerSquareMeter(0))(_ + _) / resources.size)
    resources.map(i => println(i._1 + "\t" + i._2.toWattsPerSquareMeter + "\t" + i._3.toSquareKilometers + "\t" + i._4.toSquareKilometers + "\t" + (netYearlyProductions(i._3 * i._5 + i._4 * i._6, i._2, PVMono) / Hours(365 * 24)).to(Megawatts)
      + "\t" + (potential(i._3 * i._5 + i._4 * i._6, i._2, PVMono) / Hours(365 * 24)).to(Megawatts) + "\t" + PVMono.eroi(i._2) + "\t" + PVMono.lifeTimeEfficiency(WattsPerSquareMeter(1000))))
    val pvMonoRes = listValueVSCumulated(resources.map(i => (PVMono.eroi(i._2), netYearlyProductions(i._3 * i._5, i._2, PVMono).to(Exajoules))))
    val pvMonoCom = listValueVSCumulated(resources.map(i => (PVMono.eroi(i._2), netYearlyProductions(i._4 * i._6, i._2, PVMono).to(Exajoules))))
    println(pvMonoRes._1.max)
    println(pvMonoCom._1.max)

    val pvPolyRes = listValueVSCumulated(resources.map(i => (PVPoly.eroi(i._2), netYearlyProductions(i._3 * i._5, i._2, PVPoly).to(Exajoules))))
    val pvPolyCom = listValueVSCumulated(resources.map(i => (PVPoly.eroi(i._2), netYearlyProductions(i._4 * i._6, i._2, PVPoly).to(Exajoules))))
    println(pvPolyRes._1.max)
    println(pvPolyCom._1.max)

    plotXY(List((pvMonoRes._1, pvMonoRes._2, "Residential"), (pvMonoCom._1, pvMonoCom._2, "Commercial")), legend = true, xLabel = "mono-Si-PV, Net Potential [EJ/year]", yLabel = "EROI", title = "rooftop_potential_mono")
    plotXY(List((pvPolyRes._1, pvPolyRes._2, "Residential"), (pvPolyCom._1, pvPolyCom._2, "Commercial")), legend = true, xLabel = "poly-Si-PV, Net Potential [EJ/year]", yLabel = "EROI", title = "rooftop_potential_poly")

  }

  def potential(area: Area, irradiance: Irradiance, tech: SolarTechnology): Energy = {
    tech.lifeTimeEfficiency(irradiance) * area * irradiance * Hours(365 * 24)
  }
  def capacityFactor(area: Area, irradiance: Irradiance, tech: SolarTechnology): Double = {
    potential(area, irradiance, tech) / (tech.ratedPower(area, irradiance) * Hours(365 * 24))
  }
  def netYearlyProductions(area: Area, irradiance: Irradiance, tech: SolarTechnology): Energy = {
    
    val power = tech.ratedPower(area, irradiance)
    val gross = potential(area, irradiance, tech)
    gross - tech.embodiedEnergy(power, gross, area) / tech.lifeTime
  }

}
  