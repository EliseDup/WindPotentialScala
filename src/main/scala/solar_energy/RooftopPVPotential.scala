package solar_energy

import utils.Helper
import squants.space.SquareKilometers
import squants.radio.WattsPerSquareMeter
import squants.energy._
import utils.Terawatts
import squants.space.Area
import squants.radio.Irradiance
import squants.time.Hours
import utils.Exajoules
import utils._
import squants.energy.Joules
import squants.energy.Energy
import squants.energy.Megawatts
import java.io.PrintStream
import squants.energy.Power

object RooftopPVPotential {

  import PlotHelper._
  import Helper._

  val grid = SolarGrid._0_5deg
  // File with, per line: Country, Area PV Residential, Area PV Commercial, Suitability Factor Residential, Suitability Factor Commercial
  val rooftop_area = Helper.getLines("runs_history/solar_2019/rooftop_area", "\t").map(i =>
    (i(0).toString, SquareKilometers(i(1).toDouble), SquareKilometers(i(2).toDouble), i(3).toDouble, i(4).toDouble)).filter(i => grid.country(i._1).nonEmpty)
  val resources: List[(String, Irradiance, Area, Area, Double, Double)] = rooftop_area.map(c => {
    val cells = grid.country(c._1)
    (c._1, WattsPerSquareMeter(cells.map(_.ghi.toWattsPerSquareMeter).sum / cells.size), c._2, c._3, 0.25, 0.65)
  })

  val eu28countries = Helper.getLines("../model_data/countries/EU28", "\t").map(_(0)) ++ List("Norway", "Switzerland")
  val eu28 = resources.filter(i => eu28countries.contains(i._1))

  def main(args: Array[String]): Unit = {

  }

  def printResults(res : List[(String, Irradiance, Area, Area, Double, Double)]) {
    res.map(r => println(r._1 + "\t" + r._2.toWattsPerSquareMeter + "\t" + r._3.toSquareKilometers + "\t" + r._4.toSquareKilometers + "\t" + r._5 + "\t" + r._6))
    val area = (res.map(i => i._3 * i._5 + i._4 * i._6).foldLeft(SquareKilometers(0))(_ + _))
    val meanGhi = (res.map(_._2).foldLeft(WattsPerSquareMeter(0))(_ + _) / res.size)
    res.map(i => println(i._1 + "\t" + i._2.toWattsPerSquareMeter + "\t" + i._3.toSquareKilometers + "\t" + i._4.toSquareKilometers + "\t" + (potential(i._3 * i._5 + i._4 * i._6, i._2, PVMono) / Hours(365 * 24)).to(Megawatts)
      + "\t" + (potential(i._3 * i._5 + i._4 * i._6, i._2, PVMono) / Hours(365 * 24)).to(Megawatts) + "\t" + PVMono.eroi(i._2) + "\t" + PVMono.lifeTimeEfficiency(WattsPerSquareMeter(1000))))
    val pvMonoRes = listValueVSCumulated(res.map(i => (PVMono.eroi(i._2), potential(i._3 * i._5, i._2, PVMono).to(Exajoules))))
    val pvMonoCom = listValueVSCumulated(res.map(i => (PVMono.eroi(i._2), potential(i._4 * i._6, i._2, PVMono).to(Exajoules))))
    val pvMonoTot = listValueVSCumulated(res.map(i => (PVMono.eroi(i._2), potential(i._4 * i._6 + i._3 * i._5, i._2, PVMono).to(Exajoules))))

    println(pvMonoRes._1.max)
    println(pvMonoCom._1.max)

    val pvPolyRes = listValueVSCumulated(res.map(i => (PVPoly.eroi(i._2), potential(i._3 * i._5, i._2, PVPoly).to(Exajoules))))
    val pvPolyCom = listValueVSCumulated(res.map(i => (PVPoly.eroi(i._2), potential(i._4 * i._6, i._2, PVPoly).to(Exajoules))))
    val pvPolyTot = listValueVSCumulated(res.map(i => (PVPoly.eroi(i._2), potential(i._4 * i._6 + i._3 * i._5, i._2, PVPoly).to(Exajoules))))

    println(pvPolyRes._1.max)
    println(pvPolyCom._1.max)

   // plotXY(List((pvMonoTot._1, pvMonoTot._2, "Total"), (pvMonoRes._1, pvMonoRes._2, "Residential"), (pvMonoCom._1, pvMonoCom._2, "Commercial")), legend = true, xLabel = "Electricity Produced [EJ/year]", yLabel = "EROI", title = "rooftop_potential_mono")
   // plotXY(List((pvPolyTot._1, pvPolyTot._2, "Total"), (pvPolyRes._1, pvPolyRes._2, "Residential"), (pvPolyCom._1, pvPolyCom._2, "Commercial")), legend = true, xLabel = "Electricity Produced [EJ/year]", yLabel = "EROI", title = "rooftop_potential_poly")
  
    plotXY(List( (pvMonoRes._1, pvMonoRes._2, "Residential"), (pvMonoCom._1, pvMonoCom._2, "Commercial")), legend = true, xLabel = "Electricity Produced [EJ/year]", yLabel = "EROI std", title = "rooftop_potential_mono")
    plotXY(List( (pvPolyRes._1, pvPolyRes._2, "Residential"), (pvPolyCom._1, pvPolyCom._2, "Commercial")), legend = true, xLabel = "Electricity Produced [EJ/year]", yLabel = "EROI std", title = "rooftop_potential_poly")

    List(1, 2, 4, 6, 8, 10, 12).map(e =>
      println(e + "\t" + res.filter(i => PVMono.eroi(i._2) >= e).map(i => potential(i._4 * i._6 + i._3 * i._5, i._2, PVMono).to(Exajoules)).sum
        + "\t" + res.filter(i => PVPoly.eroi(i._2) >= e).map(i => potential(i._4 * i._6 + i._3 * i._5, i._2, PVPoly).to(Exajoules)).sum))
  }
  
  def printEDelivered(res : List[(String, Irradiance, Area, Area, Double, Double)], distr_losses : Double)  {
     List(1, 2, 4, 6, 8, 10, 12).map(e =>
      println(e + "\t" + res.filter(i => PVMono.eroi_pou(i._2,distr_losses) >= e).map(i => potential(i._4 * i._6 + i._3 * i._5, i._2, PVMono).to(Exajoules)*(1-distr_losses)).sum
        + "\t" + res.filter(i => PVPoly.eroi_pou(i._2,distr_losses) >= e).map(i => potential(i._4 * i._6 + i._3 * i._5, i._2, PVPoly).to(Exajoules)*(1-distr_losses)).sum))

  }
    def printEProduced(res : List[(String, Irradiance, Area, Area, Double, Double)])  {
     List(1, 2, 4, 6, 8, 10, 12).map(e =>
      println(e + "\t" + res.filter(i => PVMono.eroi(i._2) >= e).map(i => potential(i._4 * i._6 + i._3 * i._5, i._2, PVMono).to(Exajoules)).sum
        + "\t" + res.filter(i => PVPoly.eroi(i._2) >= e).map(i => potential(i._4 * i._6 + i._3 * i._5, i._2, PVPoly).to(Exajoules)).sum))

  }
  def cf_potential(countries: Option[List[String]], tech: List[PV], power_units: PowerUnit = Gigawatts) = {
    val cells = if (countries.isDefined) countries.get.map(c => resources.filter(_._1.equals(c))).flatten
    else resources
    cells.map(i =>
      (i._1, (i._4 * i._6 + i._3 * i._5).toSquareKilometers, i._2.toWattsPerSquareMeter, tech.map(t => (t.capacityFactor(i._2), power(i._4 * i._6 + i._3 * i._5, i._2, t).to(power_units)))))
  }
  def printCF_Potential(countries: Option[List[String]], tech: List[PV], name: String) {
    val res = cf_potential(countries, tech).sortBy(_._4(0)._1).reverse
    val out_stream = new PrintStream(new java.io.FileOutputStream(name + "_rooftopPV"))
    res.map(i => {
      out_stream.print(i._2 + "\t" + i._3 + "\t")
      i._4.map(j => out_stream.print(j._1 + "\t"+j._2 + "\t"))
      out_stream.println(i._1)
      
    })
    out_stream.close()
  }
  def potential(area: Area, irradiance: Irradiance, tech: SolarTechnology): Energy = {
    power(area, irradiance, tech) * Hours(365 * 24)
  }
  def power(area: Area, irradiance: Irradiance, tech: SolarTechnology): Power = {
    tech.lifeTimeEfficiency(irradiance) * area * irradiance * (1 - tech.operation_variable)
  }
  def capacityFactor(area: Area, irradiance: Irradiance, tech: SolarTechnology): Double = {
    potential(area, irradiance, tech) / (tech.ratedPower(area, irradiance) * Hours(365 * 24))
  }

  def netYearlyProductions(area: Area, irradiance: Irradiance, tech: SolarTechnology): Energy = {

    val power = tech.ratedPower(area, irradiance)
    val gross = potential(area, irradiance, tech)
    gross - tech.embodiedEnergyArea(power, area) / tech.lifeTime
  }

}
  