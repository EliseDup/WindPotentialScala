package GEMBA

import squants.energy._
import utils._
object Gemba {
  import PlotHelper._

  def source(sources: List[EnergySource], name: String) = sources.find(_.name.equalsIgnoreCase(name)).get
  // Values for the whole economy = amount of energy required per unit of industrial output
  val energy_requirement = 1.67;
  // Values for the whole economy = industrial output produced by unit of capital stock
  val capital_effectiveness = 0.093;

  def min(e1: Energy, e2: Energy) = List(e1, e2).minBy(_.toJoules)
  def max(e1: Energy, e2: Energy) = List(e1, e2).maxBy(_.toJoules)
  def sum_over_sources(array: Array[Array[Energy]], year: Int) = (0 until array.size).map(s => array(s)(year)).foldLeft(Exajoules(0))(_ + _)
  def sum(array: Array[Energy]): Energy = sum(array.toList)
  def sum(list: List[Energy]): Energy = list.foldLeft(Joules(0))(_ + _)
  val demand_exo = Helper.getLines("Demand_Exo", "\t").map(i => (i(0).toInt, Exajoules(i(1).toDouble)))
  def get_demand_exo(year: Int): Option[(Int, Energy)] = {
    if (year <= 1964) {
      val start_year = math.round(year / 10.0) * 10
      val start = demand_exo.find(_._1 == start_year).get._2
      val end = demand_exo.find(_._1 == start_year + 10).get._2
      Some(year, start + (year - start_year) * (end - start) / 10.0)
    } else if (year <= 2017) demand_exo.find(_._1 == year)
    else None
  }

  def main(args: Array[String]): Unit = {
    val resources = Helper.getLines("GEMBA_sources", "\t").map(EnergySource(_)) //.filter(s => !s.name.contains("UNCONVENTIONAL") && !s.name.contains("BIOMASS")); // .filter(_.renewable) // .filter(s => !s.renewable) // filter(s => !s.name.contains("UNCONVENTIONAL") && !s.name.contains("BIOMASS")); 
    val years = (1799 to 2200).toList;
    val years_double = years.map(_.toDouble)
    val ignore = List("UNCONVENTIONAL", "BIOMASS", "OTEC", "WAVE", "TIDAL", "GEOTHERMAL")
    val take = List("COAL", "OIL, CONVENTIONAL", "GAS, CONVENTIONAL", "BIOMASS", "WIND", "SOLAR","WAVE","OTEC","TIDAL")
    val sources = resources //.filter(s => take.contains(s.name))
    // sources.map(_.plot)
    val res = simulate(sources, years)

    val prod = (0 until sources.size).toList.map(i => (years_double, res.production_source(i).map(_.to(Exajoules)), "Production " + sources(i).name))
    plotXY(prod, legend = true, xLabel = "Year", yLabel = "EJ/year")

    plotXY(List((years_double, res.industrial_output.toList.map(_.to(Exajoules)), "Industrial Ouput"),
      (years_double, res.production_total.toList.map(_.to(Exajoules)), "Production"),
      (years_double, res.demand_total.toList.map(_.to(Exajoules)), "Demand")), legend = true, xLabel = "Year", yLabel = "EJ/year")

    plotXY(List((years_double, res.renewable_production.map(_.to(Exajoules)), "Renewable Production"),
      (years_double, res.non_renewable_production.map(_.to(Exajoules)), "Non-Renewable Production"),
      (years_double, res.production_total.toList.map(_.to(Exajoules)), "Total Production")), legend = true, xLabel = "Year", yLabel = "EJ/year")

  }

  def simulate(sources: List[EnergySource], years: List[Int]): GembaResults = {
    val n = sources.size; val nYears = years.size;
    val res = new GembaResults(sources, nYears)

    for (year <- 1 until (nYears - 1)) {
      val y = years(year)
      // Only take the resources non exhausted / already in the market
      val sources_util = sources.zipWithIndex.filter(s => !s._1.exhausted && s._1.incept_date <= y)
      for (util <- sources_util) {
        val source = util._1
        val s = util._2
        // Initialize source entering the market with a capital stock of 1 EJ
        if (y == source.incept_date) {
          res.capital_stock(s)(year) = Exajoules(1)
          res.eroi(s)(year - 1) = math.max(source.EROI(res.rho(s)(year)), 0.0001)
          println(y + "\t" + source.name)
        }
        // Simulate production, depending on the available capital stock in i-1 = capital stock / capital factor * EROI / lifetime = Energy Outputs / year
        // We cannot produce more than the technical potential / URR
        val prod = res.capital_stock(s)(year) / source.capital_factor * res.eroi(s)(year - 1) / source.life_time
        if (source.renewable) {
          res.production(s)(year) = min(source.technical_potential, prod)
          res.cumulated_production(s)(year) = res.production(s)(year) + res.cumulated_production(s)(year - 1)
          res.rho(s)(year) = res.production(s)(year) / source.technical_potential
        } else {
          res.cumulated_production(s)(year) = min(source.technical_potential, res.cumulated_production(s)(year - 1) + prod)
          if (res.cumulated_production(s)(year) == source.technical_potential) source.exhausted = true
          res.production(s)(year) = if (source.exhausted) Joules(0) else res.cumulated_production(s)(year) - res.cumulated_production(s)(year - 1)
          res.rho(s)(year) = res.cumulated_production(s)(year) / source.technical_potential
        }
        // In order to prevent errors when the EROI of a resource = 0
        res.eroi(s)(year) = math.max(source.EROI(res.rho(s)(year)), 0.0001)
        res.energy_inputs_energy_sector(s)(year) = (1 - source.capital_factor) * res.production(s)(year) / res.eroi(s)(year)
        res.net_production(s)(year) = res.production(s)(year) - res.energy_inputs_energy_sector(s)(year)
      }
      res.production_total(year) = sum_over_sources(res.production, year)

      // Simulate demand, based on industrial capital stock
      res.demand_total(year + 1) = energy_requirement * capital_effectiveness * res.industrial_capital_stock(year)
      val demand_excedent = max(Joules(0), res.demand_total(year + 1) - res.production_total(year))
      // Allocate only demand excedent (i.e. what is not already produced by existing capital stock) !! Otherwise capital stock remains unused !!
      var den = (sources_util.map(_._2)).map(s => res.eroi(s)(year)).sum
      if (den == 0) den = 1
      for (util <- sources_util) {
        val source = util._1
        val s = util._2
        res.demand(s)(year + 1) = res.production(s)(year) + demand_excedent * res.eroi(s)(year) / den
        // Capital stock required to produce the demand
        val required_capital_stock = res.demand(s)(year + 1) * source.capital_factor * source.life_time / res.eroi(s)(year)
        // Difference between what exist and what is needed for year y
        res.capital_inputs_energy_sector(s)(year + 1) = max(Joules(0), required_capital_stock - res.capital_stock(s)(year))
        res.capital_stock(s)(year + 1) = res.capital_stock(s)(year) + res.capital_inputs_energy_sector(s)(year + 1) - res.capital_stock(s)(year) / source.life_time
      }
      val net_energy = sum_over_sources(res.net_production, year)
      // The amount of capital stock that must be transferred from the economy to the energy sector (the surplus required to produce the demand, = 0 if there was enough capital stock installed)
      val capital_feedback = sum_over_sources(res.capital_inputs_energy_sector, year)
      // The net production in term of industrial output, = 0 if not enough is produced to feed the capital stock of the energy sector !!
      res.industrial_output(year) = max(Joules(0), net_energy / energy_requirement - capital_feedback) // / capital_effectiveness ? Units ..
      // The capital stock of the industry = cs of t-1 + output - depreciation
      res.industrial_capital_stock(year + 1) = res.industrial_capital_stock(year) + res.industrial_output(year) - res.industrial_capital_stock(year) / res.lifetime_industrial_capital_stock

      println(y + "\t" + res.capital_stock(0)(year).to(Exajoules) + "\t" + res.production_total(year).to(Exajoules) + "\t" + res.demand_total(year).to(Exajoules) + "\t"
        + res.industrial_capital_stock(year).to(Exajoules))

    }
    res
  }
}
class GembaResults(sources: List[EnergySource], nYears: Int) {
  import Gemba._
  val nSources = sources.size
  // Energy gross and net production, and industrial output
  val production = Array.fill[Energy](nSources, nYears)(Joules(0))
  val cumulated_production = Array.fill[Energy](nSources, nYears)(Joules(0))
  val production_total = Array.fill[Energy](nYears)(Joules(0))
  val net_production = Array.fill[Energy](nSources, nYears)(Joules(0))
  val industrial_output = Array.fill[Energy](nYears)(Joules(0))
  // Ratio of the annual / cumulated production to the technical potential / URR
  val rho = Array.fill[Double](nSources, nYears)(0)
  val eroi = Array.fill[Double](nSources, nYears)(0) // Depends on rho

  val demand = Array.fill[Energy](nSources, nYears)(Joules(0))
  val demand_total = Array.fill[Energy](nYears)(Joules(0))
  // Capital stock for the industrial sector, used to estimate the demand = energy demand / unit of industrial output (espilon) * coefficient of capital stock to industrial output (kappa) * industrial capital stock
  val industrial_capital_stock = Array.fill[Energy](nYears)(Joules(0))
  val lifetime_industrial_capital_stock = 20
  // Capital stock
  val capital_stock = Array.fill[Energy](nSources, nYears)(Joules(0))

  // The amount of energy / capital stock needed to produce the energy production
  val energy_inputs_energy_sector = Array.fill[Energy](nSources, nYears)(Joules(0))
  val capital_inputs_energy_sector = Array.fill[Energy](nSources, nYears)(Joules(0))

  val renewable_indexes = sources.zipWithIndex.filter(_._1.renewable).map(_._2)
  val non_renewable_indexes = sources.zipWithIndex.filter(s => !s._1.renewable).map(_._2)

  def renewable_production = (for (y <- 0 until nYears) yield sum(renewable_indexes.map(s => production(s)(y)))).toList
  def non_renewable_production = (for (y <- 0 until nYears) yield sum(non_renewable_indexes.map(s => production(s)(y)))).toList
  def production_source(s: Int) = (for (y <- 0 until nYears) yield production(s)(y)).toList
  def renewable_cumulated_production = (for (y <- 0 until nYears) yield sum(renewable_indexes.map(s => cumulated_production(s)(y)))).toList
  def non_renewable_cumulated_production = (for (y <- 0 until nYears) yield sum(non_renewable_indexes.map(s => cumulated_production(s)(y)))).toList
  def cumulated_production_source(s: Int) = (for (y <- 0 until nYears) yield cumulated_production(s)(y)).toList

}

class EnergySource(val name: String, val technical_potential: Energy, val max_eroi: Double, val degradation_rate: Double,
    val immature_technology: Double, val learning_rate: Double, val incept_date: Double,
    val life_time: Double, val capital_factor: Double, val initial_production: Energy) {
  var exhausted: Boolean = false
  val renewable = capital_factor == 0.9
  def EROI(production: Energy) = {
    val rho_k = rho(production)
    max_eroi * physical_component(rho_k) * technological_component(rho_k)
  }
  def EROI(rho: Double) = {
    max_eroi * physical_component(rho) * technological_component(rho)
  }
  def rho(production: Energy): Double = production / technical_potential
  def physical_component(rho: Double) = math.exp(-degradation_rate * rho)
  def technological_component(rho: Double) = 1 - immature_technology * math.exp(-learning_rate * rho)

  def listEROI = {
    val rho = (0 to 100).map(_ * 0.01).toList
    val prod = rho.map(_ * technical_potential.to(Exajoules))
    (prod, rho.map(i => EROI(i)), name)
  }
  def plot {
    val rho = (0 to 100).map(_ * 0.01).toList
    val prod = rho.map(_ * technical_potential.to(Exajoules))
    PlotHelper.plotXY(List((prod, rho.map(i => EROI(i)), "EROI " + name),
      (prod, rho.map(i => max_eroi * physical_component(i)), "Physical"),
      (prod, rho.map(i => max_eroi * technological_component(i)), "Technological")),
      xLabel = "Production [EJ/year]", yLabel = "EROI", legend = true)
  }
}
object EnergySource {
  def apply(csv: Array[String]) = new EnergySource(csv(0).toString, Exajoules(csv(1).toDouble),
    csv(2).toDouble, csv(6).toDouble, csv(4).toDouble, csv(5).toDouble, csv(3).toDouble, csv(7).toDouble, csv(8).toDouble, if (csv.size >= 10) Exajoules(csv(9).toDouble) else Exajoules(0))
}