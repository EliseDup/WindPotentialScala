package GEMBA

import squants.energy._
import utils._
object Gemba {
  import PlotHelper._

  val sources = Helper.getLines("GEMBA_sources", "\t").map(EnergySource(_)) //filter(s => !s.name.contains("UNCONVENTIONAL") && !s.name.contains("BIOMASS")); 
  val n = 2 //sources.size
  
  val years = (1799 to 2200).toList; val nYears = years.size
  
  def source(name: String) = sources.find(_.name.equalsIgnoreCase(name)).get
  // Values for the whole economy = amount of energy required per unit of industrial output
  val energy_requirement = 1.67;
  // Values for the whole economy = industrial output produced by unit of capital stock
  val capital_effectiveness = 0.093;
  // Energy gross and net production, and industrial output
  val production = Array.fill[Energy](n, nYears)(Joules(0))
  val cumulated_production = Array.fill[Energy](n, nYears)(Joules(0))
  val production_total = Array.fill[Energy](nYears)(Joules(0))
  val net_production = Array.fill[Energy](n, nYears)(Joules(0))
  val industrial_output = Array.fill[Energy](nYears)(Joules(0))
  // Ratio of the annual / cumulated production to the technical potential / URR
  val rho = Array.fill[Double](n, nYears)(0)
  val eroi = Array.fill[Double](n, nYears)(0) // Depends on rho

  val demand = Array.fill[Energy](n, nYears)(Joules(0))
  val demand_total = Array.fill[Energy](nYears)(Joules(0))
  // Capital stock for the industrial sector, used to estimate the demand = energy demand / unit of industrial output (espilon) * coefficient of capital stock to industrial output (kappa) * industrial capital stock
  val industrial_capital_stock = Array.fill[Energy](nYears)(Joules(0))
  val lifetime_industrial_capital_stock = 20
  // Capital stock
  val capital_stock = Array.fill[Energy](n, nYears)(Joules(0))
  
  // The amount of energy / capital stock needed to produce the energy production
  val energy_inputs_energy_sector = Array.fill[Energy](n, nYears)(Joules(0))
  val capital_inputs_energy_sector = Array.fill[Energy](n, nYears)(Joules(0))
  
  def min(e1: Energy, e2: Energy) = List(e1, e2).minBy(_.toJoules)
  def max(e1: Energy, e2: Energy) = List(e1, e2).maxBy(_.toJoules)
  def sum_over_sources(array : Array[Array[Energy]], year : Int) = (0 until n).map(s => array(s)(year)).foldLeft(Exajoules(0))(_ + _)
  def sum(array : Array[Energy]) = array.toList.foldLeft(Joules(0))(_ + _)
  
  def main(args: Array[String]): Unit = {
    sources.find(_.name.contains("BIOMASS")).get.plot
    simulate
    val list = (0 until n).toList.map(i => (years.map(_.toDouble), production(i).map(j => j.to(Exajoules)).toList, sources(i).name))
    plotXY(list, legend = true)
    // val list_eroi = (0 until n).toList.map(i => (years.map(_.toDouble), eroi(i).toList, sources(i).name))
    // plotXY(list_eroi, legend = true)
    plotXY(List((years.map(_.toDouble), industrial_output.toList.map(_.to(Exajoules)), "Industrial Ouput"),
      (years.map(_.toDouble), production_total.toList.map(_.to(Exajoules)), "Production")), legend = true)
  }

  def simulate {
    for (year <- 1 until nYears) {
      val y = years(year)
      for (s <- 0 until n) {
        val source = sources(s)
        // Initialize source entering the market with a capital stock of 1 EJ
        if (y == source.incept_date) {
          capital_stock(s)(year - 1) = Exajoules(1)
        }
        // Simulate production, depending on the available capital stock in i-1 = capital stock / capital factor * EROI / lifetime = Energy Outputs / year
        // We cannot produce more than the technical potential / URR
        val prod = capital_stock(s)(year - 1) / source.capital_factor * eroi(s)(year - 1) / source.life_time
        if (source.renewable) {
          production(s)(year) = min(source.technical_potential, prod)
          if (production(s)(year) >= source.technical_potential) {
            println(source.name + " reaches technical potential in " + y)
          }
          cumulated_production(s)(year) = production(s)(year) + cumulated_production(s)(year - 1)
          rho(s)(year) = production(s)(year) / source.technical_potential
        } else {
          cumulated_production(s)(year) = min(source.technical_potential, cumulated_production(s)(year - 1) + prod)
          if (cumulated_production(s)(year) >= source.technical_potential) {
            println(source.name + " reaches URR in " + y)
          }
          production(s)(year) = cumulated_production(s)(year) - cumulated_production(s)(year - 1)
          rho(s)(year) = cumulated_production(s)(year) / source.technical_potential
        }
        // In order to prevent errors when the EROI of a resource = 0
        eroi(s)(year) = math.max(source.EROI(rho(s)(year)), 0.0001)
        energy_inputs_energy_sector(s)(year) = (1 - source.capital_factor) * production(s)(year) / eroi(s)(year)
        net_production(s)(year) = production(s)(year) - energy_inputs_energy_sector(s)(year)
      }
      production_total(year) = sum_over_sources(production, year) //(0 until n).map(s => production(s)(year)).foldLeft(Joules(0))(_ + _)

      // Simulate demand, based on industrial capital stock
      demand_total(year) = energy_requirement * capital_effectiveness * industrial_capital_stock(year - 1)
      // Allocate demand based on EROI and rho
      var den = (0 until n).map(s => rho(s)(year) * eroi(s)(year)).sum
      if (den == 0) den = 1
      for (s <- 0 until n) {
        val source = sources(s)
        demand(s)(year) = demand_total(year) * rho(s)(year) * eroi(s)(year) / den
        // Capital stock required to produce the demand
        val required_capital_stock = demand(s)(year) * source.capital_factor * source.life_time / eroi(s)(year)
        // Difference between what exist and what is needed for year y
        capital_inputs_energy_sector(s)(year) = max(Joules(0), required_capital_stock - capital_stock(s)(year - 1))
        capital_stock(s)(year) = capital_stock(s)(year - 1) + capital_inputs_energy_sector(s)(year) - capital_stock(s)(year - 1) / source.life_time
      }

      val net_energy = sum_over_sources(net_production, year)
      // The amount of capital stock that must be transferred from the economy to the energy sector (the surplus required to produce the demand, = 0 if there was enough capital stock installed)
      val capital_feedback = sum_over_sources(capital_inputs_energy_sector, year)
      // The net production in term of industrial output
      industrial_output(year) = net_energy / energy_requirement - capital_feedback // / capital_effectiveness ? Units ..
      // The capital stock of the industry = cs of t-1 + output - depreciation
      industrial_capital_stock(year) = industrial_capital_stock(year - 1) + industrial_output(year)*capital_effectiveness - industrial_capital_stock(year - 1) / lifetime_industrial_capital_stock
    }
  }
}

class EnergySource(val name: String, val technical_potential: Energy, val max_eroi: Double, val degradation_rate: Double,
    val immature_technology: Double, val learning_rate: Double, val incept_date: Double,
    val life_time: Double, val capital_factor: Double) {
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
    csv(2).toDouble, csv(6).toDouble, csv(4).toDouble, csv(5).toDouble, csv(3).toDouble, csv(7).toDouble, csv(8).toDouble)
}