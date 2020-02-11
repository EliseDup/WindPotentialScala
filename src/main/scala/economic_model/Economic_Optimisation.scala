package economic_model

import squants.space._
import squants.motion._
import squants.radio._
import squants.time._
import utils.Helper
import wind_energy._
import wind_energy.WakeEffect
import breeze.stats.distributions.Geometric
import utils.GeoPoint
import wind_solar.Grid
import breeze.optimize._
import breeze.linalg._
import wind_solar.Cell
import squants.energy._
import utils.Exajoules
import solar_energy._
import utils.PlotHelper
import wind_solar.RenewableTechnology

object Economic_Optimisation {

  import Helper._
  import PlotHelper._

  val all_sites = Grid().cells // Grid.eu().cells.filter(i => OnshoreWindTechnology.suitabilityFactor(i) > 0 || OffshoreWindTechnology.suitabilityFactor(i) > 0)
  val sites = all_sites
  def KEden = all_sites.map(s => math.pow(s.wind100m.mean.toMetersPerSecond, 2) * s.area.toSquareKilometers).sum
  def main(args: Array[String]): Unit = {
    val techs = List(OnshoreWindTechnology, OffshoreWindTechnology, PVMono, CSPTowerStorage12h)
    techs.map(t => println(t.name + "\t" + t.operation_variable))
    techs.map(t => println(t.name + "\t" + t.lifeTime))
    
    /**
     * writePythonInputsSimpleModel("../WindPotentialPython/Economic_Optimisation/inputs/inputs_simple_total", KEden, true)
     * writePythonInputsSimpleModelParams("../WindPotentialPython/Economic_Optimisation/inputs/inputs_params_total", KEden, true)
     * writePythonInputsSimpleModel("../WindPotentialPython/Economic_Optimisation/inputs/inputs_simple_sf", KEden, false)
     * writePythonInputsSimpleModelParams("../WindPotentialPython/Economic_Optimisation/inputs/inputs_params_sf", KEden, false)
     *
     */
    writePythonInputsSimpleModelParamsWind("../WindPotentialPython/Economic_Optimisation/inputs/inputs_params_wind", KEden, false)
    writePythonInputsSimpleModelParamsPV("../WindPotentialPython/Economic_Optimisation/inputs/inputs_params_pv", false)
    writePythonInputsSimpleModelParamsCSP("../WindPotentialPython/Economic_Optimisation/inputs/inputs_params_csp", false)

    //    val techs = List(OnshoreWindTechnology,OffshoreWindTechnology,PVMono,CSPTowerStorage12h)
    //    techs.map(tech => writePythonInputsSimpleModelOneTechnology("../WindPotentialPython/Economic_Optimisation/inputs/inputs_simple_sf"+tech.name, KEden, false, tech))
  }

  /**
   * Inputs for Python optimization that do not vary from cell to cell
   */
  def printFixedInputs(pv: PV = PVMono, csp: CSP = CSPTowerStorage12h) {
    println("efficiency_PV = " + pv.lifeTimeEfficiency(WattsPerSquareMeter(0)))
    println("operationE_PV = " + pv.operation_variable)
    println("operationE_CSP = " + csp.operation_variable)
   // println("embodiedE1y_PV = " + pv.fixed_energy_inputs_1GW(all_sites(0)).toMegawattHours / 1000.0 / pv.lifeTime)
   // println("embodiedE1y_CSP_fixed = " + csp.ee.embodiedEnergyArea(Gigawatts(1), SquareKilometers(0)).toMegawattHours / 1000.0 / csp.lifeTime)
   // println("embodiedE1y_CSP_area = " + csp.ee.embodiedEnergyArea(Gigawatts(0), csp.ee.default_area).toMegawattHours / 1000.0 / csp.lifeTime)
   // println("defaultCSP_area = " + csp.ee.default_area.toSquareKilometers)
  }
  /**
   *  Write the inputs needed for the optimization for the simple model with default design parameters (n and vr for wind farm, SM for CSP tower 12h)
   *  The inputs for each cell are written in a single line in a txt file:
   *  Col 0: Lat
   *  Col 1: Lon
   *  Col 2 to 4: suitable area (wind, pv, csp) * ground cover ratio (-> only panel/mirrors effective area for solar power plants !)
   *  Col 5 to 7: conversion efficiency (= life time efficiency for pv, csp and Capacity factor * array efficiency * availability factor for wind)
   *  Col 8 to 10: renewable resources [W/m^2] such as to have energy produced = area * efficiency * resources. For wind because efficiency = capacity factor, renewable resources = installed capacity density !), for pv = GHI, for CSP = DNI
   *  Col 11 to 13: Installed capacity density (= installed capacity : wind = 1/2 Cp,max rho pi/4 vr^3 / n^2, pv = 240 Wp/m^2, csp = 950 W/m^2 * 22% / SM)
   *  Col 14 to 16: Fixed energy inputs [kWh/MW/year]
   *  Col 17 to 19: Operational energy inputs [kWh/kWh_produced]
   */
  def writePythonInputsSimpleModel(logFile: String, KEden: Double, total: Boolean, n: Double = 8, vr: Double = 11, SM: Double = 2.7) {
    val out_stream = new java.io.PrintStream(new java.io.FileOutputStream(logFile))
    sites.map(c => {
      val windTech = if (c.onshore) OnshoreWindTechnology else OffshoreWindTechnology
      val tech = List(windTech, PVMono, CSPTowerStorage12h)
      if (total || tech.map(_.suitabilityFactor(c)).sum > 0) {
        out_stream.print(c.center.latitude.toDegrees + "\t" + c.center.longitude.toDegrees + "\t" + c.area.toSquareKilometers + "\t")
        tech.map(t => out_stream.print(t.suitabilityFactor(c) * c.area.toSquareKilometers / t.occupationRatio + "\t"))
        // Efficiency 
        out_stream.print(CapacityFactorCalculation.cubic(c.wind100m, 11) * windTech.availabilityFactor(c) * WakeEffect.arrayEfficiency(500, math.Pi / (4 * n * n)) + "\t")
        out_stream.print(PVMono.lifeTimeEfficiency(c.ghi) + "\t")
        out_stream.print((if (c.dni.value > 0) CSPTowerStorage12h.lifeTimeEfficiency(c.dni, 2.7) else 0.0) + "\t")
        // Ressources (the energy produced must be equal to area * efficiency * ressources, so there is a trick for wind)
        val cd_wind = 0.5 * 0.5 * 1.225 * Math.PI / 4 * Math.pow(vr, 3) / Math.pow(n, 2)
        out_stream.print(cd_wind + "\t" + c.ghi.toWattsPerSquareMeter + "\t" + c.dni.toWattsPerSquareMeter + "\t")
        // Installed capacity density
        out_stream.print(cd_wind + "\t")
        out_stream.print(240.0 + "\t")
        out_stream.print(950 * 0.22 / SM + "\t")
        tech.map(t => out_stream.print((t.embodiedEnergy(Gigawatts(1)).toMegawattHours / 1000.0 / t.lifeTime) + "\t"))
        tech.map(t => out_stream.print(t.operation_variable + "\t"))
        // out_stream.print(windTech.availabilityFactor(c) + "\t")
        out_stream.print(c.keDissipation.toWattsPerSquareMeter + "\t") // math.pow(c.wind100m.mean.toMetersPerSecond, 2) / KEden * 292 * 1E6 + "\t")
        out_stream.print(c.wind100m.mean.toMetersPerSecond + "\t" + c.area.toSquareKilometers)

        out_stream.print("\n")
      }
    })
    out_stream.close()
  }
  def writePythonInputsSimpleModelOneTechnology(logFile: String, KEden: Double, total: Boolean, tech: RenewableTechnology, n: Double = 8, vr: Double = 11, SM: Double = 2.7) {
    val out_stream = new java.io.PrintStream(new java.io.FileOutputStream(logFile))
    sites.map(c => {
      if (total || tech.suitabilityFactor(c) > 0) {
        out_stream.print(c.center.latitude.toDegrees + "\t" + c.center.longitude.toDegrees + "\t" + c.area.toSquareKilometers + "\t")
        out_stream.print(tech.suitabilityFactor(c) * c.area.toSquareKilometers / tech.occupationRatio + "\t")

        if (tech.isInstanceOf[WindTechnology]) {
          // Efficiency 
          out_stream.print(CapacityFactorCalculation.cubic(c.wind100m, 11) * tech.asInstanceOf[WindTechnology].availabilityFactor(c) * WakeEffect.arrayEfficiency(500, math.Pi / (4 * n * n)) + "\t")
          val cd_wind = 0.5 * 0.5 * 1.225 * Math.PI / 4 * Math.pow(vr, 3) / Math.pow(n, 2)
          // Ressources (the energy produced must be equal to area * efficiency * ressources, so there is a trick for wind)
          out_stream.print(cd_wind + "\t")
          // Installed capacity density
          out_stream.print(cd_wind + "\t")
        } else if (tech.isInstanceOf[PV]) {
          out_stream.print(tech.asInstanceOf[PV].lifeTimeEfficiency(c.ghi) + "\t")
          out_stream.print(c.ghi.toWattsPerSquareMeter + "\t")
          out_stream.print(240.0 + "\t")
        } else {
          out_stream.print((if (c.dni.value > 0) tech.asInstanceOf[CSP].lifeTimeEfficiency(c.dni, 2.7) else 0.0) + "\t")
          out_stream.print(c.dni.toWattsPerSquareMeter + "\t")
          out_stream.print(950 * 0.22 / SM + "\t")
        }
        out_stream.print((tech.embodiedEnergy(Gigawatts(1)).toMegawattHours / 1000.0 / tech.lifeTime) + "\t")
        out_stream.print(tech.operation_variable + "\t")
        out_stream.print(c.keDissipation.toWattsPerSquareMeter + "\t") // math.pow(c.wind100m.mean.toMetersPerSecond, 2) / KEden * 292 * 1E6 + "\t")
        out_stream.print(c.wind100m.mean.toMetersPerSecond + "\t" + c.area.toSquareKilometers)

        out_stream.print("\n")
      }
    })
    out_stream.close()
  }
  /**
   * area = data[:, 2:5]
   * c = data[:, 6]; k = data[:, 7]; ghi = data[:, 8]; dni = data[:, 9]
   * embodiedE1y_wind = data[:, 10]
   * availWind = data[:,11]
   */
  def writePythonInputsSimpleModelParams(logFile: String, keDen: Double, total: Boolean) {
    val out_stream = new java.io.PrintStream(new java.io.FileOutputStream(logFile))
    sites.map(c => {
      val windTech = if (c.onshore) OnshoreWindTechnology else OffshoreWindTechnology
      val tech = List(windTech, PVMono, CSPTowerStorage12h)
      if (total || tech.map(_.suitabilityFactor(c)).sum > 0) {
        out_stream.print(c.center.latitude.toDegrees + "\t" + c.center.longitude.toDegrees + "\t" + c.area.toSquareKilometers + "\t")
        tech.map(t => out_stream.print(t.suitabilityFactor(c) * c.area.toSquareKilometers / t.occupationRatio + "\t"))
        out_stream.print(c.wind100m.c.toMetersPerSecond + "\t" + c.wind100m.k + "\t" + c.ghi.toWattsPerSquareMeter + "\t" + c.dni.toWattsPerSquareMeter + "\t")
        out_stream.print(windTech.embodiedEnergy(Gigawatts(1)).toMegawattHours / 1000.0 / windTech.lifeTime + "\t" + windTech.operation_variable + "\t" + windTech.availabilityFactor(c))
        out_stream.print("\t" + c.keDissipation.toWattsPerSquareMeter) // math.pow(c.wind100m.mean.toMetersPerSecond, 2) / keDen * 292 * 1E6)

        out_stream.print("\n")
      }
    })
    out_stream.close()
  }
  def writePythonInputsSimpleModelParamsWind(logFile: String, keDen: Double, total: Boolean) {
    val out_stream = new java.io.PrintStream(new java.io.FileOutputStream(logFile))
    sites.map(c => {
      val tech = if (c.onshore) OnshoreWindTechnology else OffshoreWindTechnology
      if (total || tech.suitabilityFactor(c) > 0) {
        out_stream.print(c.center.latitude.toDegrees + "\t" + c.center.longitude.toDegrees + "\t" + c.area.toSquareKilometers + "\t" +
          tech.suitabilityFactor(c) * c.area.toSquareKilometers / tech.occupationRatio + "\t" + c.wind100m.c.toMetersPerSecond + "\t" + c.wind100m.k + "\t" +
          tech.embodiedEnergy(Gigawatts(1)).toMegawattHours / 1000.0 / tech.lifeTime + "\t" + tech.operation_variable + "\t" + tech.availabilityFactor(c)
          + "\t" + c.keDissipation.toWattsPerSquareMeter) // math.pow(c.wind100m.mean.toMetersPerSecond, 2) / keDen * 292 * 1E6)

        out_stream.print("\n")
      }
    })
    out_stream.close()
  }
  def writePythonInputsSimpleModelParamsCSP(logFile: String, total: Boolean) {
    val out_stream = new java.io.PrintStream(new java.io.FileOutputStream(logFile))
    sites.map(c => {
      val tech = CSPTowerStorage12h
      if (total || tech.suitabilityFactor(c) > 0) {
        out_stream.print(c.center.latitude.toDegrees + "\t" + c.center.longitude.toDegrees + "\t" + c.area.toSquareKilometers + "\t" +
          tech.suitabilityFactor(c) * c.area.toSquareKilometers / tech.occupationRatio + "\t" + c.dni.toWattsPerSquareMeter)
        out_stream.print("\n")
      }
    })
    out_stream.close()
  }
  def writePythonInputsSimpleModelParamsPV(logFile: String, total: Boolean) {
    val out_stream = new java.io.PrintStream(new java.io.FileOutputStream(logFile))
    sites.map(c => {
      val tech = PVMono
      if (total || tech.suitabilityFactor(c) > 0) {
        out_stream.print(c.center.latitude.toDegrees + "\t" + c.center.longitude.toDegrees + "\t" + c.area.toSquareKilometers + "\t" +
          tech.suitabilityFactor(c) * c.area.toSquareKilometers / tech.occupationRatio + "\t" + c.ghi.toWattsPerSquareMeter)
        out_stream.print("\n")
      }
    })
    out_stream.close()
  }
  def maximiseNetEnergy {
    println("I-O model with " + "\t" + sites.size + " sites")
    writePythonInputsWind("opti_inputs_wind")
    writePythonInputsSolar("opti_inputs_solar")

    // The optimal # of wind turbines in each cell
    val x_0 = DenseVector.ones[Double](sites.size)

    def f(x: DenseVector[Double]) = -(0 until x.size).map(i => netEnergy(sites(i), if (sites(i).offshore) OffshoreWindTechnology else OnshoreWindTechnology, Megawatts(x(i)))).foldLeft(Joules(0))(_ + _).to(Exajoules)
    val diffF = new ApproximateGradientFunction(f)

    val net_energy = new DiffFunction[DenseVector[Double]] {
      def calculate(x: DenseVector[Double]) = (f(x), diffF.gradientAt(x))
    }

    val lbfgs = new LBFGS[DenseVector[Double]](maxIter = 10, m = 3)
    val res = lbfgs.minimize(net_energy, x_0)

    println(net_energy(res))
    println("Optimal # MW installed " + res)
  }

  def writePythonInputsWind(logFile: String) {
    val out_stream = new java.io.PrintStream(new java.io.FileOutputStream(logFile))
    val suitable = sites.filter(c => (OnshoreWindTechnology.suitabilityFactor(c) > 0 ||
      OffshoreWindTechnology.suitabilityFactor(c) > 0))
    println("Suitable " + "\t" + suitable.size)
    sites.map(c => {
      val tech = if (c.onshore) OnshoreWindTechnology else OffshoreWindTechnology
      out_stream.print(c.center.latitude.toDegrees + "\t" + c.center.longitude.toDegrees + "\t" +
        c.wind100m.c.toMetersPerSecond + "\t" + c.wind100m.k + "\t" +
        c.area.toSquareKilometers + "\t" +
        tech.suitabilityFactor(c) * c.area.toSquareKilometers + "\t" +
        tech.embodiedEnergy(Gigawatts(1)).toMegawattHours / 1000.0 + "\t" +
        tech.operation_variable + "\t" +
        tech.availabilityFactor(c) + "\n")
    })
    out_stream.close()
  }

  def writePythonInputsSolar(logFile: String) {
    val out_stream = new java.io.PrintStream(new java.io.FileOutputStream(logFile))
    val suitable = sites.filter(c => (PVMono.suitabilityFactor(c) > 0 ||
      CSPTowerStorage12h.suitabilityFactor(c) > 0))
    println("Suitable " + "\t" + suitable.size)
    sites.map(c => {
      out_stream.print(c.center.latitude.toDegrees + "\t" + c.center.longitude.toDegrees + "\t" +
        c.ghi.toWattsPerSquareMeter + "\t" + c.dni.toWattsPerSquareMeter + "\t" +
        c.area.toSquareKilometers + "\t" +
        PVMono.suitabilityFactor(c) + "\t" + CSPTowerStorage12h.suitabilityFactor(c) + "\t" +
        PVMono.embodiedEnergy(Gigawatts(1)).toMegawattHours / 1000.0 + "\t" +
        PVMono.operation_variable + "\t" +
        CSPTowerStorage12h.embodiedEnergy(Gigawatts(1)).toMegawattHours / 1000.0 + "\t" +
        (CSPTowerStorage12h.transport_variable + CSPTowerStorage12h.construction_variable).toMegawattHours / 1000.0 + "\t" + CSPTowerStorage12h.default_aperture_area.toSquareKilometers / 1000.0 + "\t" +
        CSPTowerStorage12h.operation_variable +
        "\n")
    })
    out_stream.close()
  }

 def netEnergy(cell: Cell, tech: WindTechnology, installedPower: Power): Energy = {
    val area = cell.area * tech.suitabilityFactor(cell); val rD = tech.rotorDiameter(Megawatts(1), cell.elevation + Meters(100))
    // Spacing factor calculation for a given suitable area and installed capacity density
    val nD = Math.sqrt(area.toSquareMeters / installedPower.toMegawatts) / rD.toMeters
    val potential = tech.potential(cell, 1) * Hours(365 * 24)
    // println(area.toSquareKilometers + "\t" + installedPower.toMegawatts + "\t" + nD + "\t" + potential)
    potential - tech.embodiedEnergy(installedPower)
  }
  // Energy Sector caracteristics
  val capital_factor = 0.1; val EROI = 10; val life_time = 30;
  // Goods and services sector caracteristics
  val energy_requirement = 1.67; val capital_effectiveness = 0.1;
  // Technical coefficients
  // aie = xie / E = Quantity of energy / capital stock per unit of energy produced (measured by EROI = outputs  / total energy inputs; and capital factor = capital inputs / total energy inputs)
  // aej = xej / Q = Quantity of energy / capital stock per unit of goods and services : energy requirement of the economy
  val A = Array.fill[Double](2, 2)(0)
  val x = Array.fill[Double](2, 2)(0)
  // Index 0 is the energy sector, index 1 is the economy sector
  x(0)(0) = (1 - capital_factor) * life_time / EROI
  x(0)(1) = energy_requirement
  x(1)(0) = capital_factor * life_time / EROI
  x(1)(1) = capital_effectiveness

  def energyProductionFunction(tech: RenewableTechnology, cells: List[Cell] = all_sites) {
    val suitable = cells.filter(c => tech.suitabilityFactor(c) > 0)
    println(cells.size + "\t" + suitable.size)
    val res = suitable.map(c => {
      val energy = (tech.potential(c, 1.0) * Hours(365 * 24)).to(Exajoules) * (1 - tech.operation_variable)
      val EE = (tech.embodiedEnergy(Gigawatts(1)) * tech.ratedPower(c, 1).toGigawatts).to(Exajoules) / tech.lifeTime
      (energy, EE, energy)
    })
    val cum = listCumulatedVSCumulatedBy(res)
    plotXY(List((cum._1, cum._2, "")), yLabel = tech.name + " Energy Produced [EJ/year]", xLabel = "Embodied Energy in Capital [EJ]")

  }
}

class Site(val center: GeoPoint, val area: Area, val sf: (Double, Double, Double, Double), val c: Velocity, val k: Double, val ghi: Irradiance, val dni: Irradiance) {
  val capacityFactor = CapacityFactorCalculation.cubic(c.toMetersPerSecond, k, 3, 11)
  def lambda(nD: Double): Double = math.Pi / (4 * nD * nD)
  def arrayEfficiency(n: Double, nD: Double) = WakeEffect.arrayEfficiency(n, lambda(nD))
  // def production(n : Double, nD : Double) = capacityFactor*arrayEfficiency(n, nD)*
}