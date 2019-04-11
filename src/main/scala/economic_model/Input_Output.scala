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

object Input_Output {
   
  val all_sites = Grid().cells // Grid.eu().cells.filter(i => OnshoreWindTechnology.suitabilityFactor(i) > 0 || OffshoreWindTechnology.suitabilityFactor(i) > 0)
  val sites = all_sites

  def main(args: Array[String]): Unit = {
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
        tech.fixed_energy_inputs_1GW(c).toMegawattHours/1000.0 + "\t" +
        tech.operation_variable.toGigajoules + "\t" +
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
        c.ghi.toWattsPerSquareMeter + "\t" + c.dni.toWattsPerSquareMeter+ "\t"+
        c.area.toSquareKilometers + "\t" +
        PVMono.suitabilityFactor(c)+ "\t" + CSPTowerStorage12h.suitabilityFactor(c) + "\t" +
        PVMono.fixed_energy_inputs_1GW(c).toMegawattHours/1000.0 + "\t" +
        PVMono.operation_variable.toGigajoules + "\t" +
        CSPTowerStorage12h.fixed_energy_inputs_1GW(c).toMegawattHours/1000.0 + "\t" +
        (CSPTowerStorage12h.ee.transport_variable + CSPTowerStorage12h.ee.construction_variable).toMegawattHours/1000.0 + "\t" + CSPTowerStorage12h.ee.default_area.toSquareKilometers/1000.0 + "\t"+
        CSPTowerStorage12h.operation_variable.toGigajoules +
        "\n")
    })
    out_stream.close()
  }

  def netEnergy(cell: Cell, tech: WindTechnology, installedPower: Power): Energy = {
    val area = cell.area * tech.suitabilityFactor(cell); val rD = tech.rotorDiameter(Megawatts(1), cell.elevation + Meters(100))
    // Spacing factor calculation for a given suitable area and installed capacity density
    val nD = Math.sqrt(area.toSquareMeters / installedPower.toMegawatts) / rD.toMeters
    val potential = tech.power(cell, MetersPerSecond(11), nD) * Hours(365 * 24)
    // println(area.toSquareKilometers + "\t" + installedPower.toMegawatts + "\t" + nD + "\t" + potential)
    potential - tech.embodiedEnergy(cell, installedPower, potential)
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

}

class Site(val center: GeoPoint, val area: Area, val sf: (Double, Double, Double, Double), val c: Velocity, val k: Double, val ghi: Irradiance, val dni: Irradiance) {
  val capacityFactor = CapacityFactorCalculation.cubic(c.toMetersPerSecond, k, 3, 11)
  def lambda(nD: Double): Double = math.Pi / (4 * nD * nD)
  def arrayEfficiency(n: Double, nD: Double) = WakeEffect.arrayEfficiency(n, lambda(nD))
  // def production(n : Double, nD : Double) = capacityFactor*arrayEfficiency(n, nD)*
}