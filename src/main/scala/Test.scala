

import utils.Helper
import java.io.PrintStream
import utils.PlotHelper
import squants.space.Degrees
import grid.WorldGrid
import wind_energy.CapacityFactorCalculation
import wind_energy.WindPotential
import squants.space.SquareKilometers
import squants.radio.WattsPerSquareMeter
import utils.Exajoules
import squants.energy._
import wind_energy.WindPowerTransmission
import grid.GlobCoverClasses
import java.io.FileOutputStream
import squants.radio.Irradiance
import utils._
import utils.TerawattHours
import utils.Terawatts
import squants.time.Hours
import squants.space.Area
import grid.GridCell
import squants.motion.Velocity
import squants.motion.MetersPerSecond
import utils._
import squants.space.Meters
import org.apache.commons.math3.special.Gamma
import wind_energy.WakeEffect
import wind_energy.WindProfile
import wind_energy.WindFarmEnergyInputs
import utils.PetawattHours
import wind_energy.NormalizedPowerCurve
import squants.space.Kilometers
import wind_energy.Weibull
import squants.space.Radians
import solar_energy.CSPParabolic
import solar_energy.CSPParabolicStorage12h
import solar_energy.CSPTowerStorage12h

object Test {

  import Helper._
  import PlotHelper._
  def main(args: Array[String]): Unit = {

    var lot_number = "HELLO"
    val out_stream = new PrintStream(new java.io.FileOutputStream("pompon_res"))
    var nlot = 0; var total = 0.0; var nlines = 0
    for (i <- Helper.getLines("../lot_pompon", "\t")) {

      if (lot_number.equals("HELLO") || i(3).toString != lot_number) {
        // println(lot_number)
        nlot = nlot + 1
        lot_number = i(3).toString
        out_stream.print("\n")
        (0 to 6).map(j => out_stream.print(i(j).toString + "\t"))
      }
      nlines = nlines + 1
      out_stream.print(i(7).toString + "\t")
    }
    out_stream.close()
    println(nlot + "\t" + nlines + "\t" + total)
    /*    val lines = Helper.getLines("../lot_pompon", "\t")
    val res = (1 until lines.size).map(i => (0 until 8).map(j => lines(i)(j).toString).toList).toList
    val lot = Set[String]() ++ res.map(i => i(3))
   val test = res.filter(_.size < 8)
   println(test.size)
    lot.map(l => {
      out_stream.print("\n")
      val x = res.filter(_(3).equals(l))
      // println(l + "\t" + x.map(_(7).toDouble).sum)
      val x0 = if(x.filter(_(6).contains("180")).isEmpty) x.filter(_(6).contains("Not"))(0) 
            else x.filter(_(6).contains("180"))(0)
      
      (0 to 7).map(i => out_stream.print(x0(i) + "\t"))
      (1 to 7).map(i => {
        val l = x.filter(_(6).contains((180+i).toString))
        if(l.nonEmpty) out_stream.print(l(0)(7) +"\t")
      })
      
    })
    println(res.size)
    println(lot.size)*/

  }

  def mmc {
    val Q = 100; // Débit volumique / unité de largeur = m2/s
    val alpha = math.Pi / 2
    val h = math.pow(3 * Q / math.sin(alpha), 1 / 3.0) // m
    val pa = 1E5 // Pa = kg/(m*s^2)
    val g = 9.81 // m/s^2
    val rho = 1000.0 // kg/m^3
    val mu = 0.001 // Pa/s

    def v(x2: Double) = rho * g / (2 * mu) * math.sin(alpha) * (2 * x2 * h - x2 * x2)
    def p(x2: Double) = {
      println(x2 + "\t" + (rho * g * math.cos(alpha) * (h - x2) + pa))
      rho * g * math.cos(alpha) * (h - x2) + pa
    }

    val x = (0 until 100).map(i => h * i / 100.0).toList
    println("H " + h + "\t" + math.sin(alpha))
    plotXY(List((x.map(i => i / h), x.map(i => v(i)), "")), xLabel = "x2/h", yLabel = "v2")
    plotXY(List((x.map(i => i / h), x.map(i => p(i) / pa), "")), xLabel = "x2/h", yLabel = "p/pa")

  }
}