package data_processing

import grid.WorldGrid
import squants.space._
import utils.Helper
import squants.energy._
import squants.radio.WattsPerSquareMeter
import squants.radio.Irradiance
import squants.time.Hours
import utils._
import wind_energy._
import squants.motion.Velocity
import squants.motion.MetersPerSecond
import java.io.PrintStream
import java.io.OutputStream

object OptimalCapacityDensity {

  def main(args: Array[String]): Unit = {
println(TonOilEquivalent(9425E6).to(Exajoules))	
    val t = System.currentTimeMillis()
    val eroi_min = (0 to 40).map(_ * 0.5).toList
    val cells = loadCells(eroi_min, "../res_world_1000m")
    println("Loading Time - " + (System.currentTimeMillis() - t) / 1000 + "seconds")
    plotOptimalCD(eroi_min, cells)
    val out_stream = new PrintStream(new java.io.FileOutputStream("eroi_12_bis"))
    cells.map(g => {
      out_stream.print(g.latitude.toString + "\t" + g.longitude.toString +  "\t" + g.annualProduction(true, 12).to(TerawattHours).toString+ "\t" + g.annualProduction(false, 12).to(TerawattHours).toString + 
          "\t" + WakeEffect.arrayEfficiency(200, g.lambda(g.getOptimalCD(12))).toString + "\n")
    })
   // println(cells.map(_.totalArea).foldLeft(SquareKilometers(0))(_ + _))
    val onshore = cells.filter(_.onshore)
    val offshore = cells.filter(i => !i.onshore)
    printResults(cells, List(2.0, 7.0, 12.0), topDown = false)
    printResults(onshore, List(2.0, 7.0, 12.0), topDown = false)
    printResults(offshore, List(2.0, 7.0, 12.0), topDown = false)
    
    // plotOptimalCD(eroi_min, cells)
   // plotEROIVSProd(eroi_min, cells)
    val list = listValueVSCumulated(cells.map(i => (i.eroi(false, 0.0), i.annualProduction(false, 0.0).to(Exajoules))))
    val listS = listValueVSCumulated(cells.map(i => (i.eroi(true, 0.0), i.annualProduction(true, 0.0).to(Exajoules))))
    PlotHelper.plotXY(List( (list._1,list._2,"Total"), (listS._1,listS._2,"Suitable")))
  }

  def plotOptimalCD(eroi_min: List[Double], cells: List[Cell]) {
    val optimalCD = eroi_min.map(e => annualProduction(cells, e, true).to(Exajoules))
    val eroiTest = (0 to 200).map(_ * 0.1).toList
    val fixedCD = List(2, 10).map(i => WattsPerSquareMeter(i)).toList
    val f = fixedCD.map(cd => (eroiTest, eroiTest.map(e => energyProducedFixedCD(cells, cd, e, true).to(Exajoules)), cd.toString))
    PlotHelper.plotXY(List((eroi_min, optimalCD, "Optimal Capacity Density")) ++ f, xLabel = "Minimum EROI", yLabel = "Wind Potential [EJ/year]", legend = true)
  }

  def printResults(cells: List[Cell], eroi_min: List[Double], power: Boolean = false, topDown: Boolean = false) {
    println("topDown ? " + topDown)
    println("EROImin \t Total \t Suitable  Area")
    for (e <- eroi_min) {
      if (power)
        if (!topDown) println(e + "\t" + powerProduced(cells, e, false).to(Terawatts) + "\t" + powerProduced(cells, e, true).to(Terawatts))
        else println(e + "\t" + powerProducedTopDown(cells, e, false).to(Terawatts) + "\t" + powerProducedTopDown(cells, e, true).to(Terawatts))
      else if (!topDown) println(e + "\t" + annualProduction(cells, e, false).to(Exajoules) + "\t" + annualProduction(cells, e, true).to(Exajoules))
      else println(e + "\t" + annualProductionTopDown(cells, e, false).to(Exajoules) + "\t" + annualProductionTopDown(cells, e, true).to(Exajoules))
    }
  }

  def printOptimalCDOnGrid(cells: List[Cell], minEROI: Double, name: String) {
    val out_stream = new PrintStream(new java.io.FileOutputStream(name))

    for (lat2 <- 0 to 180 * 2) {
      for (lon2 <- 0 to 360 * 2) {
        val lat = (-lat2 + (180.0)) / 2
        val lon = (lon2 - (360.0)) / 2
        val c = cells.find(c => c.latitude == lat && c.longitude == lon).getOrElse(DefaultCell())
        val suitable = (c.suitableArea.toSquareKilometers > 0)
        val cd = c.getOptimalCD(minEROI).toWattsPerSquareMeter
        out_stream.print(lat.toString + "\t" + lon.toString + "\t" + cd.toString + "\t" + (if (suitable) cd else 0.0).toString + "\n")
      }
    }
    out_stream.close()
  }

  def loadCells(eroi_min: List[Double], file: String): List[Cell] = {
    val f = Helper.getLines(file, "\t")
    (for (l <- f) yield new Cell(l(0).toDouble, l(1).toDouble, l(2).toDouble, SquareKilometers(l(3).toDouble), SquareKilometers(l(4).toDouble), Kilometers(l(5).toDouble),
      Megawatts(l(6).toDouble), MegawattHours(l(7).toDouble),
      (for (e <- (0 until eroi_min.size)) yield (e * 0.5, (l(e * 2 + 10).toBoolean, WattsPerSquareMeter(l(e * 2 + 9).toDouble)))).toMap, WattsPerSquareMeter(l(8).toDouble), ""))
  }
  def plotEROIVSCumulatedPower(eroi_min: List[Double], cells: List[Cell]) {
    val res = eroi_min.map(e => {
      val l = listValueVSCumulated(cells.map(c => (c.eroi(true, e), c.power(true, e).to(Terawatts))))
      (l._1, l._2, e.toString)
    })
    PlotHelper.plotXY(res, legend = true, xLabel = "Wind Power Potential [TW]", yLabel = "EROI")
  }
  
  def plotEROIVSPower(eroi_min: List[Double], cells: List[Cell], topDown: Boolean = false) {
    var list = List((eroi_min, eroi_min.map(e => powerProduced(cells, e, false)).map(_.to(Terawatts)), "Total"),
      (eroi_min, eroi_min.map(e => powerProduced(cells, e, true)).map(_.to(Terawatts)), "Suitable"))
    if (topDown) {
      list = list ++ List(
        (eroi_min, eroi_min.map(e => powerProducedTopDown(cells, e, false)).map(_.to(Terawatts)), "Total Top Down"),
        (eroi_min, eroi_min.map(e => powerProducedTopDown(cells, e, true)).map(_.to(Terawatts)), "Suitable Top Down"))
    }

    PlotHelper.plotXY(list, xLabel = "Minimum EROI", yLabel = "Global Wind Potential [TW]")
  }

  def plotEROIVSProd(eroi_min: List[Double], cells: List[Cell], topDown: Boolean = true) {
    val onshore = cells.filter(_.onshore)
    val offshore = cells.filter(i => !i.onshore)

    var list = List(
      (eroi_min, eroi_min.map(e => annualProduction(cells, e, false)).map(_.to(Exajoules)), "Total"),
      (eroi_min, eroi_min.map(e => annualProduction(cells, e, true)).map(_.to(Exajoules)), "Suitable"))
    if (topDown) {
      list = list ++ List(
        (eroi_min, eroi_min.map(e => annualProductionTopDown(cells, e, false)).map(_.to(Exajoules)), "Total Top Down"),
        (eroi_min, eroi_min.map(e => annualProductionTopDown(cells, e, true)).map(_.to(Exajoules)), "Suitable Top Down"))
    }
    PlotHelper.plotXY(list, xLabel = "EROImin", yLabel = "Global Potential [EJ/year]")

  }

  def listValueVSCumulated(values: List[(Double, Double)]): (List[Double], List[Double]) = {
    val sorted = values.sortBy(_._1).reverse
    (sorted.map(_._2).scanLeft(0.0)(_ + _), sorted.map(_._1) :+ 0.0)
  }

  def meanCD(cells: List[Cell], eroi_min: Double, suitable: Boolean = true) = {
    val subCells = cells.filter(_.optimalCD(eroi_min)._1)
    (subCells.map(c => (c.optimalCD(eroi_min)._2 * c.area(suitable))).foldLeft(Watts(0))(_ + _)) / (subCells.map(_.area(suitable)).foldLeft(SquareKilometers(0))(_ + _))
  }

  def powerProduced(cells: List[Cell], eroi_min: Double, suitable: Boolean = true) = cells.map(_.power(suitable, eroi_min)).foldLeft(Watts(0))(_ + _)
  def annualProduction(cells: List[Cell], eroi_min: Double, suitable: Boolean = true) = cells.map(_.annualProduction(suitable, eroi_min)).foldLeft(Joules(0))(_ + _)
  def annualProductionTopDown(cells: List[Cell], eroi_min: Double, suitable: Boolean = true) = cells.map(_.annualProductionTopDown(suitable, eroi_min)).foldLeft(Joules(0))(_ + _)
  def powerProducedTopDown(cells: List[Cell], eroi_min: Double, suitable: Boolean = true) = cells.map(_.powerTopDown(suitable, eroi_min)).foldLeft(Watts(0))(_ + _)

  def areaCovered(cells: List[Cell], eroi_min: Double, suitable: Boolean = true) = cells.map(c => if (c.getOptimalCD(eroi_min).toWattsPerSquareMeter > 0) c.area(suitable) else SquareKilometers(0)).foldLeft(SquareKilometers(0))(_ + _)
  def energyProducedFixedCD(cells: List[Cell], cd: Irradiance, eroi_min: Double, suitable: Boolean = true) = {
    cells.map(c =>
      if (c.cf * WakeEffect.arrayEfficiency(200, c.lambda(cd)) * 20 * 365 * 24 >= eroi_min * c.embodiedEnergy.to(MegawattHours))
        c.cf * c.area(suitable) * cd * WakeEffect.arrayEfficiency(200, c.lambda(cd)) * Hours(365 * 24)
      else Joules(0)).foldLeft(Joules(0))(_ + _)

  }
  def powerFixedCD(cells: List[Cell], cd: Irradiance, eroi_min: Double, suitable: Boolean = true) = {
    cells.map(c =>
      if (c.cf * WakeEffect.arrayEfficiency(200, c.lambda(cd)) * 20 * 365 * 24 >= eroi_min * c.embodiedEnergy.to(MegawattHours))
        c.cf * c.area(suitable) * cd * WakeEffect.arrayEfficiency(200, c.lambda(cd))
      else Watts(0)).foldLeft(Watts(0))(_ + _)

  }
}
object DefaultCell {
  def apply() = new Cell(0, 0, 0, SquareKilometers(0), SquareKilometers(0), Meters(0), Megawatts(0), Joules(0), Map(), WattsPerSquareMeter(0), "Country")
}
class Cell(val latitude: Double, val longitude: Double, val cf: Double, val totalArea: Area, val suitableArea: Area,
    val diameter: Length, val ratedP: Power, val embodiedEnergy: Energy, val optimalCD: Map[Double, (Boolean, Irradiance)],
    val verticalTransport: Irradiance, val country: String) {

  def onshore = (embodiedEnergy.toGigajoules <= 16000)

  def area(suitable: Boolean) = if (suitable) suitableArea else totalArea

  def getOptimalCD(e: Double) =
    if (!optimalCD.keySet.contains(e)) WattsPerSquareMeter(0)
    else if (!optimalCD(e)._1) WattsPerSquareMeter(0)
    else optimalCD(e)._2

  def lambda(cd: Irradiance) = cd * (diameter * diameter) / ratedP * Math.PI / 4
  def powerInstalled(suitable: Boolean, eroimin: Double): Power = powerInstalled(getOptimalCD(eroimin), suitable, eroimin)
  def powerInstalled(cd: Irradiance, suitable: Boolean, eroimin: Double): Power = cd * area(suitable)
  def power(suitable: Boolean, eroimin: Double): Power = power(getOptimalCD(eroimin), suitable, eroimin)
  def power(cd: Irradiance, suitable: Boolean, eroimin: Double): Power = {
    powerInstalled(cd, suitable, eroimin) * cf * WakeEffect.arrayEfficiency(200, lambda(cd))
  }
  def annualProduction(suitable: Boolean, eroimin: Double): Energy = annualProduction(getOptimalCD(eroimin), suitable, eroimin)
  def annualProduction(cd: Irradiance, suitable: Boolean, eroimin: Double): Energy = power(cd, suitable, eroimin) * Hours(365 * 24)

  def eroi(suitable: Boolean, eroimin: Double): Double = eroi(getOptimalCD(eroimin), suitable, eroimin)
  def eroi(cd: Irradiance, suitable: Boolean, eroimin: Double): Double = (annualProduction(cd, suitable, eroimin) * 20) / (embodiedEnergy * powerInstalled(cd, suitable, eroimin).toMegawatts)

  def powerTopDown(suitable: Boolean, eroimin: Double): Power = {
    val res = power(suitable, eroimin)
    if ((res / area(suitable)) > verticalTransport) verticalTransport * area(suitable)
    else res
  }
  def powerDensity(suitable: Boolean, eroimin: Double): Irradiance = (powerTopDown(suitable, eroimin) / area(suitable))
  def annualProductionTopDown(suitable: Boolean, eroimin: Double): Energy = powerTopDown(suitable, eroimin) * Hours(365 * 24)
  def productionDensity(suitable: Boolean, eroimin: Double): Irradiance = power(suitable, eroimin) / area(suitable)
}