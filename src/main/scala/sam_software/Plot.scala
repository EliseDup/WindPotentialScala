package sam_software

import utils._
import solar_energy.CSPParabolic
import squants.space.SquareMeters
import squants.energy.KilowattHours
import squants.energy.Gigawatts
import squants.energy.Megawatts
import squants.radio.WattsPerSquareMeter
import solar_energy.SolarTechnology
import solar_energy.CSPParabolicStorage12h
import solar_energy.CSPTowerStorage12h

object Plot {
  import PlotHelper._

  def main(args: Array[String]): Unit = {
    println("Parabolic no storage")
    plotSimulations("../resources/data_solar/sam_data/trough_simulations", 0, CSPParabolic)
     println("Parabolic 12h storage")
    plotSimulations("../resources/data_solar/sam_data/trough_simulations", 12, CSPParabolicStorage12h)
    // println("Tower 12h storage")
    //plotSimulations("../resources/data_solar/sam_data/tower_simulations", 12, CSPTowerStorage12h)
    //plotDNIvsEROI("../sam_data/latitude_dni_eff_tower12h_sm2_7",2.7,CSPTowerStorage12h)
    //plotDNIvsEROI("../sam_data/latitude_dni_eff_trough0h_sm1_3",1.3, CSPParabolic)
    //plotDNIvsEROI("../sam_data/latitude_dni_eff_trough0h_sm1_615",1.615, CSPParabolic)
  }

  def plotSimulations(file: String, storage: Int, technology: SolarTechnology) {
    // SM - Hours storage - DNI - efficiency
    val trough = Helper.getLines(file, "\t").map(i => new SAMData(i(0).toDouble, i(1).toInt, i(2).toDouble, i(3).toDouble)).filter(_.hours_storage == storage)
    val dni = (Set() ++ trough.map(_.daily_dni)).filter(_ * 365 >= 1500).toList.sorted
    val list = dni.map(d => {
      val x = trough.filter(_.daily_dni == d)
      val eff1_3 = x.find(_.sm == 1).get.efficiency
      (x.map(_.sm), x.map(_.efficiency), (d * 365).toInt + " kWh/m2/year")
    })
    val listEROI = dni.map(d => {
      val x = trough.filter(_.daily_dni == d)
      val eff1_3 = x.find(_.sm == 1).get.efficiency
      (x.map(_.sm), x.map(i => eroi(i.daily_dni * 365, i.sm, i.efficiency, technology)), (d * 365).toInt + " kWh/m2/year")
    })
    for (i <- list) {
      val max = i._2.zipWithIndex.maxBy(_._1)._2
      println("Max eff" + "\t" + i._3 + "\t" + i._1(max) + "\t" + i._2(max))
    }
    for (i <- listEROI) {
      val max = i._2.zipWithIndex.maxBy(_._1)._2
      println("Max EROI" + "\t" + i._3 + "\t" + i._1(max) + "\t" + i._2(max))
    }
    plotXY(list, xLabel = "Solar Multiple", yLabel = "Efficiency [%]", legend = false)
    plotXY(listEROI, xLabel = "Solar Multiple", yLabel = "EROI", legend = false)
  }
  def plotDNIvsEfficiency(data: String) {
    val res = Helper.getLines(data, "\t").map(i => (i(1).toDouble, i(2).toDouble))
    scatterPlot(res, xLabel = "DNI [kWh/m2/year]", yLabel = "Efficiency [%]")
  }

  def plotDNIvsEROI(data: String, sm: Double, technology: SolarTechnology) {
    val res = Helper.getLines(data, "\t").map(i => (i(1).toDouble, eroi(i(1).toDouble, sm, i(2).toDouble, technology)))
    scatterPlot(res, xLabel = "DNI [kWh/m2/year]", yLabel = "EROI")
  }
  def eroi(dni: Double, sm: Double, efficiency: Double, technology: SolarTechnology): Double = {
    val ratedPower = Megawatts(1)
    val area = ratedPower / (technology.designEfficiency * technology.designPointIrradiance) * sm
    val annualOutput = KilowattHours(dni * efficiency / 100 * area.toSquareMeters)
    30 * annualOutput / (technology.ee.embodiedEnergyArea(ratedPower, area))
  }

}

class SAMData(val sm: Double, val hours_storage: Int, val daily_dni: Double, val efficiency: Double)
