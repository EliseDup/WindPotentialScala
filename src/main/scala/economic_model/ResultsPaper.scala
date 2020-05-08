package economic_model

import wind_energy.OnshoreWindTechnology
import wind_energy.OffshoreWindTechnology
import solar_energy.PVMono
import utils._
import squants.energy._
import squants.time.Hours

object ResultsPaper {
  import Helper._
  import PlotHelper._
  def main(args: Array[String]): Unit = {
    printCalibrationResults()
    printCalibrationResults(new calibration_results_work(energy_units = MegaTonOilEquivalent, pib_units = 1E9.toInt))

    Calibration.printTableCalibration_new(2017, List(15, 25), List(0.04, 0.08), List(0.04, 0.1), List(0.0))
  }

  def printCalibrationResults(c: calibration_results_work = new calibration_results_work()) {
    println("alpha : " + c.alpha * 100 + ", m :" + c.m * 100 + " , gpt : " + c.gpt * 100)
    println("EROI : " + c.eroi)
    println(c.pib_units + "US $ " + "/ " + c.energy_units.toString)
    
    println("Det > 0 ?" + "\t" + ((c.le * c.vf - c.lf * c.ve) > 0))
  }

  def plotParametersHistory {
    import Calibration._

    println((growth_rates(qf)).sum / (growth_rates(qf).size))
    println((growth_rates(lf)).sum / (growth_rates(lf).size))

    (0 until year_double.size).map(y => println(data.year(y) + "\t" + qe(y) + "\t" + qf(y) + "\t" + ve(y) + "\t" + vf(y) + "\t" + le(y) + "\t" + lf(y)))
    val res = List((qe, "qe"), (qf, " qf"), (ve, "ve"), (vf, "vf"), (le.map(_ * 1E6), "le"), (lf.map(_ * 1E6), "lf"))

    import CalibrationData._
    // res.map(r => plotXY(List((year_double, r._1, "")), yLabel = r._2, title = r._2))
    val year_growth = (1 until year_double.size).map(year_double(_)).toList
    plotXY(List((year_growth, CalibrationData.smooth_double(growth_rates(qf), (true, 10)).map(_ * 100), "")), yLabel = "qf growth rate [%/y]")
    plotXY(List((year_growth, CalibrationData.smooth_double(growth_rates(lf), (true, 10)).map(_ * 100), "")), yLabel = "lf growth rate [%/y]")

  }

}