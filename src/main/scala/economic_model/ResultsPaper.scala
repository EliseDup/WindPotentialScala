package economic_model

import wind_energy.OnshoreWindTechnology
import wind_energy.OffshoreWindTechnology
import solar_energy.PVMono

object ResultsPaper {

  def main(args: Array[String]): Unit = {
    val c = Calibration2017(25, 0.05, 0.065)

    println(c.k + "\t" + c.ki + "\t" + c.ks + "\t" + (c.k > c.ki && c.k < c.ks))
    println(c.gk * 100 + "\t" + c.gki * 100 + "\t" + c.gks * 100 + "\t" + (c.gk > c.gki && c.gk < c.gks))
    val techs = List((OnshoreWindTechnology,0.25),(OffshoreWindTechnology,0.25),(PVMono,0.5))
    val res = GrowthModel.calculate(c.Ye, c.T,techs, 0.25)
  }
}