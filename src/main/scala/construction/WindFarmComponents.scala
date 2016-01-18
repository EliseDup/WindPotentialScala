package construction

import squants.energy.Power
import squants.mass.Mass
import utils.Helper
import squants.energy.Megawatts
import squants.energy.Energy

/**
 *
 * Internal cables, transformer station and external cables to a wind farm
 *
 */
abstract class WindFarmComponents extends Components {
  val totalPower: Power
}

object WindFarmComponents {
  def apply(): WindFarmComponents = {
    val sheet = Helper.xlsSheet("ressources/windTurbines.xls", "transformer")
    val start = Helper.toInt(sheet.getRow(0), 0) - 1;
    val end = Helper.toInt(sheet.getRow(0), 1) - 1;
    new WindFarmComponents {
      val components = (start to end).map(r => {
        val row = sheet.getRow(r)
        val mass = Mass(Helper.toDouble(row, 1).toString() + Helper.toString(row, 2)).get
        val intensity = Energy(Helper.toDouble(row, 3).toString + Helper.toString(row, 4)).get
        val perUnit = Mass(Helper.toString(row, 5)).get
        (mass, Material(Helper.toString(row, 0), intensity, perUnit))
      }).toList
      val totalPower = Power(Helper.toString(sheet.getRow(2), 0)).get
    }
  }
}