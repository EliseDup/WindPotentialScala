package solar_energy

import squants.energy._
import squants.time.Hours

class EmbodiedEnergy(val raw_materials : Energy,
    val construction_decomissioning : Energy, val transport_materials : Energy, val O_M_fixed : Energy,
    val O_M_output : Energy, val lifeTime : Int = 25) {
  
  def embodiedEnergy1GW(output_year : Energy) = raw_materials+construction_decomissioning+transport_materials+lifeTime*(O_M_fixed+output_year.toGigajoules*O_M_output)
  def embodiedEnergy(rated_power : Power, output_year : Energy) = {
    rated_power.toGigawatts * embodiedEnergy1GW(output_year)
  }
  def embodiedEnergy(rated_power : Power, capacity_factor : Double) {
    rated_power.toGigawatts * embodiedEnergy1GW(rated_power*capacity_factor*Hours(24*365))
  }
}

object PVPoly extends EmbodiedEnergy(Gigajoules(16605974),Gigajoules(143305),Gigajoules(653102),Gigajoules(52911),Gigajoules(0.0097))
object PVMono extends EmbodiedEnergy(Gigajoules(14477204),Gigajoules(122559),Gigajoules(469376),Gigajoules(45251),Gigajoules(0.0097))
object CSPParabolic extends EmbodiedEnergy(Gigajoules(20122927),Gigajoules(220400),Gigajoules(837985),Gigajoules(89118),Gigajoules(0.072))
object CSPParabolicStorage extends EmbodiedEnergy(Gigajoules(23097102),Gigajoules(457757),Gigajoules(1372567),Gigajoules(183720),Gigajoules(0.12))
object CSPTowerStorage extends EmbodiedEnergy(Gigajoules(30601229),Gigajoules(457757),Gigajoules(1088293),Gigajoules(183720),Gigajoules(0.12))