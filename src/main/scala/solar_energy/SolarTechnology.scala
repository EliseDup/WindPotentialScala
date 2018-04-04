package solar_energy

import squants.energy._
import squants.time.Hours
import squants.radio.Irradiance

object PVPoly extends  SolarTechnology{
  val ee = new EmbodiedEnergy(Gigajoules(16605974),Gigajoules(143305),Gigajoules(653102),Gigajoules(52911),Gigajoules(0.0097))
  val efficiency = 0.17;
  val performanceRatio = 0.883;
  val occupationRatio =5.0;
  val directOnly = false;
}

object PVMono extends SolarTechnology {
  val ee = new EmbodiedEnergy(Gigajoules(14477204),Gigajoules(122559),Gigajoules(469376),Gigajoules(45251),Gigajoules(0.0097))
  val efficiency = 0.24;
  val performanceRatio = 0.883;
  val occupationRatio =5.0;
  val directOnly = false;
}

object CSP {
  def fullLoadHours(dni : Irradiance, sm : Double):Double  = fullLoadHours(dni.toWattsPerSquareMeter*8.76, sm)
  def fullLoadHours(dni_kWh_year : Double, sm : Double):Double  =  (2.5717*dni_kWh_year - 694)*(-0.0371*sm*sm + 0.4171*sm - 0.0744)
}

object CSPParabolic extends SolarTechnology {
  val ee = new EmbodiedEnergy(Gigajoules(20122927),Gigajoules(220400),Gigajoules(837985),Gigajoules(89118),Gigajoules(0.072)) 
  val efficiency = 0.16;
  val performanceRatio = 1.0;
  val occupationRatio = 7.5;
  val directOnly = true;
}

trait SolarTechnology {
  val ee : EmbodiedEnergy
  val efficiency: Double;
  val performanceRatio: Double;
  val occupationRatio: Double;
  val directOnly: Boolean
  
  def eroi(cf : Double) = ee.eroi(cf)
}



//object CSPParabolicStorage extends EmbodiedEnergy(Gigajoules(23097102),Gigajoules(457757),Gigajoules(1372567),Gigajoules(183720),Gigajoules(0.12))
//object CSPTowerStorage extends EmbodiedEnergy(Gigajoules(30601229),Gigajoules(457757),Gigajoules(1088293),Gigajoules(183720),Gigajoules(0.12))

class EmbodiedEnergy(val raw_materials : Energy,
    val construction_decomissioning : Energy, val transport_materials : Energy, val O_M_fixed : Energy,
    val O_M_output : Energy, val lifeTime : Int = 25){
    
  def eroi(cf : Double) = (lifeTime * Gigawatts(1) * cf * Hours(24*365))/embodiedEnergy1GW(cf)
  
  def embodiedEnergy1GW(cf : Double) = {
    raw_materials+construction_decomissioning+transport_materials+
    lifeTime*(O_M_fixed + (Gigawatts(1)*cf*Hours(24*365)).to(Gigajoules) *O_M_output)
  }
  def embodiedEnergy1GW(output_1GW_year : Energy) = {
    raw_materials+construction_decomissioning+transport_materials+
    lifeTime*(O_M_fixed + output_1GW_year.toGigajoules*O_M_output)
  }
  def embodiedEnergy(rated_power : Power, output_year : Energy): Energy = {
    val ratio = rated_power.toGigawatts
    ratio * embodiedEnergy1GW(output_year*ratio)
  }
  def embodiedEnergy(rated_power : Power, capacity_factor : Double) = {
    rated_power.toGigawatts * embodiedEnergy1GW(Gigawatts(1)*capacity_factor*Hours(24*365))
  }
}