package solar_energy

import squants.energy._
import squants.time.Hours
import squants.radio.Irradiance
import squants.space.Area
import squants.radio.WattsPerSquareMeter
import squants.space.SquareMeters
import squants.time.Time

trait PV extends SolarTechnology {
  val designPointIrradiance: Irradiance = WattsPerSquareMeter(1000)
  val performanceRatio = 0.81 // 0.883;
  val occupationRatio = 5.0;
  val directOnly = false;
  val maximumSlope = 30.0
}

object PVPoly extends PV {
  val name = "poly-Si-PV"
  val ee = new EmbodiedEnergy(Gigajoules(12211495+4394479), Gigajoules(71652+71652), Gigajoules(653102), Gigajoules(52911.81), Gigajoules(0.0097), 25)
  val designEfficiency = 0.17;
  val degradationRate = 0.5 / 100
}
object PVMono extends PV {
  val name = "mono-Si-PV"
  val ee = new EmbodiedEnergy(Gigajoules(14477204), Gigajoules(122559), Gigajoules(469378), Gigajoules(45251), Gigajoules(0.0097), 25)
  val designEfficiency = 0.24;
  val degradationRate = 0.36 / 100
}

/**
 * Calculation Energy ADJUSTMENT FACTOR
 *
 * Ea = Actual Area / Reference Area = Rated Power * Solar Multiple / (Reference DNI * efficiency) / Reference Area
 *
 * Reference area for energy inputs calculation :
 *
 * CSP parabolic trough excl. storage : 697286 m² * 1.3
 * CSP-Parabolic trough + 12 h molten salt: 1261286 m² * 2.7
 * CSP-Power tower + 12 h molten salt:  1443932 m² * 2.7
 *
 * CSP parabolic trough excl. storage :  Ea = (1E9*1.3/(950*0.15)) / 697286 = 15.02
 * CSP-Parabolic trough + 12 h molten salt: Ea = (1E9*2.7/(950*0.15)) / 1261286 = 15.02
 * CSP-Power tower + 12 h molten salt: Ea = (1E9*2.7/(950*0.15)) / 1443932 = 15.02
 *
 */

trait CSP extends SolarTechnology {
  def fullLoadHours(dni: Irradiance, sm: Double): Time = fullLoadHours(dni.toWattsPerSquareMeter * 8.76, sm)
  def fullLoadHours(dni_kWh_year: Double, sm: Double): Time = Hours((2.5717 * dni_kWh_year - 694) * (-0.0371 * sm * sm + 0.4171 * sm - 0.0744))

  val designPointIrradiance = WattsPerSquareMeter(950)
  val performanceRatio = 1.0;
  val occupationRatio = 7.5
  val directOnly = true;
  val maximumSlope = 2.0
  val degradationRate = 0.2 / 100
  
  val sm_range: List[Double]
  def a(sm :Double) : Double; def b(sm:Double): Double;  
  // Aperture Area is the area needed to reach the rated power under a given irradiance conditions (dni design point) : Power = Area * DNI * efficiency
  // Then we add a solar multiple (in general 1,3 without storage) to have a security
  // The solar mutiple that maximises the EROI
  override def max_eroi_sm(solar: Irradiance) = {
    val sm = sm_range.zipWithIndex
    sm(sm.map(s => (s,eroi(solar, s._1))).maxBy(_._2)._1._2)._1
  }
  def max_efficiency_sm(solar: Irradiance) = {
    val sm = sm_range.zipWithIndex
    sm(sm.map(s => (s,efficiency(solar, s._1))).maxBy(_._2)._1._2)._1
  }
  def efficiency(dni: Irradiance, sm: Double): Double = (a(sm) * math.log(dni.toWattsPerSquareMeter * 8.76) + b(sm)) / 100.0
  def lifeTimeEfficiency(dni: Irradiance, sm: Double): Double = efficiency(dni, sm) * performanceRatio * ((1.0 - math.pow(1.0 - degradationRate, ee.lifeTime)) / degradationRate) / ee.lifeTime

  def eroi(solar: Irradiance, sm: Double):Double = {
    val power = Gigawatts(1)
    val yearProd = yearlyProduction(solar, panelArea(power, sm), sm)
    yearProd * ee.lifeTime / ee.embodiedEnergyArea(power, yearProd, panelArea(power, sm))
  }
  def potential(solar: Irradiance, panelArea: Area, sm: Double): Power = panelArea * solar * lifeTimeEfficiency(solar, sm)
  def yearlyProduction(solar: Irradiance, panelArea: Area, sm: Double): Energy = potential(solar, panelArea, sm) * Hours(365 * 24)
  def panelArea(ratedPower : Power, sm : Double) = ratedPower / (designPointIrradiance * designEfficiency) * sm
  
  override def efficiency(solar: Irradiance): Double = efficiency(solar, max_eroi_sm(solar))
  override def eroi(solar: Irradiance): Double = eroi(solar, max_eroi_sm(solar))
  override def potential(solar: Irradiance, panelArea: Area): Power = potential(solar, panelArea, max_eroi_sm(solar))
  override def yearlyProduction(solar: Irradiance, panelArea: Area): Energy = yearlyProduction(solar, panelArea, max_eroi_sm(solar))
  
}

/**
 * Efficiency with DNI was extrapolated using SAM simulations. For the Solar Multiple usually used, and for the Solar Multiple that maximizes the EROI
 * efficiency = (a*sm + b) ln DNI + c*sm + d
 *
 */

object CSPParabolic extends CSP {
  val name = "PTPP"
  val designEfficiency = 0.22
  def a(sm : Double) = -3.38*sm + 11.55
  def b(sm : Double) = 23.85*sm - 72.26
  val sm_range = (10 to 50).map(_*0.1).toList
  val ee = new EmbodiedEnergy(Gigajoules(7032927), Gigajoules(220400), Gigajoules(356270), Gigajoules(2619 + 5215 + 89118), Gigajoules(0.05 + 0.05), 30,
    Gigajoules(1348389), Gigajoules(49617), SquareMeters(607286))

}

object CSPParabolicStorage12h extends CSP {
  val name = "PTPP-TES"
  val designEfficiency = 0.22
  
  val sm_range = (10 to 100).map(_*0.1).toList
  
  def a(sm : Double) =  -1.578*sm + 11.17
  def b(sm : Double) = 10.65*sm - 66.33
  
  val ee = new EmbodiedEnergy(Gigajoules(12756143), Gigajoules(457757), Gigajoules(738320), Gigajoules(1985 + 3838 + 183720), Gigajoules(0.05 + 0.023), 30,
    Gigajoules(1067143), Gigajoules(65463), SquareMeters(1261286))
}

object CSPTowerStorage12h extends CSP {
  val name = "STPP-TES"
  val designEfficiency = 0.21
  val sm_range = (10 to 100).map(_*0.1).toList
  def a(sm : Double) = -1.62*sm + 8.742
  def b(sm : Double) = 11.01*sm - 46.86
  
  val ee = new EmbodiedEnergy(Gigajoules(18379658), Gigajoules(457757), Gigajoules(1425920), Gigajoules(3723 + 7197 + 183720), Gigajoules(0.05 + 0.023), 30,
    Gigajoules(1329266), Gigajoules(52168), SquareMeters(1443932))
}

trait SolarTechnology {
  val name: String;
  val ee: EmbodiedEnergy;
  val designPointIrradiance: Irradiance;
  val performanceRatio: Double;
  val degradationRate: Double
  val occupationRatio: Double;
  def max_eroi_sm(solar: Irradiance): Double = 1.0;
  val directOnly: Boolean;
  val maximumSlope: Double;
  val designEfficiency: Double;

  def efficiency(i: Irradiance): Double = designEfficiency
  def lifeTimeEfficiency(i: Irradiance) =
    if (degradationRate == 0) efficiency(i) * performanceRatio
    else efficiency(i) * performanceRatio * ((1.0 - math.pow(1.0 - degradationRate, ee.lifeTime)) / degradationRate) / ee.lifeTime

  def potential(solar: Irradiance, panelArea: Area): Power = panelArea * solar * lifeTimeEfficiency(solar)
  def yearlyProduction(solar: Irradiance, panelArea: Area): Energy = potential(solar, panelArea) * Hours(365 * 24)
  // The size of the power block depending on design conditions, and solar multiple for CSP plants
  def ratedPower(panelArea: Area, solar : Irradiance) = panelArea * (designPointIrradiance * designEfficiency) / max_eroi_sm(solar)
  def panelArea(ratedPower: Power, solar : Irradiance): Area = ratedPower / (designPointIrradiance * designEfficiency) * max_eroi_sm(solar)

  def capacityFactor(solar: Irradiance, panelArea: Area): Double = if (solar.value != 0 && panelArea.value != 0) potential(solar, panelArea) / ratedPower(panelArea,solar) else 0.0

  def eroi(cf: Double) = ee.eroi(cf)
  def eroi(solar: Irradiance) = {
    val power = Gigawatts(1)
    val yearProd = yearlyProduction(solar, panelArea(power,solar))
    yearProd * ee.lifeTime / ee.embodiedEnergy(power, yearProd)
  }
}

class EmbodiedEnergy(val raw_materials: Energy,
    val construction_decomissioning: Energy, val transport_materials: Energy, val O_M_fixed: Energy,
    val O_M_output: Energy, val lifeTime: Int, val construction_variable: Energy = Joules(0), val transport_variable: Energy = Joules(0), val default_area: Area = SquareMeters(1)) {

  def eroi(cf: Double) = (lifeTime * Gigawatts(1) * cf * Hours(24 * 365)) / embodiedEnergy1GW(cf)
  def eroi(cf: Double, area: Area) = {
    val output_year = Gigawatts(1) * cf * Hours(24 * 365)
    output_year * lifeTime / embodiedEnergyArea(Gigawatts(1), output_year, area)
  }

  def embodiedEnergy1GW(output_1GW_year: Energy) =
    raw_materials + construction_decomissioning + transport_materials +
      lifeTime * (O_M_fixed + output_1GW_year.toGigajoules * O_M_output)

  def embodiedEnergy1GW(cf: Double): Energy = embodiedEnergy1GW(Gigawatts(1) * cf * Hours(24 * 365))

  def embodiedEnergy(rated_power: Power, output_year: Energy): Energy = {
    val ratio = rated_power.toGigawatts
    ratio * embodiedEnergy1GW(output_year / ratio)
  }

  def embodiedEnergy(rated_power: Power, capacity_factor: Double) = {
    rated_power.toGigawatts * embodiedEnergy1GW(capacity_factor)
  }
  // For CSP, the embodied energy was calculated for a default aperture area !
  def embodiedEnergyArea(rated_power: Power, output_year: Energy, area: Area): Energy = {
    val area_ratio = area / default_area
    embodiedEnergy(rated_power, output_year) + area_ratio * (transport_variable + construction_variable)
  }
  // For csp optimzation
  val fixed1MW = Megawatts(1).toGigawatts * (raw_materials + construction_decomissioning + transport_materials + O_M_fixed * lifeTime)
  val variable1MW = {
    val area_ratio = Megawatts(1) / (WattsPerSquareMeter(950) * 0.22) / default_area
    area_ratio * (transport_variable + construction_variable)
  }
}