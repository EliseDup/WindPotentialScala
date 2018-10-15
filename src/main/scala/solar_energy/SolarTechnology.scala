package solar_energy

import squants.energy._
import squants.time.Hours
import squants.radio.Irradiance
import squants.space.Area
import squants.radio.WattsPerSquareMeter
import squants.space.SquareMeters
import squants.time.Time

trait PV extends SolarTechnology {
  val performanceRatio = 0.81 // 0.883;
  val occupationRatio = 5.0;
  val directOnly = false;
  val maximumSlope = 30.0
  val solarMultiple = 1.0
}
object PVPoly extends PV {
  val name = "PV Poly 17 %"
  val ee = new EmbodiedEnergy(Gigajoules(16605974), Gigajoules(143305), Gigajoules(653102), Gigajoules(52911), Gigajoules(0.0097), 25)
  val designEfficiency = 0.17;
  val degradationRate = 0.5 / 100
}
object PVMono extends PV {
  val name = "PV Mono 24 %"
  val ee = new EmbodiedEnergy(Gigajoules(14477204), Gigajoules(122559), Gigajoules(469376), Gigajoules(45251), Gigajoules(0.0097), 25)
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

  override val designPointIrradiance = WattsPerSquareMeter(950)
  val performanceRatio = 1.0;
  val occupationRatio = 7.5
  val directOnly = true;
  val maximumSlope = 2.0
  val degradationRate = 0.2 / 100

  // Aperture Area is the area needed to reach the rated power under a given irradiance conditions (dni design point) : Power = Area * DNI * efficiency
  // Then we add a solar multiple (in general 1,3 without storage) to have a security
  def apertureArea(ratedPower: Power, sm: Double = solarMultiple): Area = ratedPower * sm / (designPointIrradiance * designEfficiency)
  // override def yearlyProduction(solar : Irradiance, ratedPower : Power) = fullLoadHours(solar, solar_multiple) *ratedPower
  override def panelArea(ratedPower: Power) = apertureArea(ratedPower)
  override def eroi(cf: Double) = ee.eroi(cf, apertureArea(Gigawatts(1)))
  override def eroi(solar: Irradiance) = {
    val power = Gigawatts(1)
    val yearProd = yearlyProduction(solar, power)
    yearProd * ee.lifeTime / ee.embodiedEnergyArea(power, yearProd, apertureArea(power))
  }

  def eroi(solar: Irradiance, sm: Double) = {
    val power = Gigawatts(1)
    val yearProd = apertureArea(power, sm) * solar * lifeTimeEfficiency(solar) * Hours(365 * 24) //power * fullLoadHours(solar, sm) //
    yearProd * ee.lifeTime / ee.embodiedEnergyArea(power, yearProd, apertureArea(power, sm))
  }

  val optimal_sm: Boolean = true; val a_opt: Double; val b_opt: Double; val a: Double; val b: Double;
  override def efficiency(dni: Irradiance): Double = efficiency(dni, optimal_sm)
  def efficiency(dni: Irradiance, opt_sm: Boolean): Double = ((if (opt_sm) a_opt else a) * math.log(dni.toWattsPerSquareMeter * 8.76) + (if (opt_sm) b_opt else b)) / 100.0

}

/**
 * Efficiency with DNI was extrapolated using SAM simulations. For the Solar Multiple usually used, and for the Solar Multiple that maximizes the EROI
 * efficiency = a ln DNI + b
 *
 * Trough no storage, sm = 1.3 : 7.349 x - 42.12
 * Trough no storage, sm = 1.4 : 7.524 x - 42.64
 * Trough 12h storage, sm = 2.7 : 6.747 x - 36.72
 * Trough 12h storage, sm = 3.6 : 5.483 x - 28.34
 * Tower 12h storage, sm = 2.7 : 4.339 x - 16.97
 * Tower 12h storage, sm = 3.6 : 2.933 x - 7.435
 *
 */
object CSPParabolic extends CSP {
  val name = "CSP Parabolic through, no storage"
  val designEfficiency = 0.22
  val solarMultiple = if (optimal_sm) 1.4 else 1.3 // Optimized via SAM simulations
  val a = 7.349; val b = -42.12
  val a_opt = 7.524; val b_opt = -42.64
  val ee = new EmbodiedEnergy(Gigajoules(7032927), Gigajoules(220400), Gigajoules(356270), Gigajoules(2619 + 5215 + 89118), Gigajoules(0.05 + 0.05), 30,
    Gigajoules(1348389), Gigajoules(49617), SquareMeters(697286))
}

object CSPParabolicStorage12h extends CSP {
  val name = "CSP Parabolic through, 12 hours of storage"
  val designEfficiency = 0.22
  val solarMultiple = if (optimal_sm) 3.6 else 2.7 // Optimized via SAM simulations
  val a = 6.747; val b = -36.72
  val a_opt = 5.483; val b_opt = -28.34

  val ee = new EmbodiedEnergy(Gigajoules(12756143), Gigajoules(457757), Gigajoules(738320), Gigajoules(1985 + 3838 + 183720), Gigajoules(0.05 + 0.023), 30,
    Gigajoules(1067143), Gigajoules(65463), SquareMeters(1261286))
}

object CSPTowerStorage12h extends CSP {
  val name = "CSP Power Tower, 12 hours of storage"
  val designEfficiency = 0.21
  val solarMultiple = if (optimal_sm) 3.6 else 2.7 // Optimized via SAM simulations
  val a = 4.339; val b = -16.97
  val a_opt = 2.933; val b_opt = -7.435
  val ee = new EmbodiedEnergy(Gigajoules(18379658), Gigajoules(457757), Gigajoules(1425920), Gigajoules(1985 + 3838 + 183720), Gigajoules(0.05 + 0.023), 30,
    Gigajoules(2116786), Gigajoules(52168), SquareMeters(1443932))
}

trait SolarTechnology {
  val name: String;
  val ee: EmbodiedEnergy;
  val designPointIrradiance: Irradiance = WattsPerSquareMeter(1000);
  val performanceRatio: Double;
  val degradationRate: Double
  val occupationRatio: Double;
  val solarMultiple: Double;
  val directOnly: Boolean;
  val maximumSlope: Double;
  val designEfficiency: Double;

  def efficiency(i: Irradiance): Double = designEfficiency
  def lifeTimeEfficiency(i: Irradiance) =
    if (degradationRate == 0) efficiency(i) * performanceRatio
    else efficiency(i) * performanceRatio * ((1.0 - math.pow(1.0 - degradationRate, ee.lifeTime)) / degradationRate) / ee.lifeTime
  def panelArea(ratedPower: Power): Area = ratedPower / (designPointIrradiance * designEfficiency) * solarMultiple
  def potential(solar: Irradiance, ratedPower: Power): Power = panelArea(ratedPower) * solar * lifeTimeEfficiency(solar)
  def yearlyProduction(solar: Irradiance, ratedPower: Power): Energy = potential(solar, ratedPower) * Hours(365 * 24)

  def eroi(cf: Double) = ee.eroi(cf)
  def eroi(solar: Irradiance) = {
    val power = Gigawatts(1)
    val yearProd = yearlyProduction(solar, power)
    yearProd * ee.lifeTime / ee.embodiedEnergy(power, yearProd)
  }
}

//object CSPParabolicStorage extends EmbodiedEnergy(Gigajoules(23097102),Gigajoules(457757),Gigajoules(1372567),Gigajoules(183720),Gigajoules(0.12))
//object CSPTowerStorage extends EmbodiedEnergy(Gigajoules(30601229),Gigajoules(457757),Gigajoules(1088293),Gigajoules(183720),Gigajoules(0.12))

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