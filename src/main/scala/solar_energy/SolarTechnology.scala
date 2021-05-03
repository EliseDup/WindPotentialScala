package solar_energy

import squants.energy._
import squants.time.Hours
import squants.radio.Irradiance
import squants.space.Area
import squants.radio.WattsPerSquareMeter
import squants.space.SquareMeters
import squants.time.Time
import squants.space.SquareKilometers
import wind_solar.RenewableTechnology
import wind_solar.Cell
import wind_solar.EmbodiedEnergy
import utils.Petajoules
import wind_solar.EmbodiedEnergy

trait SolarTechnology extends RenewableTechnology {

  val wind = false; val solar = true;
  val designPointIrradiance: Irradiance;
  val performanceRatio: Double;
  val degradationRate: Double;
  def max_eroi_sm(solar: Irradiance): Double = 1.0;
  val directOnly: Boolean;
  val maximumSlope: Double;
  val designEfficiency: Double;

  // GHI for PV, DNI for CSP
  def solar(cell: Cell): Irradiance = if (directOnly) cell.dni else cell.ghi;
  def reflectiveArea(cell: Cell): Area = (cell.area * suitabilityFactor(cell) / occupationRatio)
  def embodiedEnergyArea(ratedPower: Power, area: Area): Energy = super.embodiedEnergy(ratedPower)

  def potential(cell: Cell, eroi_min: Double): Power =
    if (eroi(cell, eroi_min) >= eroi_min) reflectiveArea(cell) * solar(cell) * lifeTimeEfficiency(solar(cell)) * (1 - operation_variable)
    else Watts(0)

  def potential(cell: Cell): Power = reflectiveArea(cell) * solar(cell) * lifeTimeEfficiency(solar(cell)) * (1 - operation_variable)

  override def eroi(cell: Cell, eroi_min: Double): Double = {
    val wi = ratedPower(cell, eroi_min)
    if (wi.value == 0) 0.0
    else {
      val out_year = potential(cell) * Hours(365 * 24)
      out_year / (1 - operation_variable) * lifeTime / (embodiedEnergy(cell, eroi_min) + operation_variable * out_year / (1 - operation_variable) * lifeTime)
    }
  }
  override def eroi_pou(cell: Cell, eroi_min: Double, distr_losses: Double): Double = {
  val wi = ratedPower(cell, eroi_min)
    if (wi.value == 0) 0.0
    else {
      val out_year = potential(cell) * Hours(365 * 24)
      out_year * (1 - distr_losses) * lifeTime / (embodiedEnergy(cell, eroi_min) + (operation_variable / (1-operation_variable) + distr_losses) * out_year * lifeTime)
    }
 
  }
  
 override def eroi_pou_external(cell: Cell, eroi_min: Double, distr_losses: Double): Double = {
  val wi = ratedPower(cell, eroi_min)
    if (wi.value == 0) 0.0
    else {
      val out_year = potential(cell) * Hours(365 * 24)
      out_year * (1 - distr_losses) * lifeTime / (embodiedEnergy(cell, eroi_min) +  distr_losses * out_year * lifeTime)
    } 
 }
 
  override def geer(cell: Cell, eroi_min: Double): Double = {
    val wi = ratedPower(cell, eroi_min)
    if (wi.value == 0) 0.0
    else {
      val out_year = potential(cell) * Hours(365 * 24)
      out_year * lifeTime / embodiedEnergy(cell, eroi_min)
    }
  }

  def ratedPower(cell: Cell, eroi_min: Double): Power = {
    reflectiveArea(cell) * designPointIrradiance * designEfficiency / max_eroi_sm(solar(cell))
  }

  override def suitabilityFactor(cell: Cell): Double = super.suitabilityFactor(cell) * (if (solar(cell).value > 0) 1.0 else 0.0) * cell.landCovers.suitabilityFactorSolar * cell.slope.slope_leq(maximumSlope)

  def efficiency(i: Irradiance): Double = designEfficiency
  def lifeTimeEfficiency(i: Irradiance) =
    if (degradationRate == 0) efficiency(i) * performanceRatio
    else efficiency(i) * performanceRatio * ((1.0 - math.pow(1.0 - degradationRate, ee.lifeTime)) / degradationRate) / ee.lifeTime
  def eroi(solar: Irradiance) = {
    val ratedPower = Gigawatts(1)
    val energyPerYear = yearlyProduction(solar, panelArea(ratedPower, solar))
    energyPerYear / (1 - operation_variable) * ee.lifeTime / (embodiedEnergyArea(ratedPower, panelArea(ratedPower, solar)) + energyPerYear / (1 - operation_variable) * operation_variable * ee.lifeTime)
  }
    def eroi_pou(solar: Irradiance, distr_losses : Double) = {
    val ratedPower = Gigawatts(1)
    val energyPerYear = yearlyProduction(solar, panelArea(ratedPower, solar))
    energyPerYear * (1 - distr_losses) * ee.lifeTime / (embodiedEnergyArea(ratedPower, panelArea(ratedPower, solar)) + energyPerYear * (operation_variable/ (1 - operation_variable) + distr_losses) * ee.lifeTime)
  }
  def geer(solar: Irradiance) = {
    val ratedPower = Gigawatts(1)
    val energyPerYear = yearlyProduction(solar, panelArea(ratedPower, solar))
    energyPerYear * ee.lifeTime / embodiedEnergyArea(ratedPower, panelArea(ratedPower, solar))
  }
  def capacityFactor(solar: Irradiance): Double = (solar.toWattsPerSquareMeter * lifeTimeEfficiency(solar)) / ratedPower(SquareMeters(1), solar).toWatts
  def potential(solar: Irradiance, panelArea: Area): Power = panelArea * solar * lifeTimeEfficiency(solar)
  def yearlyProduction(solar: Irradiance, panelArea: Area): Energy = potential(solar, panelArea) * Hours(365 * 24)
  // The size of the power block depending on design conditions, and solar multiple for CSP plants
  def ratedPower(panelArea: Area, solar: Irradiance) = panelArea * (designPointIrradiance * designEfficiency) / max_eroi_sm(solar)
  def panelArea(ratedPower: Power, solar: Irradiance): Area = {
    ratedPower / (designPointIrradiance * designEfficiency) * max_eroi_sm(solar)
  }
}

trait PV extends SolarTechnology {
  val csp = false; val pv = true;
  val designPointIrradiance: Irradiance = WattsPerSquareMeter(1000)
  val performanceRatio = 0.81 // 0.883;
  val occupationRatio = 5.0;
  val directOnly = false;
  val maximumSlope = 30.0
  val lifeTime = 25

}

object PVPoly extends PV {
  val name = "poly-Si-PV"
  val ee = new EmbodiedEnergy(Gigawatts(1), Gigajoules(12211495), Gigajoules(4394479), Gigajoules(71652), Gigajoules(71652), Gigajoules(652934), Gigajoules(52911.81), 0.0097, 25)
  val designEfficiency = 0.17;
  val degradationRate = 0.5 / 100
}
object PVMono extends PV {
  val name = "mono-Si-PV"
  val ee = new EmbodiedEnergy(Gigawatts(1), Gigajoules(8880746), Gigajoules(5596459), Gigajoules(61279), Gigajoules(61279), Gigajoules(469257), Gigajoules(45252), 0.0097, 25)
  // Gigajoules(63885) / GW / Year of maintenance in 2018 paper on 15 energy generation technology ..
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
  val csp = true; val pv = false;
  def fullLoadHours(dni: Irradiance, sm: Double): Time = fullLoadHours(dni.toWattsPerSquareMeter * 8.76, sm)
  def fullLoadHours(dni_kWh_year: Double, sm: Double): Time = Hours((2.5717 * dni_kWh_year - 694) * (-0.0371 * sm * sm + 0.4171 * sm - 0.0744))
  val lifeTime = 30
  val designPointIrradiance = WattsPerSquareMeter(950)
  val performanceRatio = 1.0;
  val occupationRatio = 7.5
  val directOnly = true;
  val maximumSlope = 2.0
  val degradationRate = 0.2 / 100
  val sm_range: List[Double]

  // Embodied energy specific calculations: transport and construction energy inputs depending on the actual
  // size of the solar field
  val transport_variable: Energy
  val construction_variable: Energy
  val default_aperture_area: Area;
  override def embodiedEnergyArea(ratedPower: Power, area: Area): Energy = super.embodiedEnergy(ratedPower) + area / default_aperture_area * (transport_variable + construction_variable)

  def a(sm: Double): Double; def b(sm: Double): Double;
  // Aperture Area is the area needed to reach the rated power under a given irradiance conditions (dni design point) : Power = Area * DNI * efficiency
  // Then we add a solar multiple (in general 1,3 without storage) to have a security
  // The solar mutiple that maximises the EROI
  override def max_eroi_sm(solar: Irradiance) = {
    val sm = sm_range.zipWithIndex
    sm(sm.map(s => (s, eroi(solar, s._1))).maxBy(_._2)._1._2)._1
  }
  def max_efficiency_sm(solar: Irradiance) = {
    val sm = sm_range.zipWithIndex
    sm(sm.map(s => (s, efficiency(solar, s._1))).maxBy(_._2)._1._2)._1
  }
  def max_net_energy_sm(solar: Irradiance, area: Area) = {
    val sm = sm_range.zipWithIndex
    sm(sm.map(s => (s, netPotential(solar, area / occupationRatio, s._1).toWatts)).maxBy(_._2)._1._2)._1
  }
  def efficiency(dni: Irradiance, sm: Double): Double = (a(sm) * math.log(dni.toWattsPerSquareMeter * 8.76) + b(sm)) / 100.0
  def lifeTimeEfficiency(dni: Irradiance, sm: Double): Double = efficiency(dni, sm) * performanceRatio * ((1.0 - math.pow(1.0 - degradationRate, ee.lifeTime)) / degradationRate) / ee.lifeTime

  def eroi(solar: Irradiance, sm: Double): Double = {
    if (solar.value == 0) 0.0
    else {
      val power = Gigawatts(1)
      val yearProd = yearlyProduction(solar, panelArea(power, sm), sm)
      // yearProd * (1 - operation_variable) * ee.lifeTime / (embodiedEnergyArea(power, panelArea(power, sm)) + yearProd * (1 - operation_variable) * ee.lifeTime * operation_variable)
      yearProd / (1 - operation_variable) * ee.lifeTime / (embodiedEnergyArea(power, panelArea(power, sm)) + yearProd * operation_variable / (1 - operation_variable) * ee.lifeTime)

    }
  }
  def corrected_eroi(solar : Irradiance, factor : Double)={
    if (solar.value == 0) 0.0
    else {
      val sm = max_eroi_sm(solar)
      val power = Gigawatts(1)
      val yearProd = yearlyProduction(solar, panelArea(power, sm), sm)*factor
      // yearProd * (1 - operation_variable) * ee.lifeTime / (embodiedEnergyArea(power, panelArea(power, sm)) + yearProd * (1 - operation_variable) * ee.lifeTime * operation_variable)
      yearProd / (1 - operation_variable) * ee.lifeTime / (embodiedEnergyArea(power, panelArea(power, sm)) + yearProd * operation_variable / (1 - operation_variable) * ee.lifeTime)

    }
  }
    def eroi_dynamic(solar: Irradiance, sm: Double, improvment : Double): Double = {
    if (solar.value == 0) 0.0
    else {
      val power = Gigawatts(1)
      val yearProd = yearlyProduction(solar, panelArea(power, sm), sm)
      // yearProd * (1 - operation_variable) * ee.lifeTime / (embodiedEnergyArea(power, panelArea(power, sm)) + yearProd * (1 - operation_variable) * ee.lifeTime * operation_variable)
      yearProd / (1 - operation_variable) * ee.lifeTime / (improvment*embodiedEnergyArea(power, panelArea(power, sm)) + yearProd * operation_variable / (1 - operation_variable) * ee.lifeTime)

    }
  }
  def eroi_pou(solar: Irradiance, sm: Double, distr_losses: Double): Double = {
    if (solar.value == 0) 0.0
    else {
      val power = Gigawatts(1)
      val yearProd = yearlyProduction(solar, panelArea(power, sm), sm)
      yearProd * (1 - distr_losses) * ee.lifeTime / (embodiedEnergyArea(power, panelArea(power, sm)) + yearProd * (distr_losses + operation_variable / (1 - operation_variable)) * ee.lifeTime)
    }
  }
  def eroi_pou_external(solar: Irradiance, sm: Double, distr_losses: Double): Double = {
    if (solar.value == 0) 0.0
    else {
      val power = Gigawatts(1)
      val yearProd = yearlyProduction(solar, panelArea(power, sm), sm)
      yearProd * (1 - distr_losses) * ee.lifeTime / (embodiedEnergyArea(power, panelArea(power, sm)) + yearProd * distr_losses * ee.lifeTime)
    }
  }
  def potential(solar: Irradiance, panelArea: Area, sm: Double): Power = {
    List(panelArea * solar * lifeTimeEfficiency(solar, sm) * (1 - operation_variable), ratedPower(panelArea, sm)).minBy(_.value)
  }
  def netPotential(solar: Irradiance, panelArea: Area, sm: Double): Power = {
    potential(solar, panelArea, sm) - (embodiedEnergyArea(ratedPower(panelArea, sm), panelArea)) / Hours(lifeTime * 365 * 24)
  }
  def yearlyProduction(solar: Irradiance, panelArea: Area, sm: Double): Energy = potential(solar, panelArea, sm) * Hours(365 * 24)
  def panelArea(ratedPower: Power, sm: Double) = ratedPower / (designPointIrradiance * designEfficiency) * sm
  def ratedPower(panelArea: Area, sm: Double) = panelArea * (designPointIrradiance * designEfficiency) / sm

  override def efficiency(solar: Irradiance): Double = efficiency(solar, max_eroi_sm(solar))
  
  override def eroi(cell: Cell, eroi_min: Double): Double = {
    if (suitabilityFactor(cell) == 0 || ratedPower(cell, eroi_min).value == 0) 0.0
    else eroi(cell.dni, max_eroi_sm(cell.dni))
  }
  override def eroi_dynamic(cell: Cell, eroi_min: Double, improvment : Double): Double = {
     if (suitabilityFactor(cell) == 0 || ratedPower(cell, eroi_min).value == 0) 0.0
    else eroi_dynamic(cell.dni, max_eroi_sm(cell.dni),improvment)
  }
  override def eroi_pou(cell: Cell, eroi_min: Double, distr_losses: Double): Double = {
      if (suitabilityFactor(cell) == 0 || ratedPower(cell, eroi_min).value == 0) 0.0
      else eroi_pou(cell.dni, max_eroi_sm(cell.dni), distr_losses)
    }
  
  override def eroi_pou_external(cell: Cell, eroi_min: Double, distr_losses: Double): Double = {
    if (suitabilityFactor(cell) == 0 || ratedPower(cell, eroi_min).value == 0) 0.0
    else eroi_pou_external(cell.dni, max_eroi_sm(cell.dni), distr_losses)
  }
  
  override def eroi(solar: Irradiance): Double = eroi(solar, max_eroi_sm(solar))

  override def potential(solar: Irradiance, panelArea: Area): Power = potential(solar, panelArea, max_eroi_sm(solar))

  override def yearlyProduction(solar: Irradiance, panelArea: Area): Energy = yearlyProduction(solar, panelArea, max_eroi_sm(solar))
  def capacityFactor(solar: Irradiance, panelArea: Area, sm: Double): Double = if (solar.value != 0 && panelArea.value != 0) potential(solar, panelArea, sm) / ratedPower(panelArea, sm) else 0.0

  override def energyInputsInstallation(cell: Cell, eroi_min: Double) = {
    val area_ratio = reflectiveArea(cell) / default_aperture_area
    super.energyInputsInstallation(cell, eroi_min) + area_ratio * (transport_variable + construction_variable)
  }
}

/**
 * Efficiency with DNI was extrapolated using SAM simulations. For the Solar Multiple usually used, and for the Solar Multiple that maximizes the EROI
 * efficiency = (a*sm + b) ln DNI + c*sm + d
 *
 */

object CSPParabolic extends CSP {
  val name = "PT-oil"
  val designEfficiency = 0.22
  def a(sm: Double) = -3.38 * sm + 11.55
  def b(sm: Double) = 23.85 * sm - 72.26
  val sm_range = (5 to 25).map(_ * 0.1).toList

  val default_aperture_area = SquareMeters(1E9 / (950 * designEfficiency) * 1.3)
  val transport_variable = Gigajoules(479183)
  val construction_variable = Gigajoules(6033371 + 6732247)

  val ee = new EmbodiedEnergy(Gigawatts(1), Gigajoules(4742245), Gigajoules(3178), Gigajoules(114400), Gigajoules(106001), Gigajoules(732751), Gigajoules(89118), 0.05 + 0.023, 30)
}

object CSPParabolicStorage12h extends CSP {
  val name = "PT-salt-TES"
  val designEfficiency = 0.22
  val sm_range = (5 to 40).map(_ * 0.1).toList // (10 to 40).map(_ * 0.1).toList
  def a(sm: Double) = -1.578 * sm + 11.17
  def b(sm: Double) = 10.65 * sm - 66.33

  val default_aperture_area = SquareMeters(1E9 * 2.7 / (950 * designEfficiency))
  val transport_variable = Gigajoules(930204)
  val construction_variable = Gigajoules(11434969 + 4412439)

  val ee = new EmbodiedEnergy(Gigawatts(1), Gigajoules(5415779), Gigajoules(13500041), Gigajoules(220157), Gigajoules(237600), Gigajoules(756412 + 1080164), Gigajoules(183720), 0.05 + 0.023, 30) //0.15,30)
}

object CSPTowerStorage12h extends CSP {
  val name = "ST-salt-TES"
  val designEfficiency = 0.21
  val sm_range = (5 to 40).map(_ * 0.1).toList // (10 to 40).map(_ * 0.1).toList
  def a(sm: Double) = -1.62 * sm + 8.742
  def b(sm: Double) = 11.01 * sm - 46.86

  val default_aperture_area = SquareMeters(1E9 * 2.7 / (950 * designEfficiency))
  val transport_variable = Gigajoules(457262)
  val construction_variable = Gigajoules(7751182 + 3642818)

  val ee = new EmbodiedEnergy(Gigawatts(1), Gigajoules(8053825), Gigajoules(9086794), Gigajoules(220157), Gigajoules(237600), Gigajoules(1196178 + 727130), Gigajoules(183720), 0.05 + 0.023, 30) // 0.15,30)
}