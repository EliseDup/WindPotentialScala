package windEnergy

import squants.mass._
import squants.space._
import squants.energy._
import squants.time._
import construction._
import transportation.Transport

object WindPowerTransmission {

  // --- OFFSHORE ---
  /**
   * Life cycle assessment of an offshore grid interconnection wind farms and customers across the North Sea
   *
   * Arvesen et al., 2014
   *
   */

  // Components/600 MW Wind Farm
  val internalOffshoreCables = Product(
    KilowattHours(4.45E6) + Megajoules(4.45E6),
    // Transport until harbor + vessel
    Transport.road * 1.16E6 + Megajoules(8.36E4),
    (Tonnes(1890), Copper),
    (Tonnes(2010), Steel),
    (Tonnes(1280), Lead),
    (Tonnes(345), Polyethylene),
    (Tonnes(212), Polypropylene))

  // Components/UNIT = generic 400 kV substation for 600MW wind farm
  val HVDCSubStation = Product(
    KilowattHours(6.78E6) + Megajoules(2.71E6),
    Transport.road * (4.2E5 + 5.26E5) + Megajoules(9.08E3 + 1.37E4 + 1.66E4),
    (Tonnes(361), Copper),
    (Tonnes(4170 + 1440), Steel),
    (Tonnes(58.7), Insulation),
    (Tonnes(590) * 0.78, LubricatingOil),
    //TODO ? (Tonnes(590)*0.19, WoodPackaging),
    (Tonnes(590) * 0.027, Paint),
    (Tonnes(590) * 0.002, Resin))

  // Components/km
  val HVDCExternalCables = Product(
    KilowattHours(9.37E4) + Megajoules(6.43E4),
    Transport.road * 2.38E5 + Megajoules(7.79E4),
    (Tonnes(14.1), Copper),
    (Tonnes(35.2), Steel),
    // TODO (SquareMeters(565),Zinc coating of steel),
    (Tonnes(24.9), Lead),
    (Tonnes(3.25), Polypropylene),
    (Tonnes(5.96), Insulation))

  // O&M estimated at 1.2E6 MJ
  def embodiedEnergyOffshoreTransmission(power: Power, distanceToShore: Length) = {
    (internalOffshoreCables.embodiedEnergy + Megajoules(1.2E6)) * power.toMegawatts / 600.0 + (
      //  HVAC
      if (distanceToShore.toKilometers <= 80) {
        (HVACExternalCables.embodiedEnergy / 30.0 * distanceToShore.toKilometers + HVACSubStation.embodiedEnergy) * power.toMegawatts / 390.0
      } //HVDC
      else {
        (HVDCExternalCables.embodiedEnergy * distanceToShore.toKilometers + HVDCSubStation.embodiedEnergy) * power.toMegawatts / 600.0
      })
  }

  def weightOffshore(material: Material, power: Power, distanceToShore: Length) = {
    (internalOffshoreCables.weight(material)) * power.toMegawatts / 600.0 + (
      //  HVAC
      if (distanceToShore.toKilometers <= 80) {
        (HVACExternalCables.weight(material) / 30.0 * distanceToShore.toKilometers + HVACSubStation.weight(material)) * power.toMegawatts / 390.0
      } //HVDC
      else {
        (HVDCExternalCables.weight(material) * distanceToShore.toKilometers + HVDCSubStation.weight(material)) * power.toMegawatts / 600.0
      })
  }

  /**
   * Assessing the Life Cycle Environmental Impacts of Offshore Wind Power Generation and Power Transmission in the North Sea
   *
   * Chirstine Brikeland
   *
   * --> The energy input for PRODUCTING are not included !
   */
  // TOTAL for wind farm of 390 MW, 63.3 km of cables
  val internalOffshoreCables2 = Product(
    Joules(0), //TODO : energy embodied in cable production process !!
    Megajoules(476663) + Megajoules(124799) + Megajoules(476663) + Transport.road * (182660 + 182660),
    (Kilograms(495142), Lead),
    (Kilograms(387852), Copper),
    (Kilograms(115153), Polyethylene),
    (Kilograms(115153), Steel), // Galvanized
    (Kilograms(115153), Polypropylene))

  // TOTAL for 30 km of cables for wind farm 390 MW
  val HVACExternalCables = Product(
    Joules(0), //TODO : energy embodied in cable production process !!
    Transport.road * (205319 + 205319) + Megajoules(476663) + Megajoules(124799) + Megajoules(124799),
    (Tonnes(22), Lead),
    (Tonnes(28), Copper),
    (Tonnes(7), Polyethylene),
    (Tonnes(28), Steel),
    (Tonnes(4), Polypropylene))

  // TOTAL for 730 km of cables with capacity of 2x700 MW
  val HVDCExternalCables2 = Product(
    Joules(0),
    // Installation + O&M + EOL
    Megajoules(476663) + Megajoules(124799) + Megajoules(476663) + Transport.road * (11245465 + 11245465),
    (Tonnes(2 * 22.86), Lead),
    (Tonnes(2 * 13.025), Copper),
    (Tonnes(2 * 5.32), Insulation),
    (Tonnes(2 * 32.89), Steel),
    (Tonnes(2 * 2.93), Polypropylene))

  // Per Unit = 244 MVA transformer ~ 195 MW
  val HVACSubStation = Product(
    Megajoules(996765) + KilowattHours(65046),
    Joules(0),
    (Tonnes(26.743), LubricatingOil),
    (Tonnes(62.811), Steel),
    (Tonnes(13.498), Copper),
    (Tonnes(1.107), Aluminium),
    (Kilograms(825), Insulation),
    (Kilograms(618), Fiberglass),
    (Kilograms(104), EpoxyResin))

  // --- ONSHORE ---
  // Scale this with the size of the wind farm, here it is given for 300 MW
  val refPower = Megawatts(300)

  // 500 MVa transformer station -> ABB Power transformer TrafoStar 500MVA
  val transformerStation = Product(
    KilowattHours(750000 + 177206000 + 300000),
    Joules(0),
    (Kilograms(153258), Steel),
    (Kilograms(39960), Copper),
    (Kilograms(63000), LubricatingOil),
    (Kilograms(6500), Insulation),
    //(Kilograms(15000), Wood),
    //(Kilograms(2650),Porcelain),
    (Kilograms(2200), Paint))

  // 95 km of 32 kV cable
  val internalOnshoreCables = Product(
    Joules(0),
    Joules(0),
    (Tonnes(63.4), Aluminium),
    (Tonnes(55.2), Plastic),
    (Tonnes(30.9), Copper))
  // 50 km of 150 kV cable
  val externalOnshoreCables = Product(
    Joules(0),
    Joules(0),
    (Tonnes(1519), Plastic),
    (Tonnes(953), Aluminium),
    (Tonnes(238.6), Copper))

  def embodiedEnergyOnshoreTransmission(power: Power) = {
    (transformerStation.embodiedEnergy) * power.toMegawatts / 500.0 + (internalOnshoreCables.embodiedEnergy + externalOnshoreCables.embodiedEnergy) * power.toMegawatts / 390.0
  }
  def weightOnshore(material: Material, power: Power) = {
    transformerStation.weight(material) * power.toMegawatts / 500.0 +
      (internalOnshoreCables.weight(material) + externalOnshoreCables.weight(material)) * power.toMegawatts / 390.0
  }
}

