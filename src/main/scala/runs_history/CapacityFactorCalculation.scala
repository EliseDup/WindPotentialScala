package runs_history

import grid.WorldGrid
import squants.space.Degrees
import wind_energy.WindPotential
import squants.radio.WattsPerSquareMeter
import utils.TerawattHours
import squants.space.SquareKilometers
import utils.Terawatts
import grid.GridCell
import squants.energy.Gigawatts
import squants.radio.Irradiance
import squants.time.Hours

object CapacityFactorCalculationTest {
  import utils.Helper._
  def main(args: Array[String]): Unit = {
    val wp = WindPotential()
    val countries = getLines("countries_students").map(_(0))
    val eroi_min = (2 until 40).map(_ * 0.5).toList
    val wind = new WorldGrid("runs_history/wind_2017/results_wind_2017", Degrees(0.75), eroi_min, 34, 151, true, false)
    val d = WattsPerSquareMeter(2)
    pr("Country"); pr("EROI Minimum"); pr("Onshore Production Potential [TWh/year]"); pr("Onshore Capacity Potential [GW]"); pr("Onshore Mean Installed Capacity Density [MW/km2]"); pr("Onshore Mean Capacity Factor [%]");
    pr("Offshore Production Potential [TWh/year]"); pr("Offshore Capacity Potential [GW]"); pr("Offshore Mean Installed Capacity Density [MW/km2]"); pr("Ofshore Mean Capacity Factor [%]");
    println()
    for(eroi  <- (1 until 15)){
 
    for (c <- countries) {
      print(eroi, c)
    }}
    def print(eroi: Double, c: String) {
      val cells = wind.country(c);
      val on = cells.filter(_.onshore); val off = cells.filter(_.offshore);
      val pot_on = wp.potential(eroi, true, on).to(TerawattHours)
      val cap_on = wp.installedCapacity(eroi, true, on).to(Gigawatts)
      val cd_on = wp.meanCapacityDensity(on, eroi)
      val pot_off = wp.potential(eroi, true, off).to(TerawattHours)
      val cap_off = wp.installedCapacity(eroi, true, off).to(Gigawatts)
      val cd_off = wp.meanCapacityDensity(off, eroi)

      pr(c); pr(eroi); pr(pot_on); pr(cap_on); pr(cd_on); pr(if(cap_on > 0) (pot_on / (cap_on * 8.76) * 100) else 0.0)
      pr(pot_off); pr(cap_off); pr(cd_off); pr(if(cap_off > 0) (pot_off / (cap_off * 8.76) * 100) else 0.0)
      println()

    }
    def printFixed(eroi: Double, c: String, cd: Irradiance) {
      val cells = wind.country(c);
      val on = cells.filter(_.onshore); val off = cells.filter(_.offshore);
      val pot_on = wp.potentialFixedDensity(cd, eroi, on, true)
      val cap_on = cd * (on.map(c => if (wp.eroi(c, cd, true) >= eroi) c.suitableArea(wp) else SquareKilometers(0)).foldLeft(SquareKilometers(0))(_ + _))
      val cf_on = pot_on / (cap_on * Hours(365 * 24))

      val pot_off = wp.potentialFixedDensity(cd, eroi, off, true)
      val cap_off = cd * (off.map(c => if (wp.eroi(c, cd, true) >= eroi) c.suitableArea(wp) else SquareKilometers(0)).foldLeft(SquareKilometers(0))(_ + _))
      val cf_off = pot_off / (cap_off * Hours(365 * 24))

      pr(c);
      pr(pot_on.to(TerawattHours)); pr(cap_on.to(Gigawatts)); pr(cd.toWattsPerSquareMeter); pr(cf_on * 100)
      pr(pot_off.to(TerawattHours)); pr(cap_off.to(Gigawatts)); pr(cd.toWattsPerSquareMeter); pr(cf_off * 100)
      println()

    }
  }

  def pr(c: String) = print(c + "\t")
  def pr(c: Double) = print(c + "\t")
}