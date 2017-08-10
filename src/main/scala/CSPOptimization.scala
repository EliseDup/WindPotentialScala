

import squants.energy._
import squants.space._
import utils.PlotHelper
import squants.radio._
import squants.time.Megahertz

object CSPOptimization {
  def main(args: Array[String]): Unit = {
    println(ced(Megawatts(49), 20, 2.84, WattsPerSquareMeter(206)).to(Megajoules))
    
    PlotHelper.plotXY((1 to 2000).map(_.toDouble).toList, (1 to 2000).map(_.toDouble).toList.map(d => atmosphericAttenuation(Meters(d))))
  }

  val refPower = Megawatts(110)
  val mirrorArea = SquareMeters(13 * 11)
  val heliostatReflectivity = 0.88

  def apertureArea(power: Power, dni: Irradiance) = power / dni * 0.74

  def atmosphericAttenuation(d: Length) = {
    if (d.toMeters <= 1000) 0.99321 - 0.0001176 * d.toMeters + 1.97E-8 * d.toMeters * d.toMeters
    else Math.exp(-0.0001106 * d.toMeters)
  }

  def ced(designedPower: Power, storage: Double, solarMutliplicator: Double, dni: Irradiance) = {
    val scale = designedPower / refPower
    val nMirrors = apertureArea(designedPower, dni) / mirrorArea

    val em = Megajoules(202202) * nMirrors + Megajoules(347429433) * (0.5 + 0.5 * scale) + (Megajoules(22518574) * storage + Megajoules(57904905) + Megajoules(38603270)) * scale
    val c = Megajoules(204370230) * scale
    val om = Megajoules(3292631910.0) * scale
    val d = Megajoules(138517590) * scale

    em+c+em+d
  }
}