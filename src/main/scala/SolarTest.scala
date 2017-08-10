

import utils._
import squants.space._
import squants.energy._
import squants.time._
import squants.radio._
import grid._
import solar_energy.SolarPotential

object SolarTest {
  def main(args: Array[String]): Unit = {
    import SolarPotential._

    /*    val c = w.grids.filter(_.onshore)
    val b = w.country("South Africa")
    
    PlotHelper.plotXY( (0 until 12).map(_.toDouble).toList, (0 until 12).toList.map(b(0).clearnessIndex(_)))
    
    plotEROIFunction(c, 0, true)

    val meanU = Helper.listValueVSArea(c.map(i => (i.irradiance.mean.toWattsPerSquareMeter, i.suitableArea(SolarPotential))))
    val meanEROIU = Helper.listValueVSArea(c.map(i => (SolarPotential.eroi(i, 0, true), i.suitableArea(SolarPotential))))

    PlotHelper.dualAxisPlot(meanU._1, meanU._2, meanEROIU._2, "Surface Cumulée [Millions km2]", "Irradiation Annuelle [kWh/m2]", "EROI")

    println(c.map(_.suitableArea(SolarPotential)).foldLeft(SquareKilometers(0))(_ + _))

    val min = Helper.listValueVSArea(c.map(i => (i.irradiance.perMonth.map(_.toWattsPerSquareMeter).min, i.area(BareAreas))))
    val max = Helper.listValueVSArea(c.map(i => (i.irradiance.perMonth.map(_.toWattsPerSquareMeter).max, i.area(BareAreas))))
*/
    val months = (0 until 12).toList.map(_.toDouble)
    val days = (0 until 365).toList.map(_.toDouble)
    val hours = (0 until 24).toList.map(_.toDouble)

    val l = List(Degrees(-40), Degrees(-20), Degrees(0), Degrees(20), Degrees(40))
    val res = l.map(i => (days, days.map(m => Thermodynamics.dailyRadiation(m.toInt, i).toWattsPerSquareMeter*24.0/1000), i.toString))
    PlotHelper.plotXY(res, legend=true)
    
      val w = WorldGrid.simple()
  
    val lats = (0 to 66).toList.map(_ * 0.75) // (-120 to 120).toList.map(_ * 0.75)
    val latsCRU = (0 to 100).toList.map(_ * 0.5 + 0.25) // (-112 to 166).toList.map(_ * 0.5 + 0.25)
    val latsNASA = (0 to 50).toList.map(_.toDouble)

    val dataNet = SolarData.list("../resources/era_interim_data/solar/net40years")
    val data = SolarData.list("../resources/era_interim_data/solar/radiationDwn_40years")

    val cruData = SolarData.list("../resources/cru_data/cru_solar")
    val nasa = SolarData.listkWhPerDay("/Users/Elise/Desktop/nasa")

    println(dataNet.maxBy(_.value).value)
    println(data.maxBy(_.value).value)
    println(cruData.maxBy(_.value).value)
    println(nasa.maxBy(_.value).value)

    def meanRadiation(lat: Angle, d: List[SolarData]): Irradiance = {
      val a = d.filter(_.lat.toDegrees == lat.toDegrees).filter(_.value.value > 0)
      if (a.size == 0) WattsPerSquareMeter(0)
      else a.map(_.value).foldLeft(WattsPerSquareMeter(0))(_ + _) / a.size
    }

    def meanFromCells(lat: Angle, cells: List[GridCell]): Irradiance = {
      val a = cells.filter(_.center.latitude.toDegrees == lat.toDegrees).filter(_.irradiance.mean.value > 0)
      if (a.size == 0) WattsPerSquareMeter(0)
      else a.map(_.irradiance.mean).foldLeft(WattsPerSquareMeter(0))(_ + _) / a.size
    }

    PlotHelper.plotXY(List(
      (lats, lats.map(l => Thermodynamics.yearlyRadiation(Degrees(l)).toWattsPerSquareMeter), "Theoretical"),
      (lats, lats.map(l => meanRadiation(Degrees(l), data).toWattsPerSquareMeter), "ERA-Interim"),
      (lats, lats.map(l => meanRadiation(Degrees(l), dataNet).toWattsPerSquareMeter), "ERA-Interim Net"),
      (lats, lats.map(l => meanFromCells(Degrees(l), w.grids).toWattsPerSquareMeter), "Grid"),
      (latsCRU, latsCRU.map(l => meanRadiation(Degrees(l), cruData).toWattsPerSquareMeter), "CRU"),
      (latsNASA, latsNASA.map(l => meanRadiation(Degrees(l), nasa).toWattsPerSquareMeter), "NASA")),
      yLabel = "Yearly Irradiance [W/m2]", xLabel = "Latitude [Degrees N]",
      legend = true)

  }
}

class SolarData(val lat: Angle, val lon: Angle, val value: Irradiance) {
  override def toString() = "Solar Data " + value + ", lat :" + lat + ", lon :" + lon
}
object SolarData {
  def list(file: String) = Helper.getLines(file, "\t").map(i => new SolarData(Degrees(i(0).toDouble), Degrees(i(1).toDouble), WattsPerSquareMeter(i(2).toDouble)))
  def listkWhPerDay(file: String) = Helper.getLines(file, "\t").map(i => new SolarData(Degrees(i(0).toDouble), Degrees(i(1).toDouble), WattsPerSquareMeter(i(2).toDouble * 1000.0 / 24.0)))
}
class SolarDataByMonth(val lat: Angle, val lon: Angle, val byMonth: List[Irradiance])
object SolarDataByMonth {
  def list(file: String) = Helper.getLines(file, "\t").map(i => SolarDataByMonth(i))
  def apply(line: Array[String]) = {
    new SolarDataByMonth(Degrees(line(0).toDouble), Degrees(line(1).toDouble), (0 until 12).map(i => WattsPerSquareMeter(line(i + 2).toDouble)).toList)
  }
}