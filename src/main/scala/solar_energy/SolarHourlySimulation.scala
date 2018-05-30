package solar_energy

import squants.space._
import squants.radio._
import squants.energy._
import squants.time.Hours
import utils._
import java.io.PrintStream
object SolarHourlySimulation {
  import SolarPower._
  import Helper._
  import DayMonth._
  import PlotHelper._
  def main(args: Array[String]): Unit = {
    val maxDay = 365
    val d = (1 to maxDay).toList
    val h = (1 to 24).toList

    val solar = (getLines("../resources/cru_data/cru_solarByMonth").map(i => {
      (GeoPoint(Degrees(i(0).toDouble), Degrees(i(1).toDouble)), (0 to 12).toList.map(j => WattsPerSquareMeter(i(j + 2).toDouble)))
    })).filter(i => !i._2.exists(j => j.value == 0))
    println("Data loaded" + "\t" + solar.size)
  val out_stream = new PrintStream(new java.io.FileOutputStream("test"))

    val test = solar.filter(i => math.abs(i._1.latitude.value)<=20)
    println(test.size)
    test.map(t => {
      val res = simulate(t,2.7,12)
      out_stream.print(t._1.latitude.value +"\t" + t._1.longitude.value +"\t" + 
          t._2(0).value + "\t" + directIrradiance(t._1.latitude, t._2(0)).value + "\t" + res._1 + "\t" + res._2 +"\n")
    }
  )
  out_stream.close()
  }
  def simulate(x : (GeoPoint, List[Irradiance]), sm: Double, hours_storage: Int) :(Double,Double) = simulate(x._1.latitude, x._2(0), (1 to 12).toList.map(x._2(_)), sm, hours_storage)
  def simulate(latitude: Angle, irradiance_year: Irradiance, irradiance_month: List[Irradiance], sm: Double, hours_storage: Int):(Double,Double) = {
    val maxDay = 365
    val d = (1 to maxDay).toList; val h = (1 to 24).toList;
    val dailyRad = d.map(day => dailyDirectRadiation(latitude, irradiance_month(month_dayInMonth(day)._1), day))
    val hourlyRad = d.map(day => h.map(hour => hourlyDirectRadiation(latitude, irradiance_month(month_dayInMonth(day)._1), day, hour))).flatten

    val power = Megawatts(1); val cap = power * Hours(1);
    val area = power * sm / (WattsPerSquareMeter(950) * 0.15)
    val storageSize = MegawattHours(hours_storage)
    var storage = storageSize / 2; var totalProd = MegawattHours(0)

    val prod: List[(Energy, Energy, Energy, Energy)] = hourlyRad.map(i => {
      val e = area * i * 0.15 * Hours(1)
      if (e > cap) {
        val sup = e - cap
        val e_stored = if (storageSize - storage > sup) sup else WattHours(Math.max((storageSize - storage - sup).value, 0))
        val curt = sup - e_stored
        storage += e_stored
        totalProd += cap
        (cap, e_stored, storage, -curt)
      } // Enough storage to ensure 1 MWH
      else if (storage > cap - e) {
        storage -= cap - e
        totalProd += cap
        (cap, MegawattHours(0), storage, WattHours(0))
      } else {
        val p = e + storage
        storage = MegawattHours(0)
        totalProd += p
        (p, MegawattHours(0), storage, WattHours(0))
      }
    })
    val h_year = (0 until 8760).map(_.toDouble).toList
    plotXY(List( (h_year, prod.map(_._1.toMegawattHours),"Production"),(h_year, prod.map(_._2.toMegawattHours),"Energy Stored"),
        (h_year, prod.map(_._3.toMegawattHours),"Total Storage"),(h_year, prod.map(_._4.toMegawattHours),"Energy Lost")), legend=true) 
    val simulatedCF = totalProd / (Megawatts(1) * Hours(365 * 24))
    val calculatedCFDir = area * directIrradiance(latitude, irradiance_year) * 0.15 * Hours(365 * 24) / (Megawatts(1) * Hours(365 * 24))
    val calculatedCF = area * irradiance_year * 0.15 * Hours(365 * 24) / (Megawatts(1) * Hours(365 * 24))
    
    println(latitude + "\t" + simulatedCF*100 +"\t" + calculatedCF*100 +"\t" + calculatedCFDir*100)
    (simulatedCF,calculatedCF)
        
  }

}