
import gridData._
import squants.space._
import scala.collection.mutable.ListBuffer
import squants.motion._
import squants.energy._
import utils.Terawatts
import utils.Helper
import utils.TerawattHours
import utils.PlotHelper
import squants.time.Hours

object WindGrowth {

  val onshoreAnnualGrowth = Gigawatts(254) / 15
  val offshoreAnnualGrowth = Gigawatts(66) / 15

  val world = new WorldGrid("europe.txt", Degrees(0.25))
  val suitableOnshoreGrids = world.onshoreGrids.filter(WindPotential.suitabilityFactor(_) > 0).sortBy(g => -WindPotential.capacityFactor(g))
  val suitableOffshoreGrids = world.offshoreGrids.filter(WindPotential.suitabilityFactor(_) > 0).sortBy(g => -WindPotential.capacityFactor(g))

  val onshoreCellIterator: Iterator[GridCell] = suitableOnshoreGrids.toIterator
  val onshoreCellsWithTurbine: ListBuffer[(GridCell, Double)] = ListBuffer()
  val offshoreCellIterator: Iterator[GridCell] = suitableOffshoreGrids.toIterator
  val offshoreCellsWithTurbine: ListBuffer[(GridCell, Double)] = ListBuffer()

  def simulate(annualGrowth: Power, cellIterator: Iterator[GridCell]) {
    var currentYear = 2015
    var newProduction = List[Double]()
    while (currentYear < 2030) {
      currentYear += 1
      var addedPower = Watts(0)
      var addedProduction = WattHours(0)
      while (cellIterator.hasNext && Math.abs(addedPower.toGigawatts - annualGrowth.toGigawatts) > 0.01) {
        var next = cellIterator.next
        val pow = List(WindPotential.powerInstalled(next), annualGrowth - addedPower).min
        addedPower = addedPower + pow
        addedProduction = addedProduction + pow * WindPotential.capacityFactor(next) * Hours(365 * 24)
      }
      newProduction = newProduction :+ addedProduction.to(TerawattHours)

      println(currentYear + "\t" + addedPower.toGigawatts + "\t" + addedProduction.to(TerawattHours))
    }
    val res = newProduction.toList
    PlotHelper.plotXY((2016 to 2030).map(_.toDouble).toList, newProduction.toList)

  }
  def main(args: Array[String]): Unit = {
    simulate(onshoreAnnualGrowth, onshoreCellIterator)
    simulate(offshoreAnnualGrowth, offshoreCellIterator)

  }

  val F_acc = Newtons(1.1918E14) / world.totalArea
  val z_0 = Millimeters(1)
  val C_DN = 0.0013
  val v_hub_0 = MetersPerSecond(world.grids.map(g => g.windSpeedHub.toMetersPerSecond * g.area.toSquareKilometers).sum / world.totalArea.toSquareKilometers)

  def C_ex = 0.0013

  def v_hub = F_acc

}

object IEWA2030 {
  def main(args: Array[String]): Unit = {

    val wind = new WorldGrid("europe.txt", Degrees(0.25))
wind.writeTest()
    val target = Helper.getLines("iewa_2030.txt").map(l => (Country(l(0)), Megawatts(l(1).toDouble)))

    for (i <- target) {
      val gr = wind.grids.filter(_.country.equals(i._1)).filter(WindPotential.suitabilityFactor(_) > 0).sortBy(g => -WindPotential.capacityFactor(g))
      val cellIterator = gr.toIterator
      var addedPower = Watts(0)
      var addedProduction = WattHours(0)
      while (cellIterator.hasNext && Math.abs(addedPower.toMegawatts - i._2.toMegawatts) > 0.01) {
        var next = cellIterator.next
        val pow = List(WindPotential.powerInstalled(next), i._2 - addedPower).min
        addedPower = addedPower + pow
        addedProduction = addedProduction + pow * WindPotential.capacityFactor(next) * Hours(365 * 24)
      }

      println(i._1.name + "\t" + wind.area(gr) + "\t" + i._2.toMegawatts + "\t"+ addedPower.toMegawatts + "\t" + addedProduction.to(GigawattHours))

    }
  }

}