
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
import utils.Exajoules
import windEnergy.CapacityFactorCalculation
import squants.radio.WattsPerSquareMeter
import windEnergy.GustavsonWakeEffect

object WindGrowth {

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
        addedProduction = addedProduction + pow * CapacityFactorCalculation(next) * Hours(365 * 24)
      }
      newProduction = newProduction :+ addedProduction.to(Exajoules)

      println(currentYear + "\t" + addedPower.toGigawatts + "\t" + addedProduction.to(Exajoules))
    }
    val res = newProduction.toList
    PlotHelper.plotXY((2016 to 2030).map(_.toDouble).toList, newProduction.toList)

  }
  def simulateConstantGrowth = {
    val onshoreAnnualGrowth = Gigawatts(254) / 15
    val offshoreAnnualGrowth = Gigawatts(66) / 15

    val europe = new WorldGrid("results/europeGridWind.txt", Degrees(0.25))

    val suitableOnshoreGrids = europe.onshoreGrids.filter(WindPotential.suitabilityFactor(_) > 0).sortBy(g => -CapacityFactorCalculation(g))
    val suitableOffshoreGrids = europe.offshoreGrids.filter(WindPotential.suitabilityFactor(_) > 0).sortBy(g => -CapacityFactorCalculation(g))

    val onshoreCellIterator: Iterator[GridCell] = suitableOnshoreGrids.toIterator
    val onshoreCellsWithTurbine: ListBuffer[(GridCell, Double)] = ListBuffer()
    val offshoreCellIterator: Iterator[GridCell] = suitableOffshoreGrids.toIterator
    val offshoreCellsWithTurbine: ListBuffer[(GridCell, Double)] = ListBuffer()

    simulate(onshoreAnnualGrowth, onshoreCellIterator)
    //  simulate(offshoreAnnualGrowth, offshoreCellIterator)
  }
  def simulate(annualGrowth: List[(Int, Power)], cellIterator: Iterator[GridCell]) {
    var newProduction = List[Double]()
    annualGrowth.map(g => {
      val currentYear = g._1
      val currentGrowth = g._2
      var addedPower = Watts(0)
      var addedProduction = WattHours(0)
      while (cellIterator.hasNext && Math.abs(addedPower.toMegawatts - currentGrowth.toMegawatts) > 0.01) {
        var next = cellIterator.next
        val pow = List(WindPotential.powerInstalled(next), currentGrowth - addedPower).min
        addedPower = addedPower + pow
        addedProduction = addedProduction + pow * CapacityFactorCalculation(next) * Hours(365 * 24)
      }
      newProduction = newProduction :+ addedProduction.to(Exajoules)

      println(currentYear + "\t" + addedPower.toGigawatts + "\t" + addedProduction.to(Exajoules))
    })
    val res = newProduction.toList
    if (annualGrowth.size > 1) PlotHelper.plotXY(annualGrowth.map(_._1.toDouble).toList, newProduction.toList)

  }
  def main(args: Array[String]): Unit = {
    val world = new WorldGrid("results/worldGridWind.txt", Degrees(0.5))
    val growth = Helper.getLines("windGrowth.txt").map(l => (l(0).toInt, Megawatts(l(2).toInt)))
    val cellIterator: Iterator[GridCell] = world.onshoreConstrainedGrids(WindPotential).sortBy(g => -CapacityFactorCalculation(g)).toIterator

    simulate(List((2015, Megawatts(432883))), cellIterator)

  }

  def maxProduction {
    val world = new WorldGrid("results/worldGridWind.txt", Degrees(0.5))

    val F_acc = Newtons(1.1918E14) / world.totalArea
    val z_0 = Millimeters(1)
    val C_DN = 0.0013
    val v_hub_0 = MetersPerSecond(world.grids.map(g => g.windSpeed.toMetersPerSecond * g.area.toSquareKilometers).sum / world.totalArea.toSquareKilometers)

    def C_ex = 0.0013

    def v_hub = F_acc
  }

}

object IEWA2030 {
  def main(args: Array[String]): Unit = {

    //  val wind = new WorldGrid("results/europeGridWind.txt", Degrees(0.25))

    val cells = OptimalCapacityDensity.loadCells( (0 to 40).map(_*0.5).toList, "../WindPotentialPy/res_optimalCD_europe")
    val target = Helper.getLines("results/iewa_2030.txt").map(l => (Country(l(0)), Megawatts(l(1).toDouble), GigawattHours(l(1 + 3).toDouble)))
    val minEROI = 7
    for (i <- target) {

      val gr = cells.filter(_.country.contains(i._1.name)).filter(_.suitableArea.toSquareKilometers > 0).filter(_.getOptimalCD(minEROI).toWattsPerSquareMeter > 0).sortBy(-_.cf)
      val cellIterator = gr.toIterator

      var addedPower = Watts(0)
      var addedProduction = WattHours(0)

      while (cellIterator.hasNext && Math.abs(addedPower.toMegawatts - i._2.toMegawatts) > 0.01) {
        var next = cellIterator.next
        val pow = List(next.powerInstalled(true, minEROI), i._2 - addedPower).min
        addedPower = addedPower + pow
        addedProduction = addedProduction + pow * next.cf * GustavsonWakeEffect.wakeEffect(pow / Megawatts(2), next.lambda(next.getOptimalCD(minEROI))) * Hours(365 * 24)
      }

      var extraCapacityNeeded = Watts(0)
      var extraProduction = WattHours(0)
      while (cellIterator.hasNext && (extraProduction + addedProduction) < i._3) {
        var next = cellIterator.next
        val prod = List(next.annualProduction(true, minEROI), i._3 - (extraProduction + addedProduction)).min
        extraProduction = extraProduction + prod
        extraCapacityNeeded = extraCapacityNeeded + (prod / (next.cf * GustavsonWakeEffect.wakeEffect(200, next.lambda(next.getOptimalCD(minEROI))) * Hours(365 * 24)))

      }

      println(i._1.name + "\t" + gr.map(_.suitableArea).foldLeft(SquareKilometers(0))(_ + _).toSquareKilometers + "\t" + i._2.toMegawatts + "\t" + i._3.toGigawattHours + "\t" + addedPower.toMegawatts + "\t" + addedProduction.to(GigawattHours) + "\t" + extraCapacityNeeded.toMegawatts + "\t" + extraProduction.toGigawattHours)

    }
  }

}