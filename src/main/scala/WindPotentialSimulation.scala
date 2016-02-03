

import construction.Materials
import landCover.GridData
import squants.space._
import operation.WindTurbine2MW
import squants.space.SquareKilometers
import operation.WindTurbineWithPower
import utils.PlotHelper
import squants.energy.MegawattHours
import squants.energy.KilowattHours
import construction.WindTurbineComponents
import construction.WindTurbineComponents
import operation.WindTurbineWithPower
import squants.space.Kilometers
import construction.Transmission
import utils.Helper
import java.io.File
import java.io.PrintStream
import java.io.FileOutputStream

object WindPotentialSimulation {
  def main(args: Array[String]): Unit = {
  plotCRUWind()
    plotEuropeWind()
    
    
    val cru = new GridData("results/CRU10min", Arcminutes(10), new WindTurbineWithPower("2MW"), new WindTurbineWithPower("5MWoffshore"), details = true)
   
    val ecmwf = new GridData("results/europe2002", Degrees(0.125), new WindTurbineWithPower("2MW"), new WindTurbineWithPower("5MWoffshore"))
    val list = List((cru.windSpeeds(cru.landGrids(cru.clcGrids), atHub = true), "CRU"), (ecmwf.windSpeeds(ecmwf.landGrids(ecmwf.clcGrids), atHub = true), "ecmwf"))
    PlotHelper.cumulativeDensity(list)
PlotHelper.histogram(list(0)._1,title="CRU Data")
PlotHelper.histogram(list(1)._1, title="EMCWF Data")
    println("--- Hello ---")
  }

  def plot(wind: GridData) {
    PlotHelper.cumulativeDensity(List((wind.windSpeeds(), "")), title = wind.name)
    PlotHelper.cumulativeDensity(List((wind.powerDensities(), "")), title = wind.name)
  }
  def plotEuropeWind() {
    val l = List("europe2002", "europeNovember2001", "europeFebruary2002", "europeMay2002", "europeAugust2002")
    val speed = l.map({ w =>
      val wind = new GridData("results/" + w, Degrees(0.125), new WindTurbineWithPower("2MW"), new WindTurbineWithPower("5MWoffshore"))
      (wind.windSpeeds(wind.landGrids(wind.clcGrids)), w)
    })
    PlotHelper.cumulativeDensity(speed)
  }

  def plotCRUWind() {
    val month = (0 until 12).toList
    val w = new GridData("results/CRU10min", Arcminutes(10), new WindTurbineWithPower("2MW"), new WindTurbineWithPower("5MWoffshore"), details = true)
      
    val speeds = month.map(i => (w.windSpeedsMonth(i, w.landGrids(w.clcGrids)),i.toString))
   
    PlotHelper.cumulativeDensity(speeds)
    //   PlotHelper.cumulativeDensity(densities)

  }
}