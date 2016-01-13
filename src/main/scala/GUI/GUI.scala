package GUI

import scala.swing._
import scala.swing.TabbedPane._
import swing.event._
import embodiedEnergy.Materials
import javax.swing.JComboBox
import energyGeneration.GridData
import utils.PlotHelper
import squants.space.Degrees
import squants.space.SquareKilometers
import squants.energy.Kilowatts
import squants.energy.Megawatts
import calculation.WindTurbine

object GUI extends SimpleSwingApplication {

  // Simulation tab 
  lazy val mainUI = new BoxPanel(Orientation.Vertical) {
    val area = new ComboBox(List("Europe", "World"))
    val sizeTurbine = List(Kilowatts(850), Megawatts(2), Megawatts(3))
    val turbine = new ComboBox(List("850 kW", "2 MW", "3 MW"))

    val start = new Button {
      text = "Go !"
    }

    listenTo(start)
    reactions += {
      case ButtonClicked(start) =>
        val wind = new GridData("results/" + area.selection.item.toLowerCase() + "5yearslc", Degrees(0.25), WindTurbine(sizeTurbine(turbine.selection.index)), WindTurbine(sizeTurbine(turbine.selection.index)))
        wind.plotEROIVSCumulatedProduction(wind.agriculturalAreas(wind.clcGrids))
        PlotHelper.cumulativeDensity(List((wind.erois(wind.agriculturalAreas(wind.clcGrids)), "")), xLabel = "% of Sites", yLabel = "EROI")

        println(area.selection.item + "\t" + turbine.selection.item)
    }

    contents += new FlowPanel(new Label("Area Studied"), area); contents += new FlowPanel(new Label("Wind Turbine Rated Power"), turbine); contents += start;
  }

  def top = new MainFrame {
    title = "Wind power potential"
    preferredSize = new Dimension(500, 350)
    contents = new TabbedPane {
      pages += new Page("Simulation", mainUI)
      pages += new Page("Materials", new MaterialsPage)
    }
  }

}