package GUI

import scala.swing._
import scala.swing.TabbedPane._
import swing.event._
import construction.Materials
import javax.swing.JComboBox
import utils.PlotHelper
import squants.space.Degrees
import squants.space.SquareKilometers
import squants.energy._
import landCover.GridData
import operation.WindTurbine
import construction.WindFarmComponents

object GUI extends SimpleSwingApplication {
  
  val sizeTurbine = List(Kilowatts(850), Megawatts(2), Megawatts(3))
  val farm = WindFarmComponents()
  def top = new MainFrame {
    title = "Wind power potential"
    preferredSize = new Dimension(750, 500)
    val materials = new MaterialsTab
    val simu = new SimulationTab(sizeTurbine)
    val start = new Button { text = "Go !" }

    contents = new FlowPanel {
      contents.append(new TabbedPane {
        pages += new Page("Simulation", simu)
        pages += new Page("Materials", materials)
      })
      contents.append(start)
    }
    listenTo(start)

    reactions += {
      case ButtonClicked(start) =>
        val wind = new GridData("results/" +
          simu.area.selection.item.toLowerCase() + "5yearslc", Degrees(0.25),
          WindTurbine(sizeTurbine(simu.onshore.selection.index)), WindTurbine(sizeTurbine(simu.offshore.selection.index)),farm,farm)
        
        wind.plotEROIVSCumulatedProduction(wind.agriculturalAreas(wind.clcGrids))
        PlotHelper.cumulativeDensity(List((wind.erois(wind.agriculturalAreas(wind.clcGrids)), "")), xLabel = "% of Sites", yLabel = "EROI")
    }
  }

}