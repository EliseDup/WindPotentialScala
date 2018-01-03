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
import grid.WorldGrid
import wind_energy.WindTurbine
import wind_energy.WindTurbineWithPower

object GUI extends SimpleSwingApplication {
   
  val sizeTurbine = List("850 kW", "2 MW", "3 MW")

  def top = new MainFrame {
    title = "Wind power potential"
    preferredSize = new Dimension(750, 500)
    val mat = new MaterialsTab
    val simu = new SimulationTab(sizeTurbine)
    val start = new Button { text = "Go !" }

    contents = new FlowPanel {
      contents.append(new TabbedPane {
        pages += new Page("Simulation", simu)
        pages += new Page("Materials", mat)
      })
      contents.append(start)
    }
    listenTo(start)

    reactions += {
      case ButtonClicked(start) =>
        //val wind = new WorldGrid("results/" +
        //  simu.area.selection.item.toLowerCase() + "5yearslc", Degrees(0.25))
        
    }
  }

}