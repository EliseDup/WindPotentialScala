package GUI

import scala.swing._
import squants.energy._

class SimulationTab(sizeTurbines: List[Power]) extends BoxPanel(Orientation.Vertical) {

  val area = new ComboBox(List("Europe", "World"))
  val onshore = new ComboBox(sizeTurbines.map(_.toString))
  val offshore = new ComboBox(sizeTurbines.map(_.toString))

  contents += new FlowPanel(new Label("Area Studied"), area);
  contents += new FlowPanel(new Label("Onshore Wind Turbine Rated Power"), onshore);
  contents += new FlowPanel(new Label("Offshore Wind Turbine Rated Power"), offshore);

}