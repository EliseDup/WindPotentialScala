package GUI

import scala.swing.Panel
import construction.Materials
import scala.swing._
import scala.swing.TabbedPane._
import swing.event._
import squants.energy.Power
import javax.swing.DefaultCellEditor
import javax.swing.JComboBox
import javax.swing.table._
import javax.swing._
import javax.swing.table.{ TableCellEditor, AbstractTableModel }
import java.awt.{ Component => AWTComponent }
import squants.mass.Mass
import squants.energy.Energy

class MaterialsTab extends BoxPanel(Orientation.Vertical) {

  val materials = Materials()
  def ModifiedMaterials : Materials = Materials()
  val model = materials.list.map(_.toArray).toArray

  val table = new Table(model, Array("Material", "Energy Intensity", "", "Unit", "")) {
    selection.elementMode = Table.ElementMode.Cell
    override def editor(row: Int, col: Int): TableCellEditor = {
      if (col == 2) new ComboBoxEditor("GJ", Energy.units.map(_.symbol).toArray)
      else if (col == 4) new ComboBoxEditor("t", Mass.units.map(_.symbol).toArray)
      else super.editor(row, col)
    }
  }
  listenTo(table.selection)
  contents += new ScrollPane(table)
}
trait AbstractCellEditorTrait extends AbstractCellEditor
trait TableCellEditorTrait extends TableCellEditor
class ComboBoxEditor(val current: String, val list: Array[String]) extends AbstractCellEditorTrait with TableCellEditorTrait {
  private val cb = new ComboBox(list)
  cb.selection.item = current
  def getCellEditorValue: AnyRef = cb.selection.item.asInstanceOf[AnyRef]
  def getTableCellEditorComponent(tab: JTable, value: AnyRef, isSelected: Boolean, row: Int, col: Int): AWTComponent = cb.peer.asInstanceOf[AWTComponent]
}