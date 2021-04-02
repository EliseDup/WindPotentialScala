import utils.Helper
import utils.PlotHelper
object ThesisResults {
  import Helper._
  import PlotHelper._
  
  def main(args: Array[String]): Unit = {
    println("Thesis Results")
    tpes_gdp
  }
  def tpes_gdp {
    val data = getLines("../model_data/TPES_GDP")
    val y = data.map(_(0).toDouble)
    val tpes = data.map(_(1).toDouble)
    val gdp = data.map(_(2).toDouble/1000)
    val intensity = (0 until y.size).toList.map(i => tpes(i)/gdp(i))
    plotXY(List((tpes,gdp,"")), xLabel ="Primary Energy Consumption [EJ]", yLabel = "GDP [TUS$2010]", title="tpc_gdp")
    plotXY(List((y,intensity,"")), yLabel = "Energy Intensity of GDP [MJ/US$2010]", title="energy_intensity", int_x_axis=true)
    
  }
  
}