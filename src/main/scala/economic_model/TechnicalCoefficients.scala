package economic_model

import utils._

object TechnicalCoefficients {
  import PlotHelper._
  import Helper._
  def main(args: Array[String]): Unit = {
  
    val e_pib = getLines("/Users/Elise/Model_Doctorat/Year_ECons_PIB", "\t").map(i => (i(0).toInt, TonOilEquivalent(i(2).toDouble*1E6), i(1).toDouble))
    // plotXY(List((e_pib.map(_._2.to(TonOilEquivalent)/1E6), e_pib.map(_._3), "")), xLabel = "Primary Energy Consumption [Mtoe]", yLabel="PIB [US $ 2010]")
    plotXY(List((e_pib.map(_._1.toDouble), (0 until e_pib.size).toList.map(i => e_pib(i)._2.to(TonOilEquivalent)/(e_pib(i)._3/1000)),"")), xLabel ="Year", yLabel="[toe/1000 US $ 2010]") // , title = "Energy Intensity [Toe/ 1000 US $ 2010]")
    
    val tpes_tfc_pib = getLines("/Users/Elise/Model_Doctorat/Year_TPES_TFC_PIB", "\t").map(i => (i(0).toInt, TerawattHours(i(1).toDouble), TerawattHours(i(2).toDouble), i(3).toDouble))
    
    plotXY(List((tpes_tfc_pib.map(_._1.toDouble), (0 until tpes_tfc_pib.size).toList.map(i => tpes_tfc_pib(i)._3/tpes_tfc_pib(i)._2),"")), xLabel ="Year", yLabel="TFC / TPES", title = "tfc_tpes")
    plotXY(List((tpes_tfc_pib.map(_._1.toDouble), (0 until tpes_tfc_pib.size).toList.map(i => tpes_tfc_pib(i)._3.toKilowattHours/tpes_tfc_pib(i)._4),"")), xLabel ="Year", yLabel="TFC [kWh]/ US $ 2010",title="tfc_pib")
    plotXY(List((tpes_tfc_pib.map(_._1.toDouble), (0 until tpes_tfc_pib.size).toList.map(i => tpes_tfc_pib(i)._2.toKilowattHours/tpes_tfc_pib(i)._4),"")), xLabel ="Year", yLabel="TPES [kWh]/ US $ 2010",title="tpes_pib")
 
  }
  
}