

import utils.PlotHelper

object Test {
   def main(args: Array[String]): Unit = {
     val eroi = (100 to 3000).map(_*0.01).toList
     def output(eroi : Double) = 100.0 - 100.0/eroi
     
     PlotHelper.plotXY(List((eroi, eroi.map(output(_)), "")), xLabel = "EROI", yLabel ="% Total Energy")
   }
}