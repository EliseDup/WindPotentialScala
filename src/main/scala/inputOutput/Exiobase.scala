import scala.io.Source
import org.apache.commons.math3.linear.MatrixUtils

object Exiobase {
  
  def main(args: Array[String]): Unit = {
    val txt = "/Users/Elise/Documents/workspace/ressources/input_output/mrIOT_IxI_fpa_coefficient_version2.2.2/"
    val lines = Source.fromFile(txt + "mrIot_version2.2.2.txt").getLines().toList
    val nRows = lines.size
    val nCols = lines(0).split("\t").size
    /// 2 header rows, 3 header columns
    val list = (for(r <- 2 until nRows) yield {
      val v = lines(r).split("\t")
      (3 until nCols).map(c => v(c).toDouble)
    })
    val array = Array.tabulate(nRows,nCols) ((i,j) => list(i)(j))
    
    val L = MatrixUtils.createRealMatrix(array)
    println("Hello")
  }
  
}