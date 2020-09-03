package economic_model

import wind_energy._
import solar_energy._
import utils._
import squants.energy._
import squants.time.Hours
import java.io.FileOutputStream
/*
 * Read IOTs from OCDE stats and calculate some econometric data
 */
object TES {
  import Helper._
  import PlotHelper._
  import GrowthModel._

  def main(args: Array[String]): Unit = {
   /*for (i <- 1995 to 2011) {
      val tes = new TES(i, true)
      println(i + "\t" + tes.eta_tot+ "\t" + tes.alpha_tot  + "\t" + tes.zf_tot)
    }*/
    for (i <- 2005 to 2015) {
      val tes = new TES(i, false)
      println(i + "\t" + tes.eta_tot+ "\t" + tes.alpha_tot +"\t" + tes.zf_tot + "\t" +tes.z_e +"\t" + tes.va_tot  )
    }
    
    val tes = new TES(2015, false)
    List("USA", "JPN", "DEU", "FRA", "ITA", "GBR", "CHN", "CAN", "RUS").map(c => println(c + "\t" + tes.alpha(List(c)) + "\t" + tes.eta(List(c)) + "\t" + tes.zf(List(c))))

    println(tes.transactions(tes.se_row_indexes, tes.se_col_indexes) + "\t" + tes.transactions(tes.sf_row_indexes, tes.se_col_indexes) + "\t" + tes.consumption(tes.se_row_indexes))
    println(tes.transactions(tes.sf_row_indexes, tes.se_col_indexes) + "\t" + tes.transactions(tes.sf_row_indexes, tes.sf_col_indexes) + "\t" + tes.consumption(tes.sf_row_indexes))

    println(tes.value_added(tes.se_col_indexes) + "\t" + tes.value_added(tes.sf_col_indexes) + "\t" + tes.value_added(tes.hfce_col_indexes))
    println(tes.output(tes.se_col_indexes) + "\t" + tes.output(tes.sf_col_indexes) + "\t" + tes.output(tes.hfce_col_indexes))

  }

  def round(d: Double) = math.round(d * 100) / 100.0

}
class TES(year: Int, v3: Boolean) {
  import Helper._

  def file_name(year: Int, v3: Boolean) = {
    val extension = if (v3) "2016_" else "2018_"
    "/Users/Elise/Desktop/IOTs_OECD/ICIO" + extension + year.toString + ".CSV"
  }
  val tes = getLines(file_name(year, v3), ",")
  val col_index = tes(0).toList.zipWithIndex
  val row_index = tes.map(i => i(0)).toList.zipWithIndex

  def getCol(st: String): List[Int] = col_index.filter(_._1.contains(st)).map(_._2)
  def getRow(st: String): List[Int] = row_index.filter(_._1.contains(st)).map(_._2)
  def getCols(st: List[String]): List[Int] = st.map(s => getCol(s)).flatten
  def getRows(st: List[String]): List[Int] = st.map(s => getRow(s)).flatten
  // Energy sector = sum of 3 sectors
  // V4 :
  // D05T06	Mining and extraction of energy producing products
  // D19	Coke and refined petroleum products
  // D35T39	Electricity, gas, water supply, sewerage, waste and remediation services
  /*
     *  V3 : 
     *  C10T14MIN : Mining and quarrying
     *  C23PET	Coke, refined petroleum products and nuclear fuel
     *  C40T41EGW	Electricity, gas and water supply
     */
  val list_se = if (v3) List("C10T14MIN", "C23PET", "C40T41EGW") else List("_05T06", "_19", "_35T39")
  def is_se(st: String): Boolean = {
    if (v3) (st.contains("C10T14MIN") || st.contains("C23PET") || st.contains("C40T41EGW"))
    else (st.contains("_05T06") || st.contains("_19") || st.contains("_35T39"))
  }
  /*  *
     *  HFCE	Households final consumption expenditure
     *  NPISH	Non-profit institutions serving households
     *  GGFC	General government final consumption
     *  GFCF	Gross fixed capital formation
     *  INVNT	Change in inventories and valuables
     *  P33	Direct purchases abroad
     *  V3 : DISC and DIRP
     *  */
  val fds_list = List("HFCE", "NPISH", "GGFC", "GFCF", "INVNT", "P33", "DISC", "DIRP", "INV", "NPS")
  val total_col = "TOTAL" // Only in V4
  val value_row = if (v3) "VA+TAXSUB" else "VALU"
  val output_row = if (v3) "OUT" else "OUTPUT"
  val cell_0 = col_index(0)._1

  def is_fd(st: String) = {
    var res = false
    for (i <- fds_list) {
      if (st.contains(i))
        res = true
    }
    res
  }
  def is_ci_col(st: String) = !st.equals(col_index(0)._1) && !st.contains(total_col) && !is_fd(st)
  def is_ci_row(st: String) = !is_tax(st) && !st.equals(cell_0) && !st.contains(value_row) && !st.contains(output_row)
  def is_tax(st: String) = if (v3) false else st.contains("TAXSUB")
  def col_indexes(st: String) = col_index.filter(_._1.contains(st)).map(_._2)
  val se_col_indexes = col_index.filter(i => is_se(i._1)).map(_._2)
  val se_row_indexes = row_index.filter(i => is_se(i._1)).map(_._2)
  val ci_col_indexes = col_index.filter(i => is_ci_col(i._1)).map(_._2)
  val ci_row_indexes = row_index.filter(i => is_ci_row(i._1)).map(_._2)
  val sf_col_indexes = ci_col_indexes.filter(i => !is_se(col_index(i)._1))
  val sf_row_indexes = ci_row_indexes.filter(i => !is_se(row_index(i)._1))
  
  // Final consumption = HFCE	Households final consumption expenditure
  // NPISH	Non-profit institutions serving households
  // GGFC	General government final consumption
  val hfce_col_indexes = col_index.filter(i => (i._1.contains("HFCE") || i._1.contains("NPISH") || i._1.contains("GGFC") || i._1.contains("NPS"))).map(_._2)
  val investment_col_indexes = col_index.filter(i => (i._1.contains("GFCF") || i._1.contains("INVNT"))).map(_._2)
  val tax_row_indexes = row_index.filter(i => is_tax(i._1)).map(_._2)
  val va_row_index = row_index.find(_._1.contains(value_row)).get._2
  val output_row_index = row_index.find(_._1.contains(output_row)).get._2
  if (v3) {
    assert(se_col_indexes.size == 213)
  } else {
    assert(se_col_indexes.size == 207)
  }
  // println(se_col_indexes.size + "\t" + se_row_indexes.size + "\t" + ci_col_indexes.size + "\t" + hfce_col_indexes.size + "\t" + va_row_index + "\t" + output_row_index)
  // Remove TAXES !!
  def taxes(col_indexes: List[Int]) = {
    tax_row_indexes.map(i => col_indexes.map(j => tes(i)(j).toDouble).sum).sum
  }
  def value_added(col_indexes: List[Int]) = {
    col_indexes.map(i => tes(va_row_index)(i).toDouble).sum
  }
  def output(col_indexes: List[Int]) = {
    col_indexes.map(i => tes(output_row_index)(i).toDouble).sum - taxes(col_indexes)
  }
  def intermediate_cons(col_indexes: List[Int]) = {
    output(col_indexes) - value_added(col_indexes)
  }

  def va_go_ci(col_indexes: List[Int]) = {
    val va = value_added(col_indexes); val go = output(col_indexes);
    val ci = intermediate_cons(col_indexes)
    // In the files, (go-tax) - va corresponds to the sum of the intermediate consumptions (ie all rows below tax)
    (va, go, ci)
  }

  def va_go_ci_sector(sector: String) = {
    val col = col_indexes(sector)
    va_go_ci(col)
  }
  def consumption(row_indexes: List[Int]) = {
    row_indexes.map(i => hfce_col_indexes.map(j => tes(i)(j).toDouble).sum).sum
  }
  def transactions(from: List[Int], to: List[Int]) = {
    from.map(i => to.map(j => tes(i)(j).toDouble).sum).sum
  }
  val (va_se, go_se, ci_se) = va_go_ci(se_col_indexes)
  val (va_tot, go_tot, ci_tot) = va_go_ci(ci_col_indexes)
  val alpha_tot = va_se / va_tot
  val c_tot = consumption(ci_row_indexes); val c_e_tot = consumption(se_row_indexes);
  val eta_tot = c_e_tot / c_tot
  val z_tot = ci_tot / go_tot
  val z_e = ci_se / go_se
  val zf_tot = (ci_tot - ci_se) / (go_tot - go_se)

  // Country by country
  def countriesCIIndex(countries: List[String]) = countries.map(c => countryCIIndex(c)).flatten
  def countryCIIndex(country: String) = ci_col_indexes.filter(i => col_index(i)._1.contains(country))
  def countriesSeIndex(countries: List[String]) = countries.map(c => countrySeIndex(c)).flatten
  def countrySeIndex(country: String) = se_col_indexes.filter(i => col_index(i)._1.contains(country))

  def alpha(country: List[String]) = {
    val va_se = value_added(countriesSeIndex(country))
    val va_tot = value_added(countriesCIIndex(country))
    // println(country + "\t" + va_se + "\t" + va_tot)
    va_se / va_tot
  }

  def eta(country: List[String]) = {
    val country_hfce = country.map(c => hfce_col_indexes.filter(i => col_index(i)._1.contains(c))).flatten
    val c_tot = ci_row_indexes.map(i => country_hfce.map(j => tes(i)(j).toDouble).sum).sum
    val c_se = se_row_indexes.map(i => country_hfce.map(j => tes(i)(j).toDouble).sum).sum
    // val tax_c = tax_row_indexes.map(i => country_hfce.map(j => tes(i)(j).toDouble).sum).sum
    // println(country + "\t" + c_tot + "\t" + c_se + "\t" +tax_c)
    c_se / c_tot
  }

  def zf(country: List[String]) = {
    val se_indexes = countriesSeIndex(country); val ci_indexes = countriesCIIndex(country)
    if (se_indexes.size == 0) println("---" + country + "---")
    val output_se = output(se_indexes)
    val output_tot = output(ci_indexes)
    val va_se = value_added(se_indexes)
    val va_tot = value_added(ci_indexes)
    val ci_e = intermediate_cons(se_indexes)
    val ci_tot = intermediate_cons(ci_indexes)
    val ci_f = ci_tot - ci_e
    val output_f = output_tot - output_se
    // println(country + "\t" + ci_tot + "\t" +ci_f + "\t" + output_tot+ "\t" + output_f)
    ci_f / output_f
  }

  def ci_from_to(from: List[String], to: List[String]) = {
    val from_indexes = getRows(from)
    val to_indexes = getCols(to)
    from_indexes.map(i => to_indexes.map(j => tes(i)(j).toDouble).sum).sum
  }
  def printIOTable(from: List[String], to: List[String]) {
    print("\t")
    for (to <- list_se) {
      print(to + "\t")
    }
    println()
    for (from <- list_se) {
      print(from + "\t")
      for (to <- list_se) {
        print(ci_from_to(List(from), List(to)) + "\t")
      }
      println()
    }
    print("VA" + "\t")
    for (to <- list_se) {
      print(value_added(getCol(to)) + "\t")
    }
    println()
    print("OUTPUT" + "\t")
    for (to <- list_se) {
      print(output(getCol(to)) + "\t")
    }
  }
}