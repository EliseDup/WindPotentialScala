package economic_model

import wind_solar._
import squants.time.Hours
import squants.energy._
import wind_energy._
import solar_energy._
import utils._
import solar_energy.PVMono
import solar_energy.CSPTowerStorage12h
import squants.motion.MetersPerSecond
import squants.motion.Velocity

object ProductionFunction {
  import PlotHelper._
  import Helper._

  val all_sites = Grid().cells
  
  val ge_params = Map(
      (OnshoreWindTechnology, ( -0.0660122912605, 11.6932829079, 8.16493401788 )),
      (OffshoreWindTechnology, ( -0.0812000868004, 9.3508428644, 0.836906474931 )),
      (PVMono, ( -0.0171951862575 , 11.1382977802 , -13.9870880741 )),
      (CSPTowerStorage12h, ( -0.190393047847 , 17.9105310119 , 3.06044444688 )))
  val ne_params = Map(
      (OnshoreWindTechnology, ( -0.0637018610663, 10.2840180061, 7.87916132725 )),
      (OffshoreWindTechnology, ( -0.0806316861928, 8.28538696435, 0.831048129606 )),
      (PVMono, ( -0.0170283929508 , 10.0302562917 , -13.8514133198 )),
      (CSPTowerStorage12h, ( -0.176494355354 , 15.6030622481 , 2.83703200226 )))
  val ge_params_total = ( -0.00982610467919 , 10.2006852367 , 9.46741133647 )
  val ne_params_total = ( -0.00962975230951 , 9.03258327511 , 8.64449520532 )
  def main(args: Array[String]): Unit = {
    val techs = List(OnshoreWindTechnology,OffshoreWindTechnology,PVMono,CSPTowerStorage12h)
    techs.map(t => {
      val f = new ProductionFunction(all_sites, List(t), t.name)
      f.plot(ne_params.get(t))
      f.write(f.name)
    })

    val total = new ProductionFunction(all_sites, List(OnshoreWindTechnology, OffshoreWindTechnology, PVMono), "Wind and Solar")
    total.plot(Some(ne_params_total))
    total.write("wind_solar")
  }

}

class ProductionFunction(val sites: List[Cell], val techs: List[RenewableTechnology], val name: String,
    val defaultVR: Velocity = MetersPerSecond(11), val defaultN: Double = 8, val defaultSM: Double = 2.7) {
  import PlotHelper._
  import Helper._

  val sites_sf = sites.filter(s => techs.map(_.suitabilityFactor(s)).sum > 0)
  println("# Suitable sites for " + name + " = " + sites_sf.size + " / " + sites.size)

  // Build the list of Gross Energy Produced, Net Energy Produced, Embodied Energy) for all the sites
  val e_ne_ee: List[(Double, Energy, Energy, Energy)] = sites_sf.map(s => {
    val e_ee = techs.map(potential_ee(s, _))
    val e = e_ee.map(_._1).foldLeft(Joules(0))(_ + _)
    val ne = e_ee.map(i => i._2).foldLeft(Joules(0))(_ + _)
    val ee = e_ee.map(_._3).foldLeft(Joules(0))(_ + _)
    (e / ee, e, ne, ee)
  })
  // (Embodied Energy, Energy) cumulated, by decreasing ratio of e/ee
  val ee_e_cum = doubleToEnergy(Helper.listCumulatedVSCumulatedBy(e_ne_ee.map(i => (i._1, i._4.toGigajoules, i._2.toGigajoules))))
  // (Embodied Energy, Net Energy) cumulated, by decreasing ratio of e/ee
  val ee_ne_cum = doubleToEnergy(Helper.listCumulatedVSCumulatedBy(e_ne_ee.map(i => (i._1, i._4.toGigajoules, i._3.toGigajoules))))

  def plot(interpolation: Option[(Double, Double, Double)] = None) {
    val ee_e_cum_double = energyToDouble(ee_e_cum)
    val ee_ne_cum_double = energyToDouble(ee_ne_cum)
    println("Max E " + math.round(ee_e_cum_double._2.max))
    println("Max Net E " + math.round(ee_ne_cum_double._2.max))

    var list = List((ee_e_cum_double._1, ee_e_cum_double._2, "Gross Energy"))
    list = list ++ List((ee_ne_cum_double._1, ee_ne_cum_double._2, "Net Energy"))
    interpolation match {
      case Some(params) => {
        val x = (0 to ee_e_cum_double._1.max.toInt).map(_.toDouble).toList
        val inter = List((x, x.map(i => params._1 * i * i + params._2 * i + params._3), "Interpolation"))
        list = list ++ inter
      }
      case None => {}
    }
    plotXY(list, yLabel = name + " [EJ/year]", xLabel = "Embodied Energy [EJ/year]", title = name, legend = true)
  }

  // Write Energy / Net Energy / Embodied Energy
  def write(output: String) {
    val out_stream = new java.io.PrintStream(new java.io.FileOutputStream(output))
    val ee_e_cum_double = energyToDouble(ee_e_cum)
    val ee_ne_cum_double = energyToDouble(ee_ne_cum)
    for (i <- 0 until ee_e_cum_double._1.size) {
      assert(ee_e_cum_double._1(i) == ee_ne_cum_double._1(i))
      out_stream.print(ee_e_cum_double._2(i) + "\t" + ee_ne_cum_double._2(i) + "\t" + ee_e_cum_double._1(i) + "\n")
    }
    out_stream.close()
  }

  def potential_ee(site: Cell, tech: RenewableTechnology): (Energy, Energy, Energy) = {
    if (tech.wind) {
      val windTech = tech.asInstanceOf[WindTechnology]
      val grossE = windTech.power(site, defaultVR, defaultN) * Hours(365 * 24)
      val EE = windTech.embodiedEnergy(site, windTech.ratedPower(site, defaultVR, defaultN), Joules(0)) / windTech.lifeTime
      val netE = windTech.power(site, defaultVR, defaultN) * Hours(365 * 24) * (1 - windTech.operation_variable.toGigajoules) - EE
      (grossE, netE, EE)

    } else if (tech.csp) {
      val cspTech = tech.asInstanceOf[CSP]
      if (site.dni.value == 0) (Joules(0), Joules(0), Joules(0))
      else {
        val panelArea = cspTech.reflectiveArea(site)
        val ratedPower = cspTech.ratedPower(panelArea, defaultSM)
        val grossE = cspTech.potential(site.dni, panelArea, defaultSM) * Hours(365 * 24)
        val EE = cspTech.embodiedEnergy(ratedPower, Joules(0), panelArea) / cspTech.lifeTime
        val netE = grossE * (1 - cspTech.operation_variable.toGigajoules) - EE
        (grossE, netE, EE)
      }
    } else {
      if (site.ghi.value == 0) (Joules(0), Joules(0), Joules(0))
      else {
        val pvTech = tech.asInstanceOf[PV]
        val panelArea = pvTech.reflectiveArea(site)
        val ratedPower = pvTech.ratedPower(site, 1.0)
        val grossE = pvTech.potential(site) * Hours(365 * 24)
        val EE = pvTech.embodiedEnergy(ratedPower, Joules(0), panelArea) / pvTech.lifeTime 
        val netE = grossE * (1 - pvTech.operation_variable.toGigajoules) - EE
        (grossE, netE, EE)

      }
    }
  }

  def energyToDouble(list: (List[Energy], List[Energy])) = (list._1.map(_.to(Exajoules)), list._2.map(_.to(Exajoules)))
  def doubleToEnergy(list: (List[Double], List[Double])) = (list._1.map(Gigajoules(_)), list._2.map(Gigajoules(_)))

}