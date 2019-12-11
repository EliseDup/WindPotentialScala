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

  val ed_params = Map(
    (OnshoreWindTechnology, ( -0.0637018610663 , 11.2840180061 , 7.87916132725 )),
    (OffshoreWindTechnology, ( -0.0806316861928 , 9.28538696435 , 0.831048129606 )),
    (PVMono, ( -0.0172059611559 , 11.0685240846 , -15.6644974786 )),
    (CSPTowerStorage12h, ( -0.175549090097 , 16.5633370464 , 3.19917790027 )))
   val ed_params_total =( -0.0105137914601 , 10.2371376248 , 2.35772267845 )
  def main(args: Array[String]): Unit = {
    
    val techs = List(OnshoreWindTechnology, OffshoreWindTechnology, PVMono, CSPTowerStorage12h)
    techs.map(t => {
      val f = new ProductionFunction(all_sites, List(t), t.name)
    //  f.plotCapitalIntensity(t.name)
      f.plot(Some(ed_params(t)))
    //  f.write(f.name)
    })

    val total = new ProductionFunction(all_sites, List(OnshoreWindTechnology, OffshoreWindTechnology, PVMono), "Wind and Solar")
    //total.plotCapitalIntensity("Total")
    total.plot(Some(ed_params_total))
    //total.write("wind_solar")
  }

}

class ProductionFunction(val sites: List[Cell], val techs: List[RenewableTechnology], val name: String,
    val defaultVR: Velocity = MetersPerSecond(11), val defaultN: Double = 8, val defaultSM: Double = 2.7) {
  import PlotHelper._
  import Helper._

  val sites_sf = sites.filter(s => techs.map(_.suitabilityFactor(s)).sum > 0)
  println("# Suitable sites for " + name + " = " + sites_sf.size + " / " + sites.size)

  // Build the list of Gross Energy Produced, Net Energy Produced, Embodied Energy, Operational Energy) for all the sites
  val e_ne_ee: List[(Double, Energy, Energy, Energy)] = sites_sf.map(s => {
    val e_ee = techs.map(potential_ee(s, _))
    val e = e_ee.map(_._1).foldLeft(Joules(0))(_ + _)
    val ne = e_ee.map(_._2).foldLeft(Joules(0))(_ + _)
    val ee = e_ee.map(_._3).foldLeft(Joules(0))(_ + _)
    (e / ee, e, ne, ee)
  })
  // (Embodied Energy, Energy delivered) cumulated, by decreasing ratio of e/ee
  val ee_ed_cum = doubleToEnergy(Helper.listCumulatedVSCumulatedBy(e_ne_ee.map(i => (i._1, i._4.toGigajoules, i._2.toGigajoules))))

  def plot(interpolation: Option[(Double, Double, Double)] = None) {
    val ee_ed_cum_double = energyToDouble(ee_ed_cum)
    println("Max Ed " + math.round(ee_ed_cum_double._2.max))

    var list = List((ee_ed_cum_double._1, ee_ed_cum_double._2, "Energy Delivered"))

    interpolation match {
      case Some(params) => {
        val max_x = (-params._2/(2*params._1)).toInt
        println(max_x + "\t" + ee_ed_cum_double._1.max)
        val x = (0 to max_x+10).map(_.toDouble).toList //ee_ed_cum_double._1.max.toInt).map(_.toDouble).toList
        val inter = List((x, x.map(i => params._1 * i * i + params._2 * i + params._3), "Interpolation"))
        list = list ++ inter
      }
      case None => {}
    }
    plotXY(list, yLabel = name + " [EJ/year]", xLabel = "Embodied Energy [EJ/year]", title = name, legend = list.size > 1)
  }

  // WRONG !!
  /*def plotCapitalIntensity(name: String = "") {
    val k_nete = e_ne_ee.map(i => (i._1, i._4 / (i._4 + i._5), i._2.to(Exajoules)))
    val k_nete_cum = Helper.listValueVSCumulatedBy(k_nete, true)
    plotXY(List((k_nete_cum._1, k_nete_cum._2, "")), xLabel = name + ", gross [EJ/year]", yLabel = "Capital Intensity", title = name)

  }*/
  // Write Energy / Net Energy / Embodied Energy
  def write(output: String) {
    val out_stream = new java.io.PrintStream(new java.io.FileOutputStream(output))
    val ee_ed_cum_double = energyToDouble(ee_ed_cum)
    for (i <- 0 until ee_ed_cum_double._1.size) {
      out_stream.print(ee_ed_cum_double._2(i) + "\t" + ee_ed_cum_double._1(i) + "\n")
    }
    out_stream.close()
  }

  def potential_ee(site: Cell, tech: RenewableTechnology): (Energy, Energy, Energy) = {
    if (tech.wind) {
      val windTech = tech.asInstanceOf[WindTechnology]
      val grossE = windTech.potential(site, defaultVR, defaultN) * Hours(365 * 24)
      val EE = windTech.embodiedEnergy(site, windTech.ratedPower(site, defaultVR, defaultN)) / windTech.lifeTime
      val netE = grossE - EE
      (grossE, netE, EE)

    } else if (tech.csp) {
      val cspTech = tech.asInstanceOf[CSP]
      if (site.dni.value == 0) (Joules(0), Joules(0), Joules(0))
      else {
        val panelArea = cspTech.reflectiveArea(site)
        val ratedPower = cspTech.ratedPower(panelArea, defaultSM)
        val grossE = cspTech.potential(site.dni, panelArea, defaultSM) * Hours(365 * 24)
        val EE = cspTech.embodiedEnergy(ratedPower, panelArea) / cspTech.lifeTime
        val netE = grossE - EE
        (grossE, netE, EE)
      }
    } else {
      if (site.ghi.value == 0) (Joules(0), Joules(0), Joules(0))
      else {
        val pvTech = tech.asInstanceOf[PV]
        val panelArea = pvTech.reflectiveArea(site)
        val ratedPower = pvTech.ratedPower(site, 1.0)
        val grossE = pvTech.potential(site) * Hours(365 * 24)
        val EE = pvTech.embodiedEnergy(ratedPower, panelArea) / pvTech.lifeTime
        val netE = grossE - EE
        (grossE, netE, EE)

      }
    }
  }

  def energyToDouble(list: (List[Energy], List[Energy])) = (list._1.map(_.to(Exajoules)), list._2.map(_.to(Exajoules)))
  def doubleToEnergy(list: (List[Double], List[Double])) = (list._1.map(Gigajoules(_)), list._2.map(Gigajoules(_)))

}