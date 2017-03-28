

import utils.PlotHelper
import squants.space.Length
import squants.energy.Power
import squants.energy.Megawatts
import squants.space.Meters
import squants.motion.Velocity
import squants.motion.MetersPerSecond

object WindTurbineDesign {
  
  def main(args: Array[String]): Unit = {
    val diam = (0 to 2500).toList.map(_*0.1)
    val k1 = 1.789; val a1 = 5.0 / Math.pow(135,k1);
    val k2 = 2.292; val a2 = 5.0 / Math.pow(135,k2);
    val k3 = 2; val a3 = 0.3;
    
    PlotHelper.plotXY(List( 
        (diam, diam.map(d => a2*Math.pow(d,k2)), "All WT"), (diam, diam.map(d =>a1*Math.pow(d,k1)), "Existing WT"),
        (diam, diam.map(d => a3*Math.pow(d,k3)), "0.3 D^2")), legend = true)
    
    // Installed capacity density = MW / km2
    // Only depends on turbine spacing n -> MW / km2 = Rated Power / (n * diameter)^2
    // We power law : RP = a * D^k -> MW / km2 = a * D^(k-2) / n2
    
    PlotHelper.plotXY( diam, diam.map(d => a3*Math.pow(d,k2-2.0)))
    // Top 10 biggest WT
    val biggestWT = List((10.0,190),(10.0,164),(8.0,180),(7.5,126),(7.0,171),(6.0,152),(6.0,154),(6.0,150),(6.0,128))
       
    biggestWT.map(wt => println(installedCD(Megawatts(wt._1), Meters(wt._2))))
  }
  def installedCD(ratedPower : Power, rotorDiameter : Length, n : Double = 10) = {
      ratedPower / (n*n*rotorDiameter*rotorDiameter)
    }
  def maxInstalledCD(ratedWindSpeed : Double = 11, n : Double = 10, cp : Double = 0.43) = {
    cp*0.5*1.225*Math.PI/4.0*Math.pow(ratedWindSpeed,3) / (n*n)
  }
  
}