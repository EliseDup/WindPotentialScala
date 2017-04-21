package wind_energy

import squants.energy._
import squants.space._
import squants.mass._
import grid.GridCell

object WindFarmEnergyInputs {
  
  // Energy inputs in transport is in MJ / tkm
  def truckTransport(weight: Mass, distance: Length) = Megajoules(1.25) * weight.toTonnes * Math.abs(distance.toKilometers)
  def shipTransport(weight: Mass, distance: Length) = Megajoules(0.29) * weight.toTonnes * Math.abs(distance.toKilometers)

  val installedCapacity = Gigawatts(1)

  // Fixed value for onshore / offshore bottom fixed / offshore floating
  val fixedOnshore = Gigajoules(13737625)
  val fixedOffshoreFixed = Gigajoules(16902974)
  val fixedOffshoreFloating = Gigajoules(25387974)

  def offshoreFixedFoundations(depth: Length) = scalingFactorFixedFoundations(depth) * (Gigajoules(16173 + 361962 + 10326 + 3477293))
  // For water depth up to 40 m
  def scalingFactorFixedFoundations(depth: Length) = {
    val d = depth.toMeters
    if (d <= 15) 1.0
    else if (d <= 20) 1.08
    else if (d <= 25) 1.34
    else if (d <= 30) 1.57
    else if (d <= 35) 1.95
    else 2.19
  }

  // Operation cost [GJ/GJ]: Depends on output
  def offshoreOperation(output: Energy) = output.toGigajoules * Gigajoules(0.007)
  def onshoreOperation(output: Energy) = output.toGigajoules * Gigajoules(0.035)

  def offshoreInstallation(distanceToCoast: Length) = Gigajoules(16904) * Math.abs(distanceToCoast.toKilometers)
  def onshoreInstallation(distanceToCoast: Length) = Gigajoules(605.74) * Math.abs(distanceToCoast.toKilometers)

  def offshoreCablesEnergyAndInstallation(distanceToCoast: Length) = Gigajoules(4681 + 105) * Math.abs(distanceToCoast.toKilometers)
  // Operation & Maintenance
  def offshoreOM(distanceToCoast: Length) = Gigajoules(6615) * Math.abs(distanceToCoast.toKilometers)
  def onshoreOM(distanceToCoast: Length) = Gigajoules(21.3) * Math.abs(distanceToCoast.toKilometers)
  // Decommissioning
  def offshoreDecommissioning(distanceToCoast: Length) = Gigajoules(12678) * Math.abs(distanceToCoast.toKilometers)

  def windFarmEnergyInputs(cell: GridCell, installedCapacity: Power, output : Energy): Energy = {
    if (cell.onshore) onshoreEnergyInputs(installedCapacity, output, -cell.distanceToCoast)
    else offshoreEnergyInputs(installedCapacity, output, cell.waterDepth, cell.distanceToCoast)
  }

  def onshoreEnergyInputs(installedCap: Power, output : Energy, distanceToCoast: Length): Energy = {
    (installedCap / installedCapacity) * (fixedOnshore + onshoreInstallation(distanceToCoast) + onshoreOM(distanceToCoast)) + onshoreOperation(output)
  }
  def offshoreEnergyInputs(installedCap: Power, output : Energy, waterDepth: Length, distanceToCoast: Length): Energy = {
    (installedCap / installedCapacity) * (offshoreConstructionInputs(waterDepth) + offshoreInstallation(distanceToCoast) + offshoreCablesEnergyAndInstallation(distanceToCoast) + offshoreOM(distanceToCoast) /*+ offshoreDecommissioning(distanceToCoast)*/)  + offshoreOperation(output)
  }
  
  def offshoreConstructionInputs(depth : Length) = {
    if(depth.toMeters > 40) fixedOffshoreFloating
    else fixedOffshoreFixed + offshoreFixedFoundations(depth)
  }
  
  def test {
    assert(Math.abs(onshoreEnergyInputs(Gigawatts(1), Gigajoules(179562042),Kilometers(300)).toGigajoules - 20210408) < 1)
    assert(Math.abs(offshoreEnergyInputs(Gigawatts(1), Gigajoules(309647549.25), Meters(15),Kilometers(60)).toGigajoules - 24634561) < 1)
    assert(Math.abs(offshoreEnergyInputs(Gigawatts(1), Gigajoules(309647549.25), Meters(100),Kilometers(60)).toGigajoules - 29253807) < 1)     
  }

}