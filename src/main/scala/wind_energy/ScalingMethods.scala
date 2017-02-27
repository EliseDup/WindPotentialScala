package wind_energy
/**
 * Give the weight of each components only as a function of rotor radius, hub heigth, and nominal power
 * 
 * From : 
 * 
 * NREL - 
 * Wind Turbine Design Cost and Scaling Model
 * L. Fingersh, M. Hand, and A. Laxson
 * 
 */

class ScalingMethods(val radius: Int, val hubHeight: Int, val power: Int) {

  val diameter = 2 * radius
  val area = Math.PI * radius * radius
  val blade = 0.1452 * Math.pow(radius, 2.9158)
  val hub = 0.954 * blade + 5680.3
  val pitchBearing = 0.1295 * 3 * blade + 491.31
  val pitchSystem = pitchBearing * 1.328 + 555
  val noseCone = 18.5 * diameter - 520.5
  val lowSpeedShaft = 0.0142 * Math.pow(diameter, 2.888)
  val bearing = (diameter * 8.0 / 600.0 - 0.033) * 0.0062 * Math.pow(diameter, 2.5)
  // gearbox
  val brake = (1.9894 * power - 0.1141) / 10.0
  val generator = 6.47 * Math.pow(power, 0.9223) // 3 possibilities

  val yaw = 1.6 * 0.0009 * Math.pow(diameter, 3.314)
  val mainFrame = 2.233 * Math.pow(diameter, 1.953)
  val cooling = 12 * power
  val nacelleCover = 11.537 * power + 3849.7

  val tower = 0.3973 * area * hubHeight - 1414
}