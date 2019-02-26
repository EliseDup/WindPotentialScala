package economic_model

object Input_Output {
  // Energy Sector caracteristics
  val capital_factor = 0.1; val EROI = 10; val life_time = 30;
  // Goods and services sector caracteristics
  val energy_requirement = 1.67; val capital_effectiveness = 0.1;
  // Technical coefficients
  // aie = xie / E = Quantity of energy / capital stock per unit of energy produced (measured by EROI = outputs  / total energy inputs; and capital factor = capital inputs / total energy inputs)
  // aej = xej / Q = Quantity of energy / capital stock per unit of goods and services : energy requirement of the economy
  val A = Array.fill[Double](2,2)(0)
  val x = Array.fill[Double](2,2)(0)
  // Index 0 is the energy sector, index 1 is the economy sector
  x(0)(0) = (1 - capital_factor) * life_time / EROI
  x(0)(1) = energy_requirement
  x(1)(0) = capital_factor * life_time / EROI
  x(1)(1) = capital_effectiveness
}