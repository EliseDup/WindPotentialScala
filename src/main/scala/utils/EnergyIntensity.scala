package utils

import squants.UnitOfMeasure
import squants.UnitConverter
import squants.PrimaryUnit
import squants.SiUnit
import squants.energy._
import squants.market._
import squants.UnitOfMeasure
import squants.Quantity
import squants.market.USD
import squants.AbstractQuantityNumeric
import squants.Dimension
import squants.Ratio
import squants.market.defaultMoneyContext

/**
 * Represents an energy intensity
 *
 * A price is an [[squants.Ratio]] between a quantity of [[squants.energy.Energy]]
 * and some other [[squants.Quantity]]
 *
 * @author  garyKeorkunian
 * @since   0.1
 *
 * @param energy Energy
 * @param quantity Quantity
 * @tparam A Quantity Type
 */
/*case class Intensity[A <: Quantity[A],B <: Quantity[B]](a: A, b: B) extends Ratio[A, B] with Serializable {
  def base = a
  def counter = b
  
  def times(that: Intensity[B,A]): Double = that.a/this.b * this.a/that.b
  def *(that: B): A = this.a / (that/this.b)
  def *(that: Double): Intensity[A,B] = Intensity(this.a * that, b)
  
  def /(that: Intensity[A,B]): Double = this.a/that.a * that.b/this.b  
  
}

class EnergyIntensity(a: Energy, b: Money) extends Intensity[Energy, Money](a,b) {

  //def plus(that: EnergyIntensity): EnergyIntensity = EnergyIntensity(this.energy + that.energy, this.quantity+that.quantity)
  //def +(that: EnergyIntensity): EnergyIntensity = plus(that)
  //def minus(that: EnergyIntensity): EnergyIntensity = EnergyIntensity(this.energy - that.energy, quantity)
  //def -(that: EnergyIntensity): EnergyIntensity = minus(that)

  def times(that: Double): EnergyIntensity = new EnergyIntensity(this.a * that, b)
  def *(that: Double): EnergyIntensity = new EnergyIntensity(this.a * that, b)
    
  def divide(that: Double): EnergyIntensity = new EnergyIntensity(this.a / that, b)
  def /(that: Double): EnergyIntensity = divide(that)

  /**
   * Returns the Cost (Money) for a quantity `that` of A
   * @param that Quantity
   * @return
   */
  def *(that: Money): Energy = convertToBase(that)
  def *(that: Price[Energy]): Double = this*that // this.energy/that.quantity*that.money/this.quantity
  
  override def toString = a.toString + "/" + b.toString

  def toString(energy_unit: EnergyUnit, unit: UnitOfMeasure[Money]) =
    a.toString(energy_unit) + "/" + b.toString(unit)
}*/

/**
 * Represents a quantity of energy per units of money
 *
 * @author  garyKeorkunian
 * @since   0.1
 *
 * @param value value in [[squants.energy.WattHours]]
 *//*
final class EnergyIntensity private (val value: Double, val unit: EnergyIntensityUnit)
    extends Quantity[EnergyIntensity] {

  def dimension = EnergyIntensity

  def *(that: Money): Energy = KilowattHoursPerDollar(this.to(KilowattHoursPerDollar) * that.to(USD).toDouble)

  def toKiloWattHoursPerDollar = to(KilowattHoursPerDollar)
}

object EnergyIntensity extends Dimension[EnergyIntensity] {
  private[energy] def apply[A](n: A, unit: EnergyIntensityUnit)(implicit num: Numeric[A]) = new EnergyIntensity(num.toDouble(n), unit)
  def apply(value: Any) = parse(value)
  def name = "EnergyIntensity"
  def primaryUnit = KilowattHoursPerDollar
  def siUnit = KilowattHoursPerDollar
  def units = Set(KilowattHoursPerDollar)
}

trait EnergyIntensityUnit extends UnitOfMeasure[EnergyIntensity] with UnitConverter {
  def apply[A](n: A)(implicit num: Numeric[A]) = EnergyIntensity(n, this)
}

object KilowattHoursPerDollar extends EnergyIntensityUnit with PrimaryUnit with SiUnit {
  val symbol = KilowattHours.symbol + "/" + USD.symbol
}

object EnergyDensityConversions {
  lazy val kilowattHoursPerDollar = KilowattHoursPerDollar(1)

  implicit class EnergyDensityConversions[A](n: A)(implicit num: Numeric[A]) {
    def kilowattHoursPerDollar = KilowattHoursPerDollar(n)
  }

  implicit object EnergyIntensityNumeric extends AbstractQuantityNumeric[EnergyIntensity](EnergyIntensity.primaryUnit)
}*/