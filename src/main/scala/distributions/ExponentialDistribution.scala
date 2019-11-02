package distributions

import java.util.Random

import utils.RichDouble._

import scala.math.log

case class ExponentialDistribution(λ: Double)(implicit random: Random) extends Distribution {
  //def f(t: Double): Double = if (t < 0) 0 else λ * exp(-λ * t)

  //def F(t: Double): Double = if (t < 0) 0 else 1 - exp(-λ * t)

  def F_inv(p: Double): Double = log(1 - p) / -λ

  override def toString: String = s"ExpD(${λ.round(4)})"
}
