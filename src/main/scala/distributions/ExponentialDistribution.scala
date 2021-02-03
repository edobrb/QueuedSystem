package distributions

import utils.RichDouble._

import scala.math.log

case class ExponentialDistribution(λ: Double) extends Distribution {
  def F_inv(p: Double): Double = log(1 - p) / -λ

  override def toString: String = s"ExpD(${λ.round(4)})"
}
