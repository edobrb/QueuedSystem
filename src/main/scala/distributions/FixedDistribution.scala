package distributions

import java.util.Random

import utils.RichDouble._

/**
 * A fixed distribution, P(v) = 1
 *
 * @param v the value of this fixed distribution
 */
case class FixedDistribution(v: Double) extends Distribution {
  def F_inv(p: Double): Double = v

  override def toString: String = s"FixedD(${v.round(4)})"
}