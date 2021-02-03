package distributions

import java.util.Random

import distributions.EventsGenerator._


abstract class Distribution {
  /**
   * Given a probability p it gives the time t in which P(x < t) = p is true.
   *
   * @param p the probability
   * @return the time in which the event occur with the specified probability
   */
  def F_inv(p: Double): Double
}
