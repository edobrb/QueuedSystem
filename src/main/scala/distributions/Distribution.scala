package distributions

import java.util.Random

abstract class Distribution(implicit random: Random) extends TimeEventsGenerator with TimeEventGenerator {
  //def f(t: Double): Double

  //def F(t: Double): Double

  /**
   * Given a probability p it gives the time t in which P(x < t) = p is true.
   * @param p the probability
   * @return the time in which the event occur with the specified probability
   */
  def F_inv(p: Double): Double


  def element: Double = F_inv(random.nextDouble())
  def events: Iterable[Double] = LazyList.continually(element).scanLeft(element)(_+_)
}
