import java.util.Random

import distributions.ExponentialDistribution
import model.QueuedSystem



object Helper {
  /**
   * Generate event until from 0 to T according to Distribution D.
   */
  def avgθ(μ: Double): Double = 1 / μ

  def μ(avgθ: Double): Double = 1 / avgθ

  def A0(λ: Double, avgθ: Double): Double = λ * avgθ

  def avgθ(λ: Double, A0: Double): Double = A0 / λ
}





object Prove extends App {

  implicit private val random: Random = new Random(1234)


  val λ = 67
  val A0 = 60
  val avgθ: Double = 0.0121
  val μ = Helper.μ(avgθ)
  val system = QueuedSystem(ExponentialDistribution(λ), ExponentialDistribution(μ), m = 1, l = 1000000)

  val take = 1000000
  val asd = system.simulate2.take(take)

  println(asd.last)

  system.leftEvents.events.take(100).foreach(println)
}
