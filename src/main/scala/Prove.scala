import java.util.Random

import Prove.{system, take}
import distributions.ExponentialDistribution
import model.QueuedSystem
import  utils.RichDouble._


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

  println(system.statesSimulation.take(take).last.stats)

  val system2 = QueuedSystem(system.leftEvents, ExponentialDistribution(μ), m = 1, l = 1000000)

  println(system2.statesSimulation.take(take).last.stats)

  //system.leftEvents.events.take(100).foreach(println)
}

object Test extends App {


  implicit private val random: Random = new Random(1234)
  val λ = 67
  val A0 = 60
  val avgθ: Double = 0.0121
  val μ = Helper.μ(avgθ)
  val system = QueuedSystem(ExponentialDistribution(λ), ExponentialDistribution(μ), m = 1, l = 1000000)

  val take = 10000000

  val stats = system.statesSimulation.take(take).last.stats

  println(stats)
  assert(stats.waitRatio between 0.805 and 0.815)
  assert(stats.utilization between 0.805 and 0.815)
  assert(stats.avgK between 4.2 and 4.4)
  assert(stats.avgTheta.value between 0.012 and 0.0122)
  assert(stats.avgEta.value between 0.0500 and 0.0530)
  assert(stats.avgEpsilon.value between 0.0630 and 0.0660)
  assert(stats.rejectedRatio == 0)
  assert(stats.avgQueued between 3.40 and 3.55)
}
