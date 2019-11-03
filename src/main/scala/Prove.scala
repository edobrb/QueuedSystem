import java.util.Random

import distributions.{ExponentialDistribution, FixedDistribution}
import model.{QueueEvents, QueuedSystem}
import utils.Avg
import utils.RichIterator._
import utils.RichDouble._

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


  val λ = 67 / 2.0
  val A0 = 60
  val avgθ: Double = 0.0121
  val μ = Helper.μ(avgθ)
  val system = QueuedSystem(ExponentialDistribution(λ), ExponentialDistribution(μ), m = 1, l = 1000000)

  val take = 10000000

  println(system.statesSimulation.take(take).last.stats)

  val system2 = QueuedSystem(system.output + system.output, ExponentialDistribution(μ), m = 1, l = 1000000)

  println(system2.statesSimulation.take(take).last.stats)

  val system3 = QueuedSystem(ExponentialDistribution(λ * 2), FixedDistribution(0.0121), m = 1, l = 1000000)

  println(system3.statesSimulation.take(take).last.stats)

  //system.leftEvents.events.take(100).foreach(println)
}

object Test extends App {



  implicit private val random: Random = new Random(1234)
  val λ = 67
  val A0 = 60
  val avgθ: Double = 0.0121
  val μ = Helper.μ(avgθ)
  val system = QueuedSystem(ExponentialDistribution(λ), ExponentialDistribution(μ), m = 1, l = 1000000)

  //val n = 1000000
  val n = 10000000


  val last = system.statesSimulation.take(n).last
  val stats = last.stats

  println(last)
  assert(stats.waitRatio between 0.805 and 0.815)
  assert(stats.utilization between 0.805 and 0.815)
  assert(stats.avgK between 4.2 and 4.4)
  assert(stats.avgTheta.value between 0.012 and 0.0122)
  assert(stats.avgEta.value between 0.0500 and 0.0530)
  assert(stats.avgEpsilon.value between 0.0630 and 0.0660)
  assert(stats.rejectedRatio == 0)
  assert(stats.avgQueued between 3.40 and 3.55)

  val p = system.eventsSimulation.take(n).collect {
    case QueueEvents.DequeuedEvent(_, _, queuedTime) => if (queuedTime < 0.2) 1 else 0
  }.foldLeft(Avg())((avg, queuedTime) => avg.add(queuedTime))

  println("P(ε > 0.2) = " + p)
}
