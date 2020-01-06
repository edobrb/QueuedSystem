import java.util.Random

import distributions.EventGenerator._
import distributions.EventsGenerator._
import distributions.ExponentialDistribution
import model.{PrioritizedQueuedSystem, QueueEvents, QueuedSystem}
import utils.Avg
import utils.RichDouble._
import utils.RichIterator._

object Helper {
  def avgθ(μ: Double): Double = 1 / μ

  def μ(avgθ: Double): Double = 1 / avgθ

  def A0(λ: Double, avgθ: Double): Double = λ * avgθ

  def avgθ(λ: Double, A0: Double): Double = A0 / λ
}





object Prove extends App {

  implicit private val random: Random = new Random(1234)


  val λ = 10
  val avgθ: Double = 1
  val μ = Helper.μ(0.004)
  //val lines = QueuedSystem(inEvents = ExponentialDistribution(λ), serviceDurations = ExponentialDistribution(μ), m = 4, l = 0)
  val system = PrioritizedQueuedSystem(
    insEvents = Seq(ExponentialDistribution(10), ExponentialDistribution(50), ExponentialDistribution(50)),
    servicesDurations = Seq(ExponentialDistribution(μ), ExponentialDistribution(μ / 2), ExponentialDistribution(μ / 2)),
    m = 1, l = 1000)


  //val system2 = QueuedSystem(inEvents = system.rejected, serviceDurations = FixedDistribution(μ), m = 6, l = 0)

  val take = 100000000

  val a = system.statesSimulation.take(take).last
  println(a.stats)
  println(system.getTraffic(0).statesSimulation.takeWhile(_.t < a.t).last.stats)
  println(system.getTraffic(1).statesSimulation.takeWhile(_.t < a.t).last.stats)
  println(system.getTraffic(2).statesSimulation.takeWhile(_.t < a.t).last.stats)


  //println(system2.statesSimulation.takeWhile(_.t < a.t).last.stats)
  /*
    val system2 = QueuedSystem(system.output + system.output, ExponentialDistribution(μ), m = 1, l = 1000000)

    println(system2.statesSimulation.take(take).last.stats)

    val system3 = QueuedSystem(ExponentialDistribution(λ * 2), FixedDistribution(0.0121), m = 1, l = 1000000)

    println(system3.statesSimulation.take(take).last.stats)

    //system.leftEvents.events.take(100).foreach(println)*/
}

object Test extends App {



  implicit private val random: Random = new Random(1234)
  val λ = 67
  val A0 = 60
  val avgθ: Double = 0.0121
  val μ = Helper.μ(avgθ)
  val system = QueuedSystem(ExponentialDistribution(λ), ExponentialDistribution(μ), m = 1, l = 1000)

  //val n = 1000000
  val n = 100000000


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

  println("P(ε < 0.2) = " + p)
  assert(p.value > 0.95)
}
