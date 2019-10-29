import java.util.Random

import scala.collection.mutable
import scala.math._


trait Distribution {
  def f(t: Double): Double

  def F(t: Double): Double

  def F_inv(p: Double): Double

  def next(implicit random: Random): Double = F_inv(random.nextDouble())

  def generateEvents(implicit random: Random): LazyList[Double] = {
    LazyList.continually(random.nextDouble())
      .map(F_inv)
      .scanLeft(next)(_ + _)
  }
}

case class ExponentialDistribution(λ: Double) extends Distribution {
  def f(t: Double): Double = if (t < 0) 0 else λ * exp(-λ * t)

  def F(t: Double): Double = if (t < 0) 0 else 1 - exp(-λ * t)


  def F_inv(p: Double): Double = log(1 - p) / -λ

  override def toString: String = s"ExpD($λ)"
}

object Helper {
  /**
   * Generate event until from 0 to T according to Distribution D.
   */
  def avgθ(μ: Double): Double = 1 / μ

  def μ(avgθ: Double): Double = 1 / avgθ

  def A0(λ: Double, avgθ: Double): Double = λ * avgθ

  def avgθ(λ: Double, A0: Double): Double = A0 / λ

  implicit class RichDouble(d: Double) {
    def round(decimals: Int): Double = {
      math.round(d * pow(10, decimals)) / pow(10, decimals)
    }
  }

}

import Helper.RichDouble

case class QueuedSystemState(system: QueuedSystem, t: Double, rejected: Int, computed: Int, processing: Int, queued: Int, stats: Stats) {
  override def toString: String = {
    val tR = math.round(t * 100) / 100.0
    s"[λ: ${system.inDistribution}, μ: ${system.outDistribution}, m: ${system.m}, l: ${system.l}] --> {t: $tR | processing: $processing | queued: $queued | computed: $computed | rej: $rejected | stats: $stats"
  }
}

trait QueueEvent {
  def t: Double

  def id: Long
}

case class LeftEvent(override val t: Double, override val id: Long/*, processTime:Double, queuedTime:Double*/) extends QueueEvent

case class EnterEvent(override val t: Double, override val id: Long) extends QueueEvent

case class RejectedEvent(override val t: Double, override val id: Long) extends QueueEvent

case class EnqueuedEvent(override val t: Double, override val id: Long) extends QueueEvent

case class DequeuedEvent(override val t: Double, override val id: Long) extends QueueEvent

case class QueuedSystem(inDistribution: Distribution, outDistribution: Distribution, m: Int, l: Int) {

  def simulate(implicit random: Random): LazyList[QueueEvent] = {

    new Iterator[QueueEvent] {

      case class Item(enterTime: Double, exitTime: Double, id: Long)

      var id: Long = 0
      var cachedNext: Item = _
      var enterStream: Iterator[Double] = inDistribution.generateEvents.iterator
      var inside = 0
      var items: mutable.Buffer[Item] = mutable.Buffer()
      var enqueued: mutable.Buffer[Item] = mutable.Buffer()
      var lastDequeued: Item = _


      def getNext(): Item = {
        if (cachedNext == null) {
          val enter = enterStream.next()
          val left = enter + outDistribution.next
          id = id + 1
          Item(enter, left, id)
        } else {
          val tmp = cachedNext
          cachedNext = null
          tmp
        }

      }

      def peekNext(): Item = {
        if (cachedNext == null) {
          cachedNext = getNext()
        }
        cachedNext
      }

      def newEnterItem(): EnterEvent = {
        val item = getNext()
        items.append(item)
        inside = inside + 1
        EnterEvent(item.enterTime, item.id)
      }

      override def hasNext: Boolean = true

      override def next(): QueueEvent = {
        if (lastDequeued != null) {
          val tmp = lastDequeued
          lastDequeued = null
          DequeuedEvent(tmp.enterTime, tmp.id)
        } else {
          items match {
            case mutable.Buffer() => //will be processed
              newEnterItem()
            case _ =>
              val nextExiting = items.minBy(_.exitTime)
              val next = peekNext()
              if (next.enterTime < nextExiting.exitTime && inside < m) { //will be processed
                newEnterItem()
              } else if (next.enterTime < nextExiting.exitTime && inside < m + l) { //will be enqueued
                val item = getNext()
                enqueued.append(item)
                inside = inside + 1
                EnqueuedEvent(item.enterTime, item.id)
              } else if (next.enterTime < nextExiting.exitTime) { //will be rejected
                val item = getNext()
                RejectedEvent(item.enterTime, item.id)
              } else { //next will left
                items.remove(items.indexOf(nextExiting))
                inside = inside - 1
                if (enqueued.nonEmpty) {
                  lastDequeued = Item(/*enqueued.head.enterTime*/ nextExiting.exitTime, nextExiting.exitTime + outDistribution.next, enqueued.head.id)
                  enqueued remove 0
                  items append lastDequeued
                }
                LeftEvent(nextExiting.exitTime, nextExiting.id)
              }
          }
        }
      }
    }.to(LazyList)
  }


  def simulate2(implicit random: Random): LazyList[QueuedSystemState] =
    simulate.scanLeft[QueuedSystemState](QueuedSystemState(this, 0.0, 0, 0, 0, 0, Stats())) { case (oldState, event) =>
      val newState = event match {
        case RejectedEvent(t_event, _) => oldState.copy(t = t_event, rejected = oldState.rejected + 1)
        case LeftEvent(t_event, _) => oldState.copy(t = t_event, processing = oldState.processing - 1, computed = oldState.computed + 1)
        case EnqueuedEvent(t_event, _) => oldState.copy(t = t_event, queued = oldState.queued + 1)
        case DequeuedEvent(t_event, _) => oldState.copy(t = t_event, processing = oldState.processing + 1, queued = oldState.queued - 1)
        case EnterEvent(t_event, _) => oldState.copy(t = t_event, processing = oldState.processing + 1)
      }

      val dt = event.t - oldState.t
      val newAvgK = (oldState.stats.avgK * oldState.t + (oldState.processing + oldState.queued) * dt) / event.t //average item inside the system
      val newUtilization = (oldState.stats.utilization * oldState.t + (oldState.processing.toDouble / oldState.system.m) * dt) / event.t //utilization of process power
      val newRejectedRatio = if ((newState.rejected + newState.computed) == 0) 0 else newState.rejected.toDouble / (newState.rejected + newState.computed) //rejected ratio ̅Πₚ
      val newLambda = (newState.computed + newState.rejected + newState.queued + newState.processing) / event.t //input rate λ
      val newAvgQueued = (oldState.stats.avgQueued * oldState.t + oldState.queued * dt) / event.t //average queue length
      val newAvgProcessing = newAvgK - newAvgQueued //average processing items count
      val newAvgTheta = newAvgProcessing / (newLambda * (1 - newRejectedRatio)) //average in process time ̅θ

      val processingFull = if(newState.system.l > 0 && (newState.processing == newState.system.m || newState.queued > 0)) 1.0 else 0.0
      val newEnqueuedRatio = (oldState.stats.waitRation * oldState.t + processingFull * dt) / event.t //enqueue ratio ̅Πᵣ


      val A0 = newLambda * newAvgTheta
      val newAvgEpsilon = newAvgTheta / (oldState.system.m - A0)

      val newAvgEta = newAvgQueued * newAvgTheta / oldState.system.m //avg in queue system time ̅η
      val newAvgDelta = newAvgEta + newAvgTheta //avg inside system time ̅δ




      newState.copy(stats = Stats(newRejectedRatio, newAvgK, newUtilization, newAvgTheta, newEnqueuedRatio, newAvgQueued, newAvgEta, newAvgEpsilon))

    }
}


case class Stats(rejectedRatio: Double = 0, avgK: Double = 0, utilization: Double = 0, avgTheta: Double = 0, waitRation:Double = 0, avgQueued:Double = 0, avgEta:Double = 0,avgEpsilon:Double = 0) {
  override def toString: String = {
    s"\n(̅Πₚ = ${rejectedRatio.round(4)}, K̅ = ${avgK.round(4)}, ρ = ${utilization.round(4)}, " +
      s"̅θ = ${avgTheta.round(4)}, ̅Πᵣ = ${waitRation.round(4)}, avgQueue: ${avgQueued.round(4)}, " +
      s"̅η:  ${avgEta.round(4)}), ̅ε: ${avgEpsilon.round(4)}}"
  }
}

object Prove extends App {

  implicit class RichQueuedSystemSimulation(simulation: Iterable[QueuedSystemState]) {
    def utilization: Double = simulation.scanLeft[(Double, Double)]((0.0, 0.0)) { case (v, s: QueuedSystemState) => {
      val dt = s.t - v._2
      val utilization = (s.processing.toDouble / s.system.m) * dt
      (v._1 + utilization, s.t)
    }
    }.map(v => v._1 / v._2).last


    def avgProcessing: Double = simulation.scanLeft[(Double, Double)]((0.0, 0.0)) { case (v, s: QueuedSystemState) => {
      val dt = s.t - v._2
      (v._1 + s.processing * dt, s.t)
    }
    }.map(v => v._1 / v._2).last

    def rejected: Double = {
      val last = simulation.last
      last.rejected.toDouble / (last.computed + last.rejected)
    }

    /*
   persone in coda * dt ==> in quell'intervallo loro hanno aspettato tot
    * */

    def avgTimeInQueue: Double = {
      val last = simulation.last
      simulation.scanLeft[(Double, Double)]((0.0, 0.0)) { case (v, s: QueuedSystemState) => {
        val dt = s.t - v._2
        val timeInQueue = s.queued.toDouble * dt
        (v._1 + timeInQueue, s.t)
      }
      }.map(v => v._1 / (last.computed + last.rejected)).last
    }
  }

  implicit private val random: Random = new Random(1234)

   QueuedSystem(ExponentialDistribution(2), ExponentialDistribution(1), m = 1, l = 20).simulate.take(100).foreach(println)

  val λ = 67
  val A0 = 20
  val avgθ: Double = 0.0121
  val μ = Helper.μ(avgθ)
  val system = QueuedSystem(ExponentialDistribution(λ), ExponentialDistribution(μ), m = 1, l = 1000000)

  val take = 10000000
  val asd = system.simulate2.take(take)

  println(asd.last)

  //println("avg k: " + asd.avgProcessing)
  //println("perdita: " + asd.rejected)
  //println("avg time in queue: " + asd.avgTimeInQueue)
}
