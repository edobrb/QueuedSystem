import java.util.Random

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

  import Helper.RichDouble

  override def toString: String = s"ExpD(${λ.round(4)})"
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

case class QueuedSystemState(system: QueuedSystem, t: Double, rejected: Int, computed: Int, processing: Int, queued: Int, totalDequeued: Int, stats: Stats) {
  override def toString: String = {
    val tR = math.round(t * 100) / 100.0
    s"[λ: ${system.inDistribution}, μ: ${system.outDistribution}, m: ${system.m}, l: ${system.l}] --> {t: $tR | " +
      s"processing: $processing | queued: $queued | computed: $computed | rej: $rejected | totalDequeued: $totalDequeued | stats: $stats"
  }
}

trait QueueEvent {
  def t: Double

  def id: Long
}

case class LeftEvent(override val t: Double, override val id: Long, processTime: Double, queuedTime: Double) extends QueueEvent

case class EnterEvent(override val t: Double, override val id: Long) extends QueueEvent

case class RejectedEvent(override val t: Double, override val id: Long) extends QueueEvent

case class EnqueuedEvent(override val t: Double, override val id: Long) extends QueueEvent

case class DequeuedEvent(override val t: Double, override val id: Long, queuedTime: Double) extends QueueEvent



case class QueuedSystem(inDistribution: Distribution, outDistribution: Distribution, m: Int, l: Int) {

  def simulate(implicit random: Random): LazyList[QueueEvent] = {

    new Iterator[QueueEvent] {

      case class Item(enterTime: Double, queueExitTime: Double, exitTime: Double, id: Long)

      var id: Long = 0
      var cachedNext: Item = _
      var enterStream: Iterator[Double] = inDistribution.generateEvents.iterator
      var inside = 0
      var items: FastFixedOrderedQueue[Item] = new FastFixedOrderedQueue[Item](m, (i1, i2) => i1.exitTime - i2.exitTime)
      var enqueued: FastFixedQueue[Item] = new FastFixedQueue[Item](l)
      var lastDequeued: Item = _


      def getNext(): Item = {
        if (cachedNext == null) {
          val enter = enterStream.next()
          val left = enter + outDistribution.next
          id = id + 1
          Item(enter, enter, left, id)
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
        items.enqueue(item)
        inside = inside + 1
        EnterEvent(item.enterTime, item.id)
      }

      override def hasNext: Boolean = true

      override def next(): QueueEvent = {
        if (lastDequeued != null) {
          val tmp = lastDequeued
          lastDequeued = null
          DequeuedEvent(tmp.queueExitTime, tmp.id, tmp.queueExitTime - tmp.enterTime)
        } else {
          items match {
            case _ if items.isEmpty => //will be processed
              newEnterItem()
            case _ =>
              val nextExiting = items.peek().get
              val next = peekNext()
              if (next.enterTime < nextExiting.exitTime && inside < m) { //will be processed
                newEnterItem()
              } else if (next.enterTime < nextExiting.exitTime && inside < m + l) { //will be enqueued
                val item = getNext()
                enqueued.enqueue(item)
                inside = inside + 1
                EnqueuedEvent(item.enterTime, item.id)
              } else if (next.enterTime < nextExiting.exitTime) { //will be rejected
                val item = getNext()
                RejectedEvent(item.enterTime, item.id)
              } else { //next will left
                items.dequeue()
                inside = inside - 1
                val queueHead = enqueued.dequeue()
                if (queueHead.isDefined) {
                  lastDequeued = Item(queueHead.get.enterTime, nextExiting.exitTime, nextExiting.exitTime + outDistribution.next, queueHead.get.id)
                  items enqueue lastDequeued
                }
                LeftEvent(nextExiting.exitTime, nextExiting.id, nextExiting.exitTime - nextExiting.queueExitTime, nextExiting.queueExitTime - nextExiting.enterTime)
              }
          }
        }
      }
    }.to(LazyList)
  }


  def simulate2(implicit random: Random): LazyList[QueuedSystemState] =
    simulate.scanLeft[QueuedSystemState](QueuedSystemState(this, 0.0, 0, 0, 0, 0, 0, Stats())) { case (oldState, event) =>
      val newState = event match {
        case RejectedEvent(t_event, _) => oldState.copy(t = t_event, rejected = oldState.rejected + 1)

        case LeftEvent(t_event, _, processTime, queueTime) =>
          oldState.copy(t = t_event, processing = oldState.processing - 1, computed = oldState.computed + 1,
            stats = oldState.stats.copy(
              avgTheta = oldState.stats.avgTheta.add(processTime),
              avgEta = oldState.stats.avgEta.add(queueTime)
            ))


        case EnqueuedEvent(t_event, _) => oldState.copy(t = t_event, queued = oldState.queued + 1)

        case DequeuedEvent(t_event, _, queueTime) =>
          oldState.copy(t = t_event, processing = oldState.processing + 1, queued = oldState.queued - 1, totalDequeued = oldState.totalDequeued + 1, stats = oldState.stats.copy(
            avgEpsilon = oldState.stats.avgEpsilon.add(queueTime)
          ))

        case EnterEvent(t_event, _) => oldState.copy(t = t_event, processing = oldState.processing + 1)
      }

      val dt = event.t - oldState.t
      val newAvgK = (oldState.stats.avgK * oldState.t + (oldState.processing + oldState.queued) * dt) / event.t //average item inside the system K̅

      val newUtilization = (oldState.stats.utilization * oldState.t + (oldState.processing.toDouble / oldState.system.m) * dt) / event.t //utilization of process power ρ

      val processingFull = if (newState.system.l > 0 && (newState.processing == newState.system.m || newState.queued > 0)) 1.0 else 0.0
      val newEnqueuedRatio = (oldState.stats.waitRation * oldState.t + processingFull * dt) / event.t //enqueue ratio ̅Πᵣ

      val newRejectedRatio = if ((newState.rejected + newState.computed) == 0) 0 else newState.rejected.toDouble / (newState.rejected + newState.computed) //rejected ratio ̅Πₚ


      val newAvgQueued = (oldState.stats.avgQueued * oldState.t + oldState.queued * dt) / event.t //average queue length

      //val newLambda = (newState.computed + newState.rejected + newState.queued + newState.processing) / event.t //input rate λ
      //val A0 = newLambda * newState.stats.avgTheta //Erlangs
      // val newAvgEpsilon = newState.stats.avgTheta / (oldState.system.m - A0) //average time in queue only if enqueued ̅ε


      newState.copy(stats = Stats(
        newRejectedRatio,
        newAvgK,
        newUtilization,
        newState.stats.avgTheta,
        newEnqueuedRatio,
        newAvgQueued,
        newState.stats.avgEta,
        newState.stats.avgEpsilon))
    }
}


case class Stats(rejectedRatio: Double = 0, avgK: Double = 0, utilization: Double = 0, avgTheta: Avg = Avg(),
                 waitRation: Double = 0, avgQueued: Double = 0, avgEta: Avg = Avg(), avgEpsilon: Avg = Avg()) {
  override def toString: String = {
    s"\n(̅Πₚ = ${rejectedRatio.round(4)}, ̅Πᵣ = ${waitRation.round(4)}, K̅ = ${avgK.round(4)}, " +
      s"̅θ = ${avgTheta.value.round(4)}, ̅η:  ${avgEta.value.round(4)}, ̅ε: ${avgEpsilon.value.round(4)}, ρ = ${utilization.round(4)}, " +
      s"avgQueue: ${avgQueued.round(4)})"
  }
}

object Prove extends App {

  implicit private val random: Random = new Random(1234)

  // QueuedSystem(ExponentialDistribution(2), ExponentialDistribution(3), m = 1, l = 20).simulate.take(100).foreach(println)

  val λ = 67
  val A0 = 60
  val avgθ: Double = 0.0121
  val μ = Helper.μ(avgθ)
  val system = QueuedSystem(ExponentialDistribution(λ), ExponentialDistribution(μ), m = 1, l = 1000000)

  val take = 1000000
  val asd = system.simulate2.take(take)

  println(asd.last)






  //println("avg k: " + asd.avgProcessing)
  //println("perdita: " + asd.rejected)
  //println("avg time in queue: " + asd.avgTimeInQueue)
}
