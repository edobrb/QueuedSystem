package model

import java.util.Random

import distributions.EventGenerator._
import distributions.EventsGenerator._
import model.QueueEvents._
import utils.RichIterator._
import utils.{BinaryHeap, FastFixedQueue}

trait QueuedSystemAnalyzer extends QueuedSystemStructure {

  def eventsSimulation: Iterator[QueueEvent]

  def statesSimulation: Iterator[QueuedSystemState] =
    eventsSimulation.scanLeft[QueuedSystemState](QueuedSystemState(this)) { case (oldState, event) =>
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

      val processingFull = if (oldState.system.l > 0 && (oldState.processing == oldState.system.m || oldState.queued > 0)) 1.0 else 0.0
      val newEnqueuedRatio = (oldState.stats.waitRatio * oldState.t + processingFull * dt) / event.t //enqueue ratio ̅Πᵣ
      //or if(newState.computed == 0) 0 else newState.totalDequeued.toDouble / newState.computed

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


  def served: TimeEventsGenerator = new TimeEventsGenerator {
    override def events: Iterator[Double] = eventsSimulation.collect {
      case LeftEvent(t, _, _, _) => t
    }
  }

  def rejected: TimeEventsGenerator = new TimeEventsGenerator {
    override def events: Iterator[Double] = eventsSimulation.collect {
      case RejectedEvent(t, _) => t
    }
  }
}

trait QueuedSystemStructure {
  def m: Int

  def l: Int
}

case class PrioritizedQueuedSystem(insEvents: Seq[TimeEventsGenerator], servicesDurations: Seq[TimeDurationGenerator], m: Int, l: Int)(implicit random: Random) extends QueuedSystemAnalyzer {

  def prioritizedEventsSimulation: Iterator[(Int, QueueEvent)] = {
    new Iterator[(Int, QueueEvent)] {

      case class Item(index: Int, enterTime: Double, queueExitTime: Double, exitTime: Double, id: Long)

      var id: Long = 0
      var cachedNext: Item = _
      val enterStream: Iterator[(Int, Double)] = insEvents.map(_.events).zipWithIndex.map(v => v._1.map(t => (v._2, t)))
        .merged((x: (Int, Double), y: (Int, Double)) => (x._2, y._2) match {
          case (x, y) if x > y => 1
          case (x, y) if x < y => -1
          case _ => 0
        })
      var inside = 0
      val items: BinaryHeap[Item] = new BinaryHeap[Item]((x: Item, y: Item) => if (x.exitTime - y.exitTime > 0) 1 else -1)
      val enqueued: Seq[FastFixedQueue[Item]] = Seq.fill(insEvents.size)(new FastFixedQueue[Item](l))
      var lastDequeued: Item = _


      def getNext: Item = {
        if (cachedNext == null) {
          val (index, enterTime) = enterStream.next()
          val leftTime = enterTime + servicesDurations(index).element
          id = id + 1
          Item(index, enterTime, enterTime, leftTime, id)
        } else {
          val tmp = cachedNext
          cachedNext = null
          tmp
        }
      }

      def peekNext(): Item = {
        if (cachedNext == null) {
          cachedNext = getNext
        }
        cachedNext
      }

      def newEnterItem(): (Int, EnterEvent) = {
        val item = getNext
        items.enqueue(item)
        inside = inside + 1
        (item.index, EnterEvent(item.enterTime, item.id))
      }

      override def hasNext: Boolean = true

      override def next(): (Int, QueueEvent) = {
        if (lastDequeued != null) {
          val tmp = lastDequeued
          lastDequeued = null
          (tmp.index, DequeuedEvent(tmp.queueExitTime, tmp.id, tmp.queueExitTime - tmp.enterTime))
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
                val item = getNext
                enqueued(item.index).enqueue(item)
                inside = inside + 1
                (item.index, EnqueuedEvent(item.enterTime, item.id))
              } else if (next.enterTime < nextExiting.exitTime) { //will be rejected
                val item = getNext
                (item.index, RejectedEvent(item.enterTime, item.id))
              } else { //next will left
                items.dequeue()
                inside = inside - 1
                val firstNonEmptyQueue = enqueued.find(!_.isEmpty)
                if (firstNonEmptyQueue.isDefined) {
                  val dequeued = firstNonEmptyQueue.get.dequeue().get
                  lastDequeued = Item(dequeued.index, dequeued.enterTime, nextExiting.exitTime, nextExiting.exitTime + servicesDurations(dequeued.index).element, dequeued.id)
                  items.enqueue(lastDequeued)
                }
                (nextExiting.index, LeftEvent(nextExiting.exitTime, nextExiting.id, nextExiting.exitTime - nextExiting.queueExitTime, nextExiting.queueExitTime - nextExiting.enterTime))
              }
          }
        }
      }
    }
  }

  override def eventsSimulation: Iterator[QueueEvent] = prioritizedEventsSimulation.map(_._2)

  def getTraffic(index: Int): QueuedSystemAnalyzer = {
    val _this = this
    new QueuedSystemAnalyzer {
      override def eventsSimulation: Iterator[QueueEvent] = _this.prioritizedEventsSimulation.filter(_._1 == index).map(_._2)

      override def m: Int = _this.m

      override def l: Int = _this.l
    }
  }
}

case class QueuedSystem(inEvents: TimeEventsGenerator, serviceDurations: TimeDurationGenerator, m: Int, l: Int)(implicit random: Random) extends QueuedSystemAnalyzer {

  private val system = PrioritizedQueuedSystem(Seq(inEvents), Seq(serviceDurations), m, l)

  def eventsSimulation: Iterator[QueueEvent] = system.eventsSimulation
}
