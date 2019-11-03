package model

import java.util.Random

import distributions.EventsGenerator._
import model.QueueEvents._
import utils.{FastFixedOrderedQueue, FastFixedQueue}


case class QueuedSystem(inEvents: TimeEventsGenerator, outDurations: TimeDurationGenerator, m: Int, l: Int)(implicit random: Random) {

  def eventsSimulation: Iterator[QueueEvent] = {
    new Iterator[QueueEvent] {
      case class Item(enterTime: Double, queueExitTime: Double, exitTime: Double, id: Long)

      var id: Long = 0
      var cachedNext: Item = _
      var enterStream: Iterator[Double] = inEvents.events
      var inside = 0
      var items: FastFixedOrderedQueue[Item] = new FastFixedOrderedQueue[Item](m, (i1, i2) => i1.exitTime - i2.exitTime)
      var enqueued: FastFixedQueue[Item] = new FastFixedQueue[Item](l)
      var lastDequeued: Item = _


      def getNext(): Item = {
        if (cachedNext == null) {
          val enter:Double = enterStream.next()
          val left:Double = enter + outDurations.element
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
                  lastDequeued = Item(queueHead.get.enterTime, nextExiting.exitTime, nextExiting.exitTime + outDurations.element, queueHead.get.id)
                  items enqueue lastDequeued
                }
                LeftEvent(nextExiting.exitTime, nextExiting.id, nextExiting.exitTime - nextExiting.queueExitTime, nextExiting.queueExitTime - nextExiting.enterTime)
              }
          }
        }
      }
    }
  }


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


  def output: TimeEventsGenerator = new TimeEventsGenerator {
    override def events: Iterator[Double] = eventsSimulation.collect {
      case LeftEvent(t, _, _, _) => t
    }
  }
}
