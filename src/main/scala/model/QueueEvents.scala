package model


trait QueueEvent {
  def t: Double

  def id: Long
}

object QueueEvents {
  //Terminated service in time t, enqueued in time t - (processTime + queuedTime) if queuedTime > 0, entered in time t - processTime
  case class LeftEvent(override val t: Double, override val id: Long, processTime: Double, queuedTime: Double) extends QueueEvent

  case class EnterEvent(override val t: Double, override val id: Long) extends QueueEvent //Already in service at time t

  case class RejectedEvent(override val t: Double, override val id: Long) extends QueueEvent //Rejected in time t

  case class EnqueuedEvent(override val t: Double, override val id: Long) extends QueueEvent //Enqueued in time t

  //Dequeued in time t and entered in time t - queuedTime
  case class DequeuedEvent(override val t: Double, override val id: Long, queuedTime: Double) extends QueueEvent
}