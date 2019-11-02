package distributions

object EventsGenerator {
  def apply(eventsGenerator: LazyList[Double]): TimeEventsGenerator = new TimeEventsGenerator {
    override def events: LazyList[Double] = eventsGenerator
  }
}

trait EventsGenerator[T] {
  def events: LazyList[T]
}
trait EventGenerator[T] {
  def element:T
}

trait TimeEventsGenerator extends EventsGenerator[Double]
trait TimeEventGenerator extends EventGenerator[Double]