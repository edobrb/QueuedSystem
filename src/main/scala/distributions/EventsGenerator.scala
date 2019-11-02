package distributions

object EventsGenerator {
  def apply(eventsGenerator: Iterable[Double]): TimeEventsGenerator = new TimeEventsGenerator {
    override def events: Iterable[Double] = eventsGenerator
  }
}

trait EventsGenerator[T] {
  def events: Iterable[T]
}
trait EventGenerator[T] {
  def element: T
}

trait TimeEventsGenerator extends EventsGenerator[Double]
trait TimeEventGenerator extends EventGenerator[Double]