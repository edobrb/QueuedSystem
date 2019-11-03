package distributions
import utils.RichIterator._

object EventsGenerator {
  def apply(eventsGenerator: Iterator[Double]): TimeEventsGenerator = new TimeEventsGenerator {
    override def events: Iterator[Double] = eventsGenerator
  }

  type TimeEventsGenerator = EventsGenerator[Double]

  type TimeDurationGenerator = EventGenerator[Double]

}

trait EventsGenerator[T] {
  private val _this = this

  def events: Iterator[T]

  def +(other: EventsGenerator[T])(implicit ord: Ordering[T]): EventsGenerator[T] = new EventsGenerator[T] {
    override def events: Iterator[T] = _this.events merge other.events
  }
}
trait EventGenerator[T] {
  def element: T
}

