package distributions

import java.util.Random

import distributions.EventGenerator._
import utils.RichIterator._

object EventsGenerator {
  def apply(eventsGenerator: Iterator[Double]): TimeEventsGenerator = new TimeEventsGenerator {
    override def events: Iterator[Double] = eventsGenerator
  }

  type TimeEventsGenerator = EventsGenerator[Double]

  implicit def distributionToTimeEventsGenerator(d: Distribution)(implicit random: Random): TimeEventsGenerator =
    new TimeEventsGenerator {
      override def events: Iterator[Double] = Iterator.continually(d.element).scanLeft(d.element)(_ + _)
    }
}

trait EventsGenerator[T] {
  private val _this = this

  def events: Iterator[T]

  def +(other: EventsGenerator[T])(implicit ord: Ordering[T]): EventsGenerator[T] = new EventsGenerator[T] {
    override def events: Iterator[T] = _this.events merge other.events
  }
}



