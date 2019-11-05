package distributions

import java.util.Random

object EventGenerator {
  type TimeDurationGenerator = EventGenerator[Double]

  implicit def distributionToTimeDurationGenerator(d: Distribution)(implicit random: Random): TimeDurationGenerator =
    new TimeDurationGenerator {
      override def element: Double = d.F_inv(random.nextDouble())
    }
}

trait EventGenerator[T] {
  def element: T
}