package utils

case class Avg(sum: Double = 0, count: Long = 0) {
  def add(element: Double): Avg = Avg(sum + element, count + 1)

  def value: Double = sum / count

  override def toString: String = if (count == 0) "NaN" else value.toString
}
