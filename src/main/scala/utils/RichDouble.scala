package utils

import scala.math.pow

object RichDouble {
  implicit class RichDouble(d: Double) {
    def round(decimals: Int): Double = {
      math.round(d * pow(10, decimals)) / pow(10, decimals)
    }
    def between(inf:Double) = new {
      def and(sup:Double):Boolean = d > inf && d < sup
    }
  }
}
