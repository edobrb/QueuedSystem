
trait Jacobson {

  def alpha: Double

  def beta: Double

  def sRTT(i: Int): Double

  def eRTT(i: Int): Double = i match {
    case 0 => sRTT(0)
    case _ => (1 - alpha) * eRTT(i - 1) + alpha * sRTT(i)
  }

  def devRTT(i: Int): Double = i match {
    case 0 => sRTT(0) / 2
    case _ => (1 - beta) * devRTT(i - 1) + beta * math.abs(eRTT(i - 1) - sRTT(i))
  }

  def G:Double
  def LB:Double
  def RTO(i:Int):Double = math.max(LB,eRTT(i)+math.max(G, 4*devRTT(i)))
}

object JacobsonAlg extends App {

  implicit class RichDouble(n: Double) {
    def roundAt(p: Int): Double = {
      val s = math pow(10, p); (math round n * s) / s
    }
  }

  val data = new Jacobson {
    override def alpha: Double = 0.2

    override def beta: Double = 0.3
    def G:Double = 0.03
    def LB:Double = 0.08
    override def sRTT(i: Int): Double = i match {
      case 0 => Double.NaN
      case 1 => 0.12
      case 2 => 0.16
      case 3 => 0.10
      case 4 => 0.09
      case 5 => 0.13
    }

    override def devRTT(i: Int): Double = i match {
      case 0 => 0.01
      case _ => super.devRTT(i)
    }

    override def eRTT(i: Int): Double = i match {
      case 0 => 0.1
      case _ => super.eRTT(i)
    }
  }

  (0 to 5) foreach (i => {
    println(s"i: $i, RTO: ${data.RTO(i).roundAt(3)}, sRTT: ${data.sRTT(i).roundAt(2)}, eRTT: ${data.eRTT(i).roundAt(4)}, devRTT: ${data.devRTT(i).roundAt(4)}")
  })
}
