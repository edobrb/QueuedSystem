package utils

import scala.reflect.ClassTag

class FastFixedOrderedQueue[T: ClassTag](maxSize: Int, compare: (T, T) => Double) {

  private var size: Int = 0
  private var start: Int = 0
  private val array: Array[T] = new Array[T](maxSize)


  def dequeue(): Option[T] = if (isEmpty) None else {
    val r = Some(array(start))
    start = (start + 1) % maxSize
    size = size - 1
    r
  }

  def peek(): Option[T] = if (isEmpty) None else Some(array(start))

  def enqueue(t: T): Boolean = if (isFull) {
      false
    } else {
      var pending = t
      (0 until size) map (i => index(i)) foreach (i => {
        val e = array(i)
        if (compare(e, pending) > 0) {
          val tmp = e
          array(i) = pending
          pending = tmp
        }
      })
      array(index(size)) = pending
      size = size + 1
      true
    }


  private def index(i: Int) = (i + start) % maxSize

  def isEmpty: Boolean = size == 0

  def isFull: Boolean = size == maxSize

}

object Test2 extends App {
  val q = new FastFixedOrderedQueue[Int](4, _ - _)

  assert(q.dequeue().isEmpty)
  assert(q.peek().isEmpty)
  assert(q.isEmpty)
  assert(!q.isFull)
  assert(q.enqueue(100))
  assert(!q.isEmpty)
  assert(!q.isFull)
  assert(q.peek().get == 100)
  assert(q.enqueue(50))
  assert(q.peek().get == 50)
  assert(q.enqueue(75))
  assert(q.peek().get == 50)
  assert(q.enqueue(125))
  assert(!q.isEmpty)
  assert(q.isFull)
  assert(q.dequeue().get == 50)
  assert(q.dequeue().get == 75)
  assert(q.dequeue().get == 100)
  assert(q.dequeue().get == 125)
  assert(q.isEmpty)
  assert(!q.isFull)
}