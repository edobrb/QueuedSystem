package utils

import scala.reflect.ClassTag

class FastFixedQueue[T:ClassTag](maxSize: Int) {

  private var size: Int = 0
  private var start: Int = 0
  private val array: Array[T] = new Array[T](maxSize)


  def dequeue(): Option[T] = if(isEmpty) None else {
    val r = Some(array(start))
    start = (start + 1) % maxSize
    size = size - 1
    r
  }

  def peek(): Option[T] = if(isEmpty) None else Some(array(start))

  def enqueue(t: T): Boolean = {
    if (isFull) {
      false
    } else {
      array((size + start) % maxSize) = t
      size = size + 1
      true
    }
  }

  def isEmpty: Boolean = size == 0

  def isFull: Boolean = size == maxSize
}

object Test extends App {

  val q = new FastFixedQueue[Int](3)

  (0 until 100).foreach( _ => {
    assert(q.dequeue().isEmpty)
    assert(q.peek().isEmpty)
    assert(q.isEmpty)
    assert(!q.isFull)
    assert(q.enqueue(100))
    assert(!q.isEmpty)
    assert(!q.isFull)
    assert(q.enqueue(200))
    assert(!q.isEmpty)
    assert(!q.isFull)
    assert(q.enqueue(300))
    assert(!q.isEmpty)
    assert(q.isFull)
    assert(!q.enqueue(300))
    assert(q.peek().get == 100)
    assert(q.dequeue().get == 100)
    assert(q.dequeue().get == 200)
    assert(q.dequeue().get == 300)
    assert(q.dequeue().isEmpty)
  })

}