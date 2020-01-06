package utils

import scala.collection.mutable.ListBuffer
import scala.reflect.ClassTag

class BinaryHeap[T: ClassTag](ordering: Ordering[T]){


  private var root: Node = null

  class Node(val t: T) {
    val childs: ListBuffer[Node] = ListBuffer[Node]()
  }


  def dequeue(): Option[T] =  if(isEmpty) None else {
    val res = Some(root.t)


    if(root.childs.isEmpty) {
      root = null
    } else {
      val min = root.childs.minBy(_.t)(ordering)
      val childs = root.childs.filter(_ != min)
      root = min
      root.childs.appendAll(childs)
    }

    res
  }

  def peek(): Option[T] = if(isEmpty) None else Some(root.t)

  def enqueue(t: T): Boolean = if (root == null) {
    root = new Node(t)
    true
  } else {
    insert(t)
    true
  }

  private def insert(t: T):Unit = {
    if (ordering.lt(t, root.t)) {
      val tmp = root
      root = new Node(t)
      root.childs.addOne(tmp)
    } else {
      root.childs.addOne(new Node(t))
    }
  }

  def isEmpty: Boolean = root == null

}

object Test2 extends App {
  val q = new BinaryHeap[Int](???)

  assert(q.dequeue().isEmpty)
  assert(q.peek().isEmpty)
  assert(q.isEmpty)
  assert(q.enqueue(100))
  assert(!q.isEmpty)
  assert(q.peek().get == 100)
  assert(q.enqueue(50))
  assert(q.peek().get == 50)
  assert(q.enqueue(75))
  assert(q.peek().get == 50)
  assert(q.enqueue(125))
  assert(!q.isEmpty)
  assert(q.dequeue().get == 50)
  assert(q.dequeue().get == 75)
  assert(q.dequeue().get == 100)
  assert(q.dequeue().get == 125)
  assert(q.isEmpty)
}