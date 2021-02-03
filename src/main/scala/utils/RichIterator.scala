package utils

import java.util.concurrent.locks.Condition


object RichIterator {

  implicit class RichIterators[T](val its: Seq[Iterator[T]]) {
    def merged(implicit ord: Ordering[T]): Iterator[T] = its.reduce((a, b) => a.merge(b))
  }

  implicit class RichIterator[T](val it: Iterator[T]) {
    def last: T = {
      var last: T = it.next()
      while (it.hasNext) {
        last = it.next()
      }
      last
    }

    def merge(other: Iterator[T])(implicit ord: Ordering[T]): Iterator[T] = new Iterator[T] {

      private var cache1: Option[T] = None
      private var cache2: Option[T] = None

      def peek(cache: Option[T], iterator: Iterator[T]): Option[T] = cache match {
        case Some(_) => cache
        case None if iterator.hasNext =>
          Some(iterator.next())
        case _ => None
      }

      override def hasNext: Boolean = cache1.isDefined || cache2.isDefined || it.hasNext || other.hasNext

      override def next(): T = {
        cache1 = peek(cache1, it)
        cache2 = peek(cache2, other)
        (cache1, cache2) match {
          case (Some(v1), Some(v2)) if ord.lteq(v1, v2) => cache1 = None; v1
          case (Some(_), Some(v2)) => cache2 = None; v2
          case (None, Some(v)) => cache2 = None; v
          case (Some(v), None) => cache1 = None; v
        }
      }
    }

    def mergeByTime(other: Iterator[T]): Iterator[T] = new Iterator[T] {
      val queue = new scala.collection.mutable.Queue[T]()

      import java.util.concurrent.locks.ReentrantLock

      val queueLock = new ReentrantLock(true)
      val queueCond: Condition = queueLock.newCondition()

      object HasNext {
        private var count = 0
        private var noCount = 0
        private val lock = new Object()

        def inc(): Unit = lock synchronized {
          count += 1
          lock.notify()
        }

        def dec(): Unit = lock synchronized {
          count -= 1
          lock.notify()
        }

        def no(): Unit = lock synchronized {
          noCount += 1
          lock.notify()
        }

        def response(): Boolean = lock synchronized {
          if (count > 0) true
          else if (noCount >= 2) false
          else {
            lock.wait()
            response()
          }
        }
      }

      def createThread(iterator: Iterator[T]): Thread =
        new Thread(() => {
          while (iterator.hasNext) {
            HasNext.inc()
            val value = iterator.next()
            queueLock.lock()
            queue.enqueue(value)
            queueCond.signal()
            queueLock.unlock()
          }
          HasNext.no()
        })

      createThread(it).start()
      createThread(other).start()

      override def hasNext: Boolean = HasNext.response()

      override def next(): T = {
        queueLock.lock()
        while (queue.isEmpty) {
          queueCond.awaitUninterruptibly()
        }
        HasNext.dec()
        val ret = queue.dequeue()
        queueLock.unlock()
        ret
      }
    }
  }

}
