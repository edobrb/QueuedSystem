package utils

object RichIterator {

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
  }

}
