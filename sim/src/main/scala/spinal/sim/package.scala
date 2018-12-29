package spinal

import scala.annotation.StaticAnnotation
import scala.collection.mutable.ArrayBuffer

package object sim {
  import scala.collection.IterableLike
  class suspendable extends StaticAnnotation
  implicit class cpsIterable[A, Repr](xs: IterableLike[A, Repr]) {
    @deprecated("You can now use regular scala collections utilities as continuation aren't used anymore.", "1.3.0")
    def suspendable = new {
      def foreach[B](f: A => Any): Unit = {
        val it = xs.iterator
        while(it.hasNext) f(it.next)
      }

      def map[R](f: A => R): Seq[R] ={
        val i = xs.iterator
        val ret = ArrayBuffer[R]()
        while(i.hasNext){
          ret += f(i.next())
        }
        ret
      }
    }
  }


  @deprecated("You can now use regular scala collections utilities as continuation aren't used anymore.", "1.3.0")
  def repeatSim(times : Long)(body : => Unit): Unit ={
    var idx = 0l
    while(idx != times){
      idx += 1
      body
    }
  }
}
