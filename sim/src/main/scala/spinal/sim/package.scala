package spinal

import scala.util.continuations.suspendable

package object sim {
  implicit class PimperA[T](pimped : TraversableOnce[T]){
    def foreachSim[U](f: T => U@suspendable): Unit@suspendable ={
      val i = pimped.toIterator
      while(i.hasNext){
        f(i.next())
      }
    }
  }
}
