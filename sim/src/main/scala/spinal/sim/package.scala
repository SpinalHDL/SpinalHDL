package spinal

import scala.collection.mutable.ArrayBuffer
import scala.util.continuations.suspendable

package object sim {
  implicit class PimperA[T](pimped : TraversableOnce[T]){
    def foreachSim[U](f: T => U@suspendable): Unit@suspendable ={
      val i = pimped.toIterator
      while(i.hasNext){
        f(i.next())
      }
    }

    def mapSim[R](f: T => R@suspendable): Seq[R]@suspendable ={
      val i = pimped.toIterator
      val ret = ArrayBuffer[R]()
      while(i.hasNext){
        ret += f(i.next())
      }
      ret
    }
  }

  implicit class PimperB(pimped : Range){
    def doSim[U](f:  => U@suspendable): Unit@suspendable ={
      val i = pimped.toIterator
      while(i.hasNext){
        i.next()
        f
      }
    }
  }
}
