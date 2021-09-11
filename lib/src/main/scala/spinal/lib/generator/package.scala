package spinal.lib

import spinal.core.ClockDomain
import spinal.core.fiber._

package object generator {
//  type Handle[T] = spinal.core.fiber.Handle[T]

  implicit class GeneratorSeqPimper(pimped : Seq[Handle[_]]){
    //TODO fiber
    def produce[T](body : => T) : Handle[T] = hardFork{pimped.foreach(_.get); body}
  }

  implicit class HandleClockDomainPimper(pimped : Handle[ClockDomain]) {
    def apply[T](block: => T): T = {
      ClockDomain.push(pimped)
      val ret: T = block
      ClockDomain.pop()
      ret
    }

    def on[T](block: => T): T = apply(block)
  }
}
