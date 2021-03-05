package spinal.lib

import spinal.core.fiber._

package object generator {
//  type Handle[T] = spinal.core.fiber.Handle[T]

  implicit class GeneratorSeqPimper(pimped : Seq[Handle[_]]){
    //TODO fiber
    def produce[T](body : => T) : Handle[T] = hardFork(body)
  }
}
