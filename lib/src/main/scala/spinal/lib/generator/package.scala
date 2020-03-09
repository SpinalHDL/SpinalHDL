package spinal.lib

package object generator {
  implicit class GeneratorSeqPimper(pimped : Seq[Handle[_]]){
    def produce[T](body : => T) : Handle[T] = Dependable(pimped)(body)
  }
}
