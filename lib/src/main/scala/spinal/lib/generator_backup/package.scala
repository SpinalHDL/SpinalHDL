package spinal.lib

package object generator_backup {
  implicit class GeneratorSeqPimper(pimped : Seq[Handle[_]]){
    def produce[T](body : => T) : Handle[T] = Dependable(pimped :_*)(body)
  }
}
