package spinal.lib.misc

package object pipeline {
  type SignalKey[T <: spinal.core.Data] = spinal.core.NamedType[T]
  val SignalKey = spinal.core.NamedType
}
