package spinal.lib.misc

package object pipeline {
  type Payload[T <: spinal.core.Data] = spinal.core.NamedType[T]
  val Payload = spinal.core.NamedType
}
