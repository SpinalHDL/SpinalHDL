package spinal.lib.misc

/** Allows to write pipelines more easily and with better parametrization.
  * 
  * @see [[https://spinalhdl.github.io/SpinalDoc-RTD/master/SpinalHDL/Libraries/Pipeline/introduction.html#introduction]]
  */
package object pipeline {
  type Payload[T <: spinal.core.Data] = spinal.core.NamedType[T]
  val Payload = spinal.core.NamedType
}
