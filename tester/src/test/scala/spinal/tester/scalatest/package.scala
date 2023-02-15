package spinal.tester

import spinal.core._

package object scalatest {
  val simWorkspacePath = spinal.tester.simWorkspacePath
  def SpinalConfig() = spinal.core.SpinalConfig(targetDirectory = simWorkspacePath)
  def SpinalConfig(defaultClockDomainFrequency: IClockDomainFrequency = UnknownFrequency()) = spinal.core.SpinalConfig(
    defaultClockDomainFrequency = defaultClockDomainFrequency,
    targetDirectory = simWorkspacePath
  )

  def SpinalVerilog[T <: Component](gen: => T): SpinalReport[T] = SpinalConfig().generateVerilog(gen)
  def SpinalVhdl[T <: Component](gen: => T): SpinalReport[T] = SpinalConfig().generateVerilog(gen)
  def SpinalSystemVerilog[T <: Component](gen: => T): SpinalReport[T] = SpinalConfig().generateVerilog(gen)
}
