package spinal.tester

import spinal.core._

package object scalatest {
  import spinal.tester.SpinalConfig
  val simWorkspacePath = spinal.tester.simWorkspacePath

  def SpinalVerilog[T <: Component](gen: => T): SpinalReport[T] = SpinalConfig().generateVerilog(gen)
  def SpinalVhdl[T <: Component](gen: => T): SpinalReport[T] = SpinalConfig().generateVerilog(gen)
  def SpinalSystemVerilog[T <: Component](gen: => T): SpinalReport[T] = SpinalConfig().generateVerilog(gen)
}
