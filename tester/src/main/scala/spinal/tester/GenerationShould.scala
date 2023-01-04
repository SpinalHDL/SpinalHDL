package spinal.tester

import spinal.core._

object GenerationShould {
  private def fails(f: => Unit): Boolean = {
    var failed = false
    try { f }
    catch {
      case e: Throwable => failed = true
    }
    failed
  }

  def fail(gen: => Component): Unit = {
    assert(fails { SpinalVhdl(gen) })
    assert(fails { SpinalVerilog(gen) })
  }

  def pass(gen: => Component): Unit = {
    assert(!fails { SpinalVhdl(gen) })
    assert(!fails { SpinalVerilog(gen) })
  }
}
