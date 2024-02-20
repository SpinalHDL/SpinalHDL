package spinal.lib.cpu.riscv.debug

import spinal.core._
import spinal.core.fiber._
import spinal.lib.cpu.riscv.RiscvHart
import spinal.lib.cpu.riscv.debug._

import scala.collection.mutable.ArrayBuffer

class DebugModuleFiber() extends Area{
  var harts = ArrayBuffer[RiscvHart]()
  def bindHart(cpu: RiscvHart) = {
    harts += cpu
  }

  def ndmreset = thread.logic.io.ndmreset

  val thread = Fiber build new Area{
    val p = DebugTransportModuleParameter(
      addressWidth = 7,
      version = 1,
      idle = 7
    )

    val logic = DebugModule(
      DebugModuleParameter(
        version = p.version + 1,
        harts = harts.size,
        progBufSize = 2,
        datacount = harts.map(_.getXlen()).max/32,
        hartsConfig = List(DebugModuleCpuConfig(
          xlen = harts.map(_.getXlen()).max,
          flen = harts.map(_.getFlen()).max,
          withFpuRegAccess = false
        ))
      )
    )

    for(i <- harts.indices){
      val from = logic.io.harts(i)
      val to = harts(i).getDebugBus
      from <> to
      to.dmToHart.removeAssignments() <-< from.dmToHart
    }
  }

  def withJtagTap() = Fiber build new Area {
    val logic = DebugTransportModuleJtagTap(
      thread.p,
      debugCd = thread.logic.clockDomain
    )
    thread.logic.io.ctrl <> logic.io.bus
    val jtag = logic.io.jtag.toIo
  }

  def withJtagInstruction() = Fiber build new Area {
    val logic = DebugTransportModuleTunneled(
      p = thread.p,
      jtagCd = ClockDomain.current,
      debugCd = thread.logic.clockDomain
    )
    thread.logic.io.ctrl <> logic.io.bus
    val instruction = logic.io.instruction.toIo
  }
}
