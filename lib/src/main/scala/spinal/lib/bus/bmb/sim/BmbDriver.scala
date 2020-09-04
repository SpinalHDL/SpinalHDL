package spinal.lib.bus.bmb.sim

import spinal.core._
import spinal.core.sim._
import spinal.lib.bus.bmb.Bmb
import spinal.sim.{SimManagerContext, SimThread}

import scala.collection.mutable



case class BmbDriver(ctrl : Bmb, cd : ClockDomain) {
  ctrl.cmd.valid #= false
  ctrl.rsp.ready #= true

  val mutex = SimMutex()

  def write(data : BigInt, address : BigInt): Unit ={
    mutex.lock()
    ctrl.cmd.valid #= true
    ctrl.cmd.address #= address
    ctrl.cmd.data #= data
    ctrl.cmd.mask #= (BigInt(1) << ctrl.p.access.byteCount)-1
    ctrl.cmd.opcode #= Bmb.Cmd.Opcode.WRITE
    cd.waitSamplingWhere(ctrl.cmd.ready.toBoolean)
    ctrl.cmd.valid #= false
    cd.waitSamplingWhere(ctrl.rsp.valid.toBoolean)
    mutex.unlock()
  }

  def read(address : BigInt): BigInt ={
    mutex.lock()
    ctrl.cmd.valid #= true
    ctrl.cmd.address #= address
    ctrl.cmd.opcode #= Bmb.Cmd.Opcode.READ
    cd.waitSamplingWhere(ctrl.cmd.ready.toBoolean)
    ctrl.cmd.valid #= false
    cd.waitSamplingWhere(ctrl.rsp.valid.toBoolean)
    mutex.unlock()
    ctrl.rsp.data.toBigInt
  }
}
