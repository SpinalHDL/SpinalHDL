package spinal.lib.bus.bmb

import spinal.core._
import spinal.lib._

object BmbExclusiveMonitor{
  def outputParameter(inputParameter : BmbParameter) = inputParameter.copy(canExclusive = false, contextWidth = inputParameter.contextWidth + 1)
}

//Only LENGTH aligned access for now
//Ensure exclusive ordering across sources by waiting on conflicts
case class BmbExclusiveMonitor(inputParameter : BmbParameter) {
  assert(inputParameter.alignment == BmbParameter.BurstAlignement.LENGTH)
  val io = new Bundle {
    val input = slave(Bmb(inputParameter))
    val output = master(Bmb(BmbExclusiveMonitor.outputParameter(inputParameter)))
  }

  val inputAddress = io.input.cmd.address & ~io.input.cmd.length.maxValue //Pessimistic tracking
  val sources = for(sourceId <- 0 until 1 << inputParameter.sourceWidth) yield new Area{
    val valid, lock = RegInit(False)
    val address = Reg(UInt(inputParameter.addressWidth bits))
    val addressHit = address === inputAddress
    val sourceHit = io.input.cmd.source === sourceId
    val lockHit = lock && addressHit && !sourceHit

    when(io.output.rsp.fire && io.output.rsp.source === sourceId && io.output.rsp.context.msb){
      lock := False
    }

    when(io.input.cmd.valid && io.input.cmd.isRead && io.input.cmd.exclusive && sourceHit){
      valid := True
      address := inputAddress
    }
    when(addressHit && io.input.cmd.lastFire && io.input.cmd.isWrite){
      valid := False
      when(sourceHit){
        lock := True
      }
    }
  }

  val exclusiveSuccess = sources.map(s => s.valid && s.addressHit).read(io.input.cmd.source)
  val lockHit = sources.map(_.lockHit).orR
  val lockSource = sources.map(s => s.lock).read(io.input.cmd.source)
  io.output.cmd.arbitrationFrom(io.input.cmd.haltWhen(lockHit || lockSource))
  io.output.cmd.payload.assignSomeByName(io.input.cmd.payload)
  io.output.cmd.context.removeAssignments() := (io.input.cmd.exclusive && exclusiveSuccess) ## io.input.cmd.context
  when(io.input.cmd.exclusive && !exclusiveSuccess){
    io.output.cmd.mask := 0
  }

  io.input.rsp.arbitrationFrom(io.output.rsp)
  io.input.rsp.payload.assignSomeByName(io.output.rsp.payload)
  io.input.rsp.context.removeAssignments() := io.output.rsp.context.resized
  io.input.rsp.exclusive := io.output.rsp.context.msb
}
