package spinal.lib.bus.bmb

import spinal.core._
import spinal.lib.bus.misc.{AddressMapping, DefaultMapping, SizeMapping}
import spinal.lib._


//TODO no rspNoHit logic when there is a default
//TODO optimized rspNoHit counter depending BMB parameters
case class BmbDecoder(p : BmbParameter,
                      mappings : Seq[AddressMapping],
                      capabilities : Seq[BmbParameter],
                      pendingMax : Int = 15) extends Component{
  val io = new Bundle {
    val input = slave(Bmb(p))
    val outputs = Vec(master(Bmb(p)), mappings.size)
  }
  val hasDefault = mappings.contains(DefaultMapping)
  val logic = if(hasDefault && mappings.size == 1 && !(p.canWrite && !capabilities.head.canWrite) && !(p.canRead && !capabilities.head.canRead)){
    io.outputs(0) << io.input
  } else new Area {
    val hits = Vec(Bool, mappings.size)
    for (portId <- 0 until mappings.length) yield {
      val slaveBus = io.outputs(portId)
      val memorySpace = mappings(portId)
      val capability = capabilities(portId)
      val hit = hits(portId)
      hit := (memorySpace match {
        case DefaultMapping => !hits.filterNot(_ == hit).orR
        case _ => memorySpace.hit(io.input.cmd.address)
      })
      if(!capability.canWrite) hit clearWhen(io.input.cmd.isWrite)
      if(!capability.canRead) hit clearWhen(io.input.cmd.isRead)
      slaveBus.cmd.valid := io.input.cmd.valid && hit
      slaveBus.cmd.payload := io.input.cmd.payload.resized
    }
    val noHit = if (!hasDefault) !hits.orR else False
    io.input.cmd.ready := (hits, io.outputs).zipped.map(_ && _.cmd.ready).orR || noHit

    val rspPendingCounter = Reg(UInt(log2Up(pendingMax + 1) bits)) init(0)
    rspPendingCounter := rspPendingCounter + U(io.input.cmd.lastFire) - U(io.input.rsp.lastFire)
    val cmdWait = Bool()
    val rspHits = RegNextWhen(hits, io.input.cmd.valid && !cmdWait)
    val rspPending = rspPendingCounter =/= 0
    val rspNoHitValid = if (!hasDefault) !rspHits.orR else False
    val rspNoHit = !hasDefault generate new Area{
      val doIt = RegInit(False) clearWhen(io.input.rsp.lastFire) setWhen(io.input.cmd.fire && noHit && io.input.cmd.last)
      val singleBeatRsp = if(p.canRead) RegNextWhen(io.input.cmd.isWrite, io.input.cmd.fire) else True
      val source = RegNextWhen(io.input.cmd.source, io.input.cmd.fire)
      val context = RegNextWhen(io.input.cmd.context, io.input.cmd.fire)
      val counter = p.canRead generate RegNextWhen(io.input.cmd.transferBeatCountMinusOne, io.input.cmd.fire)
    }

    io.input.rsp.valid := io.outputs.map(_.rsp.valid).orR || (rspPending && rspNoHitValid)
    io.input.rsp.payload := io.outputs.map(_.rsp.payload).read(OHToUInt(rspHits))
    if(!hasDefault) when(rspNoHit.doIt) {
      io.input.rsp.valid := True
      io.input.rsp.setError()
      io.input.rsp.source := rspNoHit.source
      io.input.rsp.context := rspNoHit.context

      //Manage io.input.rsp.last generation for multiple generation cases to save area
      if(!p.alignment.allowByte && (1 << p.lengthWidth) <= p.byteCount){
        io.input.rsp.last := True
      } else {
        io.input.rsp.last := False
        if (p.canRead) {
          io.input.rsp.last setWhen (rspNoHit.counter === 0)
          when(io.input.rsp.fire) {
            rspNoHit.counter := rspNoHit.counter - 1
          }
        }
        if (p.canWrite) {
          io.input.rsp.last setWhen (rspNoHit.singleBeatRsp)
        }
      }
    }
    for(output <- io.outputs) output.rsp.ready := io.input.rsp.ready

    cmdWait := (rspPending && (hits =/= rspHits || rspNoHitValid)) || rspPendingCounter === pendingMax
    when(cmdWait) {
      io.input.cmd.ready := False
      io.outputs.foreach(_.cmd.valid := False)
    }
  }
}
