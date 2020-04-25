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
  assert(!AddressMapping.verifyOverlapping(mappings), "BMB address decoding overlapping")

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


case class BmbDecoderOutOfOrder(p : BmbParameter,
                                mappings : Seq[AddressMapping],
                                capabilities : Seq[BmbParameter],
                                pendingRspMax : Int) extends Component{
  assert(!AddressMapping.verifyOverlapping(mappings), "BMB address decoding overlapping")
  assert(isPow2(pendingRspMax))

  val io = new Bundle {
    val input = slave(Bmb(p))
    val outputs = Vec(master(Bmb(p)), mappings.size)
  }
  val sourceCount = 1 << p.sourceWidth
  val portCount = mappings.size

  case class SourceHistory() extends Bundle {
    val outputId = UInt(log2Up(mappings.size) bits)
    val beatCount = UInt(p.beatCounterWidth bits)
    //    val context = Bits(p.contextWidth bits)
  }

  val sourceOrderingFifo = StreamFifoMultiChannel(SourceHistory(), channelCount = sourceCount, depth = pendingRspMax)
  val sourceOrderingUnbuffered = sourceOrderingFifo.io.pop.toStreams(withCombinatorialBuffer = true).unsetName()
  val sourceOrdering = sourceOrderingUnbuffered.map(_.m2sPipe())

  val cmdToRspCountMinusOne = io.input.cmd.isRead ? io.input.cmd.transferBeatCountMinusOne | 0

  val portsLogic = for ((port, portId) <- io.outputs.zipWithIndex) yield new Area {
    val rspFifo = StreamFifoMultiChannel(Fragment(BmbRsp(p)), channelCount = sourceCount, depth = pendingRspMax)
    rspFifo.io.push.stream.valid := port.rsp.valid
    rspFifo.io.push.stream.payload := port.rsp.payload
    rspFifo.io.push.channel := port.rsp.source
    port.rsp.ready := True
    assert(!(rspFifo.io.push.stream.isStall))

    val sourceHits = B(for (sourceId <- 0 until sourceCount) yield !rspFifo.io.pop.empty(sourceId) && sourceOrdering(sourceId).valid && sourceOrdering(sourceId).outputId === portId)
    val sourceHit = sourceHits.orR
    val sourceArbiter = B(OHMasking.first(sourceHits)) //TODO
    val lockValid = RegInit(False)
    val lockSel = Reg(Bits(sourceCount bits))
    val sourceSel = lockValid ? lockSel | sourceArbiter

    when(io.input.rsp.valid){
      lockValid := True
      lockSel := sourceSel
      when(io.input.rsp.ready && io.input.rsp.last){
        lockValid := False
      }
    }
    rspFifo.io.pop.channel := sourceSel


    val incomingRspCount = Reg(UInt(log2Up(pendingRspMax) + 1 bits)) init(2 + p.transferBeatCount) //Init 2 to compensate rspFifo availability latency in a pessimistic way
    val incomingRspAdd = port.cmd.lastFire ? ((U"0" @@ cmdToRspCountMinusOne) + 1) | 0
    incomingRspCount := incomingRspCount + incomingRspAdd - U(port.rsp.fire)

//    val rspFifoFull = cmdToRspCountMinusOne + incomingRspCount >= RegNext(rspFifo.io.availability)
    val rspFifoFull = incomingRspCount >= RegNext(rspFifo.io.availability) //Pessimistic
  }


  val cmdLogic = new Area {
    assert(mappings.contains(DefaultMapping))
    val rspCount = io.input.cmd.transferBeatCountMinusOne + 1

    val (orderingFork, cmdFork) = StreamFork2(io.input.cmd)
    val halt = False
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
      if (!capability.canWrite) hit clearWhen (io.input.cmd.isWrite)
      if (!capability.canRead) hit clearWhen (io.input.cmd.isRead)

      halt.setWhen(hit && portsLogic(portId).rspFifoFull)
      slaveBus.cmd.valid   := cmdFork.valid && hit && !portsLogic(portId).rspFifoFull
      slaveBus.cmd.payload := cmdFork.payload.resized
    }
    cmdFork.ready := (hits, io.outputs).zipped.map(_ && _.cmd.ready).orR && !halt

    val portId = OHToUInt(hits)

    sourceOrderingFifo.io.push.channel := io.input.cmd.source
    sourceOrderingFifo.io.push.stream.arbitrationFrom(orderingFork.throwWhen(!orderingFork.isFirst))
    sourceOrderingFifo.io.push.stream.outputId := portId
    sourceOrderingFifo.io.push.stream.beatCount := cmdToRspCountMinusOne
    //  sourceOrderingFifo.io.push.stream.context := io.input.cmd.context
  }



  val rspLogic = new Area {
    val lockValid = RegInit(False)
    val lockSel = Reg(Bits(portCount bits))
    val arbiterSel = B(OHMasking.first(portsLogic.map(_.sourceHit))) //TODO
    val portSel = lockValid ? lockSel | arbiterSel

    val beatCounter = Reg(UInt(p.beatCounterWidth bits)) init(0)

    when(io.input.rsp.valid){
      lockValid := True
      lockSel := portSel
      when(io.input.rsp.ready){
        beatCounter := beatCounter + 1
        when(io.input.rsp.last){
          lockValid := False
          beatCounter := 0
        }
      }
    }


    (portsLogic, portSel.asBools).zipped.foreach(_.rspFifo.io.pop.stream.ready := _ && io.input.rsp.ready)

    val sourceSel = MuxOH(portSel, portsLogic.map(_.sourceSel))
    val lasts = B(sourceOrdering.map(_.beatCount === beatCounter))
    val last = (lasts & sourceSel).orR


    io.input.rsp.valid := (B(portsLogic.map(_.rspFifo.io.pop.stream.valid)) & portSel).orR
    io.input.rsp.payload := MuxOH(portSel, portsLogic.map(_.rspFifo.io.pop.stream.payload))
    io.input.rsp.last.removeAssignments() := last

    for(sourceId <- 0 until sourceCount) {
      sourceOrdering(sourceId).ready := sourceSel(sourceId) && io.input.rsp.fire && io.input.rsp.last
    }
  }
}


