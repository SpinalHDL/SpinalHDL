package spinal.lib.bus.bmb

import spinal.core._
import spinal.lib.bus.misc.{AddressMapping, DefaultMapping, SizeMapping}
import spinal.lib._
import scala.collection.Seq

//TODO no rspNoHit logic when there is a default
//TODO optimized rspNoHit counter depending BMB parameters
case class BmbDecoder(p : BmbParameter,
                      mappings : Seq[AddressMapping],
                      capabilities : Seq[BmbParameter],
                      pendingMax : Int = 63,
                      pipelinedDecoder : Boolean = false,
                      pipelinedHalfPipe : Boolean = false) extends Component{

  val mappingWithWrite = (mappings, capabilities).zipped.filter((x,y) => y.access.canWrite)._1
  val mappingWithRead = (mappings, capabilities).zipped.filter((x,y) => y.access.canRead)._1

  assert(!AddressMapping.verifyOverlapping(mappingWithWrite), "BMB address decoding overlapping")
  assert(!AddressMapping.verifyOverlapping(mappingWithRead), "BMB address decoding overlapping")

  val io = new Bundle {
    val input = slave(Bmb(p))
    val outputs = Vec(master(Bmb(p)), mappings.size)
  }
  val hasDefault = mappings.contains(DefaultMapping)
  assert(mappings.count(_ == DefaultMapping) < 2, "Multiple interface with DefaultMapping in decoder")
  val logic = if(hasDefault && mappings.size == 1 && !(p.access.canWrite && !capabilities.head.access.canWrite) && !(p.access.canRead && !capabilities.head.access.canRead)){
    io.outputs(0) << io.input
  } else new Area {
    val input = io.input.cmd.pipelined(m2s = pipelinedDecoder && !pipelinedHalfPipe, halfRate = pipelinedDecoder && pipelinedHalfPipe)
    val hitsS0 = Vec(Bool(), mappings.size)
    val noHitS0 = if (!hasDefault) !hitsS0.orR else False
    val hitsS1 = if(pipelinedDecoder) RegNextWhen(hitsS0, io.input.cmd.fire) else hitsS0
    val noHitS1 = if(pipelinedDecoder) RegNextWhen(noHitS0, io.input.cmd.fire) else noHitS0
    for (portId <- 0 until mappings.length) yield {
      val slaveBus = io.outputs(portId)
      val memorySpace = mappings(portId)
      val capability = capabilities(portId)
      val hit = hitsS0(portId)
      hit := (memorySpace match {
        case DefaultMapping => !hitsS0.filterNot(_ == hit).orR
        case _ => memorySpace.hit(io.input.cmd.address)
      })
      if(!capability.access.canWrite) hit clearWhen(io.input.cmd.isWrite)
      if(!capability.access.canRead) hit clearWhen(io.input.cmd.isRead)
      slaveBus.cmd.valid := input.valid && hitsS1(portId)
      slaveBus.cmd.payload := input.payload.resized
    }
    input.ready := (hitsS1, io.outputs).zipped.map(_ && _.cmd.ready).orR || noHitS1

    val rspPendingCounter = Reg(UInt(log2Up(pendingMax + 1) bits)) init(0)
    if (p.access.canSync)
      rspPendingCounter := (rspPendingCounter + U(input.lastFire) - U(io.input.rsp.lastFire)
                           + U(input.lastFire && input.isWrite) - U(io.input.sync.fire))
    else
      rspPendingCounter := rspPendingCounter + U(input.lastFire) - U(io.input.rsp.lastFire)
    val cmdWait = Bool()
    val rspHits = RegNextWhen(hitsS1, input.valid && !cmdWait)
    val rspPending = rspPendingCounter =/= 0
    val rspNoHitValid = if (!hasDefault) !rspHits.orR else False
    val rspNoHit = !hasDefault generate new Area{
      val doIt = RegInit(False) clearWhen(io.input.rsp.lastFire) setWhen(input.fire && noHitS1 && input.last)
      val singleBeatRsp = if(p.access.canRead) RegNextWhen(input.isWrite, input.fire) else True
      val source = RegNextWhen(input.source, input.fire)
      val context = RegNextWhen(input.context, input.fire)
      val counter = p.access.canRead generate RegNextWhen(input.transferBeatCountMinusOne, input.fire)
    }
    val hitIndex = OHToUInt(rspHits)

    io.input.rsp.valid := io.outputs.map(_.rsp.valid).orR || (rspPending && rspNoHitValid)
    io.input.rsp.payload := io.outputs.map(_.rsp.payload).read(hitIndex)

    if(p.access.canInvalidate) {
      val outputInv = io.outputs.map(_.inv).read(hitIndex)
      val outputAck = io.outputs.map(_.ack).read(hitIndex)

      io.input.inv.weakAssignFrom(outputInv)
      io.input.inv.valid := outputInv.valid

      outputAck.weakAssignFrom(io.input.ack)
      io.input.ack.ready := outputAck.ready

      for ((output, index) <- io.outputs.zipWithIndex) {
        output.inv.ready := io.input.inv.ready && hitIndex === index
        output.ack.valid := io.input.ack.valid && hitIndex === index
      }
    }

    if(p.access.canSync) {
      val outputSync = io.outputs.map(_.sync).read(hitIndex)

      io.input.sync.weakAssignFrom(outputSync)
      io.input.sync.valid := outputSync.valid

      for ((output, index) <- io.outputs.zipWithIndex) {
        output.sync.ready := io.input.sync.ready && hitIndex === index
      }
    }

    if(!hasDefault) when(rspNoHit.doIt) {
      io.input.rsp.valid := True
      io.input.rsp.setError()
      io.input.rsp.source := rspNoHit.source
      io.input.rsp.context := rspNoHit.context

      //Manage io.input.rsp.last generation for multiple generation cases to save area
      if(!p.access.alignment.allowByte && (1 << p.access.lengthWidth) <= p.access.byteCount){
        io.input.rsp.last := True
      } else {
        io.input.rsp.last := False
        if (p.access.canRead) {
          io.input.rsp.last setWhen (rspNoHit.counter === 0)
          when(io.input.rsp.fire) {
            rspNoHit.counter := rspNoHit.counter - 1
          }
        }
        if (p.access.canWrite) {
          io.input.rsp.last setWhen (rspNoHit.singleBeatRsp)
        }
      }
    }
    for(output <- io.outputs) output.rsp.ready := io.input.rsp.ready

    cmdWait := ((rspPending && (hitsS1 =/= rspHits || rspNoHitValid))
               || (rspPendingCounter === pendingMax || rspPendingCounter === pendingMax - 1))
    when(cmdWait) {
      input.ready := False
      io.outputs.foreach(_.cmd.valid := False)
    }
  }
}


case class BmbErrorSlave(p : BmbParameter) extends Component{
  val io = new Bundle {
    val input = slave(Bmb(p))
  }

  val busy = RegInit(False)
  val beats = Reg(UInt(p.access.beatCounterWidth bits))
  val source = Reg(UInt(p.access.sourceWidth bits))
  val context = Reg(Bits(p.access.contextWidth bits))
  val opcode = Reg(Bits(1 bits))

  io.input.rsp.setError()
  io.input.rsp.source := source
  io.input.rsp.context := context
  if(p.access.canExclusive) io.input.rsp.exclusive := False
  if(p.access.canWrite) io.input.rsp.data.assignDontCare()



  io.input.rsp.last := beats === 0 || opcode === Bmb.Cmd.Opcode.WRITE
  when(!busy){
    io.input.cmd.ready := True
    io.input.rsp.valid := False
    busy := io.input.cmd.valid && io.input.cmd.last
    beats := io.input.cmd.transferBeatCountMinusOne
    source := io.input.cmd.source
    context := io.input.cmd.context
    opcode := io.input.cmd.opcode
  } otherwise {
    io.input.cmd.ready := False
    io.input.rsp.valid := True
    when(io.input.rsp.ready){
      beats := beats - 1
      when(io.input.rsp.last ){
        busy := False
      }
    }
  }
}

case class BmbDecoderPerSource  (p : BmbParameter,
                      mappings : Seq[AddressMapping],
                      capabilities : Seq[BmbParameter],
                      pendingMax : Int = 63) extends Component{
  assert(!AddressMapping.verifyOverlapping(mappings), "BMB address decoding overlapping")

  val io = new Bundle {
    val input = slave(Bmb(p))
    val outputs = Vec(master(Bmb(p)), mappings.size)
  }
  val hasDefault = mappings.contains(DefaultMapping)
  assert(mappings.count(_ == DefaultMapping) < 2, "Multiple interface with DefaultMapping in decoder")
  val logic = if(hasDefault && mappings.size == 1 && !(p.access.canWrite && !capabilities.head.access.canWrite) && !(p.access.canRead && !capabilities.head.access.canRead)){
    io.outputs(0) << io.input
  } else new Area {
    var internalMapping = mappings
    var internalOutputs = io.outputs.toList
    var internalCapabilities = capabilities
    if(!hasDefault){
      val error = BmbErrorSlave(p)
      internalMapping :+= DefaultMapping
      internalOutputs :+= error.io.input
      internalCapabilities :+= p
    }

    val hits = Vec(Bool(), internalMapping.size)
    for (portId <- 0 until internalMapping.length) yield {
      val slaveBus = internalOutputs(portId)
      val memorySpace = internalMapping(portId)
      val capability = internalCapabilities(portId)
      val hit = hits(portId)
      hit := (memorySpace match {
        case DefaultMapping => !hits.filterNot(_ == hit).orR
        case _ => memorySpace.hit(io.input.cmd.address)
      })
      if(!capability.access.canWrite) hit clearWhen(io.input.cmd.isWrite)
      if(!capability.access.canRead) hit clearWhen(io.input.cmd.isRead)
      slaveBus.cmd.valid := io.input.cmd.valid && hit
      slaveBus.cmd.payload := io.input.cmd.payload.resized
    }

    io.input.cmd.ready := (hits, internalOutputs).zipped.map(_ && _.cmd.ready).orR

    val sources = for((id, _) <- p.access.sources) yield new Area {
      val hit = io.input.cmd.source === id
      val rspHits = RegNextWhen(hits, io.input.cmd.fire && hit)
      val rspPendingCounter = Reg(UInt(log2Up(pendingMax + 1) bits)) init(0)
      rspPendingCounter := rspPendingCounter + U(io.input.cmd.lastFire && io.input.cmd.source === id) - U(io.input.rsp.lastFire && io.input.rsp.source === id)

      val rspPending = rspPendingCounter =/= 0
      val full = rspPendingCounter === pendingMax
      val cmdWait = hit && (rspPending && hits =/= rspHits || full)
    }

    when(sources.map(_.cmdWait).toSeq.orR) {
      io.input.cmd.ready := False
      internalOutputs.foreach(_.cmd.valid := False)
    }

    io.input.rsp << StreamArbiterFactory().fragmentLock.roundRobin.on(internalOutputs.map(_.rsp))
  }
}

object BmbDecoderOutOfOrder{
  def getOutputParameter(inputParameter : BmbParameter) = inputParameter.copy(access = inputParameter.access.sourcesTransform(_.copy(contextWidth = 0)))
}

case class BmbDecoderOutOfOrder(p : BmbParameter,
                                mappings : Seq[AddressMapping],
                                capabilities : Seq[BmbParameter],
                                pendingRspTransactionMax : Int) extends Component{
  assert(!AddressMapping.verifyOverlapping(mappings), "BMB address decoding overlapping")
  assert(isPow2(pendingRspTransactionMax))

  val outputParameter = BmbDecoderOutOfOrder.getOutputParameter(p)

  val io = new Bundle {
    val input = slave(Bmb(p))
    val outputs = Vec(master(Bmb(outputParameter)), mappings.size)
  }
//  val sourceCount = 1 << p.sourceWidth
  val portCount = mappings.size
  val withDefault = mappings.contains(DefaultMapping)

  case class SourceHistory() extends Bundle {
    val hits = Bits(mappings.size bits)
    val beatCount = UInt(p.access.beatCounterWidth bits)
    val context = Bits(p.access.contextWidth bits)
  }

  val sourceCount = p.access.sources.size
  val sourceOrderingFifo = StreamFifoMultiChannelSharedSpace(SourceHistory(), channelCount = sourceCount, depth = pendingRspTransactionMax)
  val sourceOrderingUnbuffered = sourceOrderingFifo.io.pop.toStreams(withCombinatorialBuffer = true).unsetName()
  val sourceOrdering = sourceOrderingUnbuffered.map(_.m2sPipe())

  val cmdToRspCountMinusOne = io.input.cmd.isRead ? io.input.cmd.transferBeatCountMinusOne | 0

  val portsLogic = for ((port, portId) <- io.outputs.zipWithIndex) yield new Area {
    val rspFifo = StreamFifoMultiChannelSharedSpace(Fragment(BmbRsp(outputParameter)), channelCount = sourceCount, depth = pendingRspTransactionMax)
    rspFifo.io.push.stream.valid := port.rsp.valid
    rspFifo.io.push.stream.payload := port.rsp.payload
    rspFifo.io.push.channel := UIntToOh(port.rsp.source, port.p.access.sourcesId.toSeq)
    port.rsp.ready := True
    assert(!(rspFifo.io.push.stream.isStall))

    val sourceHits = B(for (sourceId <- 0 until sourceCount) yield !rspFifo.io.pop.empty(sourceId) && sourceOrdering(sourceId).valid && sourceOrdering(sourceId).hits(portId))
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


    val incomingRspCount = Reg(UInt(log2Up(pendingRspTransactionMax) + 1 bits)) init(2 + p.access.transferBeatCount) //Init 2 to compensate rspFifo availability latency in a pessimistic way
    val incomingRspAdd = port.cmd.lastFire ? ((U"0" @@ cmdToRspCountMinusOne) + 1) | 0
    incomingRspCount := incomingRspCount + incomingRspAdd - U(port.rsp.fire)

//    val rspFifoFull = cmdToRspCountMinusOne + incomingRspCount >= RegNext(rspFifo.io.availability)
    val rspFifoFull = incomingRspCount >= RegNext(rspFifo.io.availability) //Pessimistic
  }


  val cmdLogic = new Area {
    assert(mappings.count(_ == DefaultMapping) <= 1)
    val rspCount = io.input.cmd.transferBeatCountMinusOne + 1

    val (orderingFork, cmdFork) = StreamFork2(io.input.cmd)
    val halt = False
    val lock = RegInit(False) setWhen(io.input.cmd.valid && !halt) clearWhen(io.input.cmd.ready) //Counter act the pessimistic occupancy tracking
    val hits = Vec(Bool(), mappings.size)
    for (portId <- 0 until mappings.length) yield {
      val slaveBus = io.outputs(portId)
      val memorySpace = mappings(portId)
      val capability = capabilities(portId)
      val hit = hits(portId)
      hit := (memorySpace match {
        case DefaultMapping => !hits.filterNot(_ == hit).orR
        case _ => memorySpace.hit(io.input.cmd.address)
      })
      if (!capability.access.canWrite) hit clearWhen (io.input.cmd.isWrite)
      if (!capability.access.canRead) hit clearWhen (io.input.cmd.isRead)

      halt.setWhen(hit && portsLogic(portId).rspFifoFull)
      slaveBus.cmd.valid   := cmdFork.valid && hit && (!portsLogic(portId).rspFifoFull || lock)
      slaveBus.cmd.payload := cmdFork.payload.resized
      slaveBus.cmd.context.removeAssignments()
    }

    halt clearWhen(lock)
    cmdFork.ready := ((hits, io.outputs).zipped.map(_ && _.cmd.ready).orR || B(hits) === 0) && !halt

    sourceOrderingFifo.io.push.channel :=  UIntToOh(io.input.cmd.source, io.input.p.access.sourcesId.toSeq)
    sourceOrderingFifo.io.push.stream.arbitrationFrom(orderingFork.throwWhen(!orderingFork.isFirst))
    sourceOrderingFifo.io.push.stream.hits := B(hits)
    sourceOrderingFifo.io.push.stream.beatCount := cmdToRspCountMinusOne
    sourceOrderingFifo.io.push.stream.context := io.input.cmd.context
  }



  val rspLogic = new Area {
    val lockValid = RegInit(False)
    val lockSel = Reg(Bits(portCount bits))
    val arbiterSel = B(OHMasking.first(portsLogic.map(_.sourceHit))) //TODO
    val portSel = lockValid ? lockSel | arbiterSel

    val error = !withDefault generate new Area{
      val hits = B(sourceOrdering.map(s => s.valid && s.hits === 0))
      val valid = RegInit(False)
      val sourceSel = Reg(Bits(sourceCount bits))

      when(!valid) {
        when(hits.orR) {
          valid := True
          sourceSel := OHMasking.first(hits)
        }
      } otherwise {
        arbiterSel := 0
        when(io.input.rsp.lastFire && portSel === 0){
          valid := False
        }
      }
    }

    val beatCounter = Reg(UInt(p.access.beatCounterWidth bits)) init(0)

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
    io.input.rsp.context.removeAssignments() := MuxOH(sourceSel, sourceOrdering.map(_.context))

    if(!withDefault) when(portSel === 0 && error.valid){
      io.input.rsp.valid := True
      io.input.rsp.setError()
      sourceSel := error.sourceSel
    }

    io.input.rsp.source.removeAssignments() := OHToUInt(sourceSel, p.access.sourcesId.toSeq)
    for(sourceId <- 0 until sourceCount) {
      sourceOrdering(sourceId).ready := sourceSel(sourceId) && io.input.rsp.fire && io.input.rsp.last
    }
  }
}


