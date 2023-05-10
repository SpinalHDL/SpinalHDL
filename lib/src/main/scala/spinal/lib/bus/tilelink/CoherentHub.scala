package spinal.lib.bus.tilelink

import spinal.core._
import spinal.lib._
import spinal.lib.bus.misc.SizeMapping
import spinal.lib.fsm._
import spinal.lib.pipeline._

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

case class CoherentHubParameters(nodes : Seq[NodeParameters],
                                 slotCount : Int,
                                 cacheSize: Int,
                                 wayCount: Int,
                                 lineSize : Int,
                                 aBufferCount: Int = 4,
                                 cBufferCount: Int = 2,
                                 cSourceCount: Int = 4) {
  val addressWidth = nodes.map(_.m.addressWidth).max
  val dataWidth = nodes.head.m.dataWidth
  val dataBytes = dataWidth/8
  val waySize = cacheSize/wayCount
  val linePerWay = waySize/lineSize
  val tagRange = addressWidth-1 downto log2Up(linePerWay*lineSize)
  val lineRange = tagRange.low-1 downto log2Up(lineSize)
  val wordRange = log2Up(lineSize)-1 downto log2Up(dataBytes)
  val refillRange = tagRange.high downto lineRange.low
  val blockRange = addressWidth-1 downto log2Up(lineSize)
  val blockSize = lineSize

  assert(nodes.forall(_.m.dataWidth == dataWidth))
}

/*
access main memory :
- read without ownership
- writes without ownership
- refill
- writeback
- release miss

Read cache :
- read without ownership
- read allocate
- writeback

write cache
- writes without ownership
- refill
- release hit

drive upstream D
- cache read
- upsteam C ?
- downstream D
 */

case class CoherentHubOrdering(p : CoherentHubParameters) extends Bundle{
  val upId = UInt(log2Up(p.nodes.size) bits)
  val upSource = UInt(p.nodes.map(_.m.sourceWidth).max bits)
}


class CoherentHub(p : CoherentHubParameters) extends Component{
  import p._

  val slotDownOffset = 0
  val cDownIdOffset = slotDownOffset + (1 << log2Up(slotCount max p.cSourceCount))
  val downIdCount = cDownIdOffset + p.cSourceCount

  val bps = p.nodes.map(_.toBusParameter())
  val upParam = NodeParameters.mergeMasters(p.nodes)
  val upParamPerNodeSourceWidth = nodes.map(_.m.sourceWidth).max
  val upBusParam = upParam.toBusParameter()
  val downPutParam = NodeParameters(
    m = CoherentHub.downPutM2s(
      name           = this,
      addressWidth   = addressWidth,
      dataWidth      = dataWidth,
      blockSize      = blockSize,
      slotCount      = slotCount,
      cSourceCount   = cSourceCount
    ),
    s = S2mParameters(slaves = Nil)
  )
  val downGetParam = NodeParameters(
    m = CoherentHub.downGetM2s(
      name           = this,
      addressWidth   = addressWidth,
      dataWidth      = dataWidth,
      blockSize      = blockSize,
      slotCount      = slotCount
    ),
    s = S2mParameters(slaves = Nil)
  )
  val downPutBusParam = downPutParam.toBusParameter()
  val downGetBusParam = downGetParam.toBusParameter()
  val wordsPerBlock = blockSize / upBusParam.dataBytes
  def upSourceToNodeOh(source : UInt) = UIntToOh(source.takeHigh(log2Up(p.nodes.size)).asUInt, nodes.size)

  val io = new Bundle{
    val ups = Vec(bps.map(e => slave(Bus(e))))
    val downPut = master(Bus(downPutBusParam))
    val downGet = master(Bus(downGetBusParam))

    val ordering = new Bundle{
      val toDownGet = master Flow(CoherentHubOrdering(p))
      val toDownPut = master Flow(CoherentHubOrdering(p))
      val bToT = master Flow(CoherentHubOrdering(p))
      def all = List(toDownGet, toDownPut, bToT)
    }
  }
  val nodeToMasterMaskMapping = p.nodes.map(node => node -> node.m.masters.filter(_.emits.withBCE).zipWithIndex.toMapLinked()).toMapLinked()
  val slots = for(i <- 0 until slotCount) yield new Area{
    val id = i
    val fire = False
    val fireBuffer = RegNext(fire) init(False)
    val valid = RegInit(False) clearWhen(fire)
    assert(!(fire && !valid))
    val address = Reg(upBusParam.address) //TODO optimize in mem
    val source  = Reg(upBusParam.source())
    val shared = Reg(Bool())
    val unique = Reg(Bool())
    val fireOnDownD = Reg(Bool())
    val tagsReaded = Reg(Bool())
    val readSent, writeSent = Reg(Bool())
    val readMem, writeMem = Reg(Bool())
    val aquireBtoT = Reg(Bool())
    val bToTRspSent = Reg(Bool())

    val lineConflict = new Area{ //TODO implement and check works
      val youngest = Reg(Bool()) //TODO set when conflict is resolved
      val valid = Reg(Bool()) //TODO clear
      val slotId = Reg(UInt(log2Up(slotCount) bits))
      def oldest = !valid
    }

    val probe = new Area{
      val waitAckData = Reg(Bool())
      val filtred = Reg(Bool())
      val ports = for((node, id) <- p.nodes.zipWithIndex) yield new Area{
        val pending =  Reg(Bits(nodeToMasterMaskMapping(node).size bits)) init(0)
        val inflight = Reg(Bits(nodeToMasterMaskMapping(node).size bits)) init(0)
        val empty = (pending ## inflight) === 0
      }
      val gotToN = Reg(Bool())
      val isShared = Reg(Bool())
    }

    def spawn(): Unit ={
      readSent := False
      writeSent := False
      probe.waitAckData := False
      aquireBtoT := False
      bToTRspSent := False
      probe.gotToN := False
      probe.isShared := False
    }
  }

  for(slot <- slots){
    when(slots.map(_.fireBuffer).read(slot.lineConflict.slotId)) {
      slot.lineConflict.valid := False
    }
  }

  val slotsC = for(i <- 0 until cSourceCount) yield new Area{
    val id = i
    val fire = False
    val valid = RegInit(False) clearWhen(fire)
    val release = Reg(Bool())
    val source = Reg(upBusParam.source)
    val probe = new Area{
      val valid = !release
      val id = Reg(UInt(log2Up(slotCount) bits))
    }

    def spawn(): Unit ={

    }
  }

  val slotsMem = new Area{
    val address = Mem.fill(slotCount)(upBusParam.address)
    val size    = Mem.fill(slotCount)(upBusParam.size)
    val source  = Mem.fill(slotCount)(upBusParam.source)
//    val selfSource = Mem.fill(slotCount)(upBusParam.source)
    val opcode  = Mem.fill(slotCount)(Opcode.A())
  }

  def slotAddress(oh : Bits) = slotsMem.address.readAsync(OHToUInt(oh))
  def slotSize(oh : Bits) = slotsMem.size.readAsync(OHToUInt(oh))
  def slotSource(oh : Bits) = slotsMem.source.readAsync(OHToUInt(oh))

  case class Payload() extends Bundle {
    val mask = upBusParam.mask()
    val data = upBusParam.data()
  }

  case class ContextC() extends Bundle{
    val address = upBusParam.address()
    val source = upBusParam.source()
    val slot = UInt(log2Up(cSourceCount) bits)
    val probe = Bool()
    val noData = Bool()
  }

  val withDataA = p.nodes.exists(_.m.withDataA)
  val upstream = new Area{
    val buses = io.ups.zipWithIndex.map{case (bus, id) => bus.withSourceOffset(id << upParamPerNodeSourceWidth, upParam.m.sourceWidth)}
    val a = new Area{
      val withoutData = buses.filter(!_.p.withDataA).map(_.a).toList
      val withData = withDataA generate new Area{
        val buffer =  new Area{
          val ram = Mem.fill(aBufferCount*p.blockSize/upParam.m.dataBytes)(Payload())
          val allocated = Reg(Bits(aBufferCount bits)) init(0)
          val set, clear = B(0, aBufferCount bits)
          val firstFree = OHToUInt(OHMasking.firstV2(~allocated))
          val full = allocated.andR
          val source = Vec.fill(aBufferCount)(Reg(upBusParam.source))
          val write = ram.writePort()
          allocated := (allocated | set) & ~clear
        }

        val filtred = buses.filter(_.p.withDataA).map(_.a)
        val merged = StreamArbiterFactory().roundRobin.lambdaLock[ChannelA](_.isLast()).on(filtred)
        val withBeats = merged.withBeats
        val halted = merged.haltWhen(withBeats && buffer.full)
        val locked = RegInit(False) setWhen(buffer.write.valid)
        val lockedValue = RegNextWhen(buffer.firstFree, !locked)
        val bufferAllocated = locked ? lockedValue | buffer.firstFree
        buffer.write.valid := withBeats && halted.valid
        buffer.write.address := bufferAllocated @@ halted.address(wordRange)
        buffer.write.data.mask := halted.mask
        buffer.write.data.data := halted.data
        when(halted.fire && halted.isLast() && withBeats){
          buffer.set(bufferAllocated) := True
          buffer.source.onSel(bufferAllocated)(_ := halted.source)
          locked := False
        }
        val toSlot = halted.translateWith(halted.asNoData()).takeWhen(halted.isLast())
        for(i <- 0 until upBusParam.sizeMax) toSlot.address(i) clearWhen(toSlot.size > i)
      }

      val aList = if(withDataA) withData.toSlot :: withoutData else withoutData
      val toSlot = StreamArbiterFactory().noLock.roundRobin.on(aList)
    }
    val c = new Area{
      val perBus = for((bus, id) <- buses.zipWithIndex if bus.p.withBCE) yield new Area {
        val nodeId = id
        val (probeFork, dataFork) = StreamFork2(bus.c)
        val toProbeRsp = probeFork.toFlow.takeWhen(bus.c.isProbeKind && bus.c.isFirst())
        val toData = dataFork.takeWhen(dataFork.isDataKind() || dataFork.isReleaseKind())
      }

      val merged = new Area{
        val arbitred = StreamArbiterFactory().lowerFirst.lambdaLock[ChannelA](_.isLast()).on(perBus.map(_.toData))
        val (bufferFork, ctrlFork) = StreamFork2(arbitred)
        val buffer = bufferFork.throwWhen(!bufferFork.isDataKind()).translateWith(bufferFork.data).queue(cBufferCount*wordsPerBlock).m2sPipe()
        val slotOh = B(OHMasking.firstV2(slotsC.map(!_.valid).asBits))
        val toCtrlUnbuffered = Stream(ContextC())
        toCtrlUnbuffered.arbitrationFrom(ctrlFork.takeWhen(ctrlFork.isFirst()))
        toCtrlUnbuffered.address := ctrlFork.address
        toCtrlUnbuffered.source := ctrlFork.source
        toCtrlUnbuffered.slot := OHToUInt(slotOh)
        toCtrlUnbuffered.probe := ctrlFork.opcode === Opcode.C.PROBE_ACK_DATA
        toCtrlUnbuffered.noData := ctrlFork.opcode === Opcode.C.RELEASE
        val toCtrl = toCtrlUnbuffered.haltWhen(slotsC.map(_.valid).andR && !toCtrlUnbuffered.noData).halfPipe()

        val hits = slots.map(slot => slot.valid && slot.lineConflict.oldest && slot.address(lineRange) === toCtrlUnbuffered.address(lineRange))
        val sel = OHToUInt(hits)
        when(toCtrlUnbuffered.fire && ctrlFork.isDataKind()){
          slotsC.onMask(slotOh){ s =>
            s.valid := True
            s.release := ctrlFork.opcode === Opcode.C.RELEASE_DATA
            s.source := ctrlFork.source
            s.probe.id := sel
            s.spawn()
          }
        }
      }
    }
    val e = new Area{
      val perBuses = for(bus <- buses if bus.p.withBCE) yield new Area{
        bus.e.ready := True
        when(bus.e.fire) {
          slots.onSel(bus.e.sink.resized) { s =>
            s.fire := True
          }
        }
      }
    }
  }


  val slotSpawn = new Area{
    val push = upstream.a.toSlot.combStage()
    val oh = OHMasking.firstV2(~B(slots.map(_.valid)))
    val sel = OHToUInt(oh)
    val full = slots.map(_.valid).andR
    val lineConflicts = B(for(slot <- slots) yield slot.valid && slot.lineConflict.youngest && slot.address(lineRange) === push.address(lineRange))
    val shared,  unique = Bool()
    val put, get, acquireBlock = False
    acquireBlock setWhen(push.opcode === Opcode.A.ACQUIRE_BLOCK)
    switch(push.opcode){
      is(Opcode.A.ACQUIRE_BLOCK, Opcode.A.ACQUIRE_PERM){
        shared := push.param === Param.Grow.NtoB
        unique := push.param === Param.Grow.NtoT || push.param === Param.Grow.BtoT
      }
      is(Opcode.A.PUT_FULL_DATA, Opcode.A.PUT_PARTIAL_DATA){
        shared := False
        unique := True
        put := True
      }
      is(Opcode.A.GET){
        shared := True
        unique := False
        get := True
      }
    }

    val mastersOh = B(upParam.m.masters.map(_.sourceHit(push.source)))
    val mastersSource = upParam.m.masters.map(v => U(v.bSourceId, upBusParam.sourceWidth bits)).toList
//    val selfSource = OHMux.mux(mastersOh, mastersSource)
    push.ready := !full
    when(push.fire){
      slotsMem.address.write(sel, push.address)
      slotsMem.size.write(sel, push.size)
      slotsMem.source.write(sel, push.source)
//      slotsMem.selfSource.write(sel, selfSource)
      slotsMem.opcode.write(sel, push.opcode)
      slots.onMask(lineConflicts){ s =>
        s.lineConflict.youngest := False
      }
      slots.onMask(oh){ s =>
        s.valid := True
        s.spawn()
        s.address := push.address
        s.source := push.source
        s.shared := shared
        s.unique := unique
        s.fireOnDownD := put | get
        s.writeMem := put
        s.readMem := get || acquireBlock
        s.lineConflict.youngest := True
        s.lineConflict.valid := lineConflicts.orR
        s.lineConflict.slotId := OHToUInt(lineConflicts)
        s.tagsReaded := True //TODO support cache
        s.probe.filtred := True //TODO support dictionary

        val isBtoT = push.opcode === Opcode.A.ACQUIRE_BLOCK && push.param === Param.Grow.BtoT
        for ((port, node) <- (s.probe.ports, nodes).zipped) {
          val mapping = nodeToMasterMaskMapping(node)
          for ((mpp, id) <- mapping) {
            //when(!mpp.sourceHit(push.source) || isBtoT) { //isBtoT is required to ensure the master copy wasn't probed out
              port.pending(id) := True
            //}
          }
        }
      }
    }
  }

  case class DispatchD(d : Stream[ChannelD], oh : Bits)
  val dispatchD = ArrayBuffer[DispatchD]()

  val probe = new Area{
    val sel = new Area{
      val pendings = Vec(slots.map(s => s.lineConflict.oldest && s.probe.ports.map(_.pending.orR).orR))
      val arbiter = StreamArbiterFactory().roundRobin.build(NoData(), slotCount).io
      Vec(arbiter.inputs.map(_.valid)) := pendings
      when(arbiter.output.fire){
        slots.onMask(arbiter.chosenOH){slot =>
          slot.probe.ports.foreach{p =>
            p.pending := 0
            p.inflight := p.pending
          }
        }
      }
      case class ProbeContext() extends Bundle{
        val slotOh  = Bits(slotCount bits)
        val slotSource = upBusParam.source()
        val address = upBusParam.address()
        val param   = Bits(3 bits)
        val mask    = Vec(nodeToMasterMaskMapping.values.map(e => Bits(e.size bits)))
      }
      val ctx = ProbeContext()
      ctx.slotOh := arbiter.chosenOH
      ctx.slotSource := slotSource(arbiter.chosenOH)
      ctx.address := slotAddress(arbiter.chosenOH)
      ctx.param   := (OhMux(arbiter.chosenOH, slots.map(_.unique)) ? B(Param.Cap.toN) | B(Param.Cap.toB)).resized
      for(nodeId <- 0 until nodes.size) {
        ctx.mask(nodeId) := OhMux(arbiter.chosenOH, slots.map(_.probe.ports(nodeId).pending))
      }
      val stream = arbiter.output.translateWith(ctx).s2mPipe() //S2m pipe ensure that the pending / inflight status are updated
    }

    val cmd = new Area{
      val input = sel.stream.combStage()
      val ssl = U(input.slotSource.dropHigh(log2Up(p.nodes.size)))
      val ssh = U(input.slotSource.takeHigh(log2Up(p.nodes.size)))
      val buses = for ((node, nodeId) <- p.nodes.zipWithIndex.filter(_._1.withBCE)) yield new Area {
        val bus = upstream.buses(nodeId).b
        val mapping = nodeToMasterMaskMapping(node)
        val fired = Reg(Bits(mapping.size bits)) init(0)
        val requests = input.mask(nodeId) & ~fired
        val masterOh = OHMasking.firstV2(requests)
        val slotSourceBusHit = ssh === nodeId
        val slotSourceChannelHits = B(mapping.keys.map(m => m.mapping.map(_.id.hit(ssl)).orR).toList)
        val isTargetingSlotSource = slotSourceBusHit && slotSourceChannelHits === masterOh
        val ready = requests === 0
        bus.valid   := input.valid && requests.orR
        bus.opcode  := Opcode.B.PROBE_BLOCK
        bus.param   := (isTargetingSlotSource ? B(Param.Cap.toT, 3 bits) | input.param)
        bus.source  := OhMux(masterOh, mapping.keys.map(m => U(m.bSourceId, upBusParam.sourceWidth bits)).toList)
        bus.address := input.address(blockRange) @@ U(0, blockRange.low bits)
        bus.size    := log2Up(p.blockSize)
        when(isTargetingSlotSource){
          bus.source := input.slotSource
        }
        when(bus.fire){
          fired.asBools.onMask(masterOh)(_ := True)
        }
        when(input.fire){
          fired := 0
        }
      }
      input.ready := buses.map(_.ready).andR
    }

    val rsps = for((node, nodeId) <- p.nodes.zipWithIndex.filter(_._1.withBCE)) yield new Area{
      val c = upstream.c.perBus.find(_.nodeId == nodeId).get.toProbeRsp
      val mapping = nodeToMasterMaskMapping(node)
      val sourceIdHits = node.m.masters.map(m => m -> m.sourceHit(c.source.resize(upParamPerNodeSourceWidth))).toMapLinked()
      val isAckData = c.opcode === Opcode.C.PROBE_ACK_DATA
      val keeptCopy = List(Param.Prune.TtoB, Param.Report.TtoT, Param.Report.BtoB).map(c.param === _).orR
      val onSlots = for(slot <- slots) yield new Area{
        val hit = slot.valid && slot.address(lineRange) === c.address(lineRange) && slot.lineConflict.oldest
        val ctx = slot.probe.ports(nodeId)
        val selfProbe = hit && slot.source === c.source
        val notEnough = False //!selfProbe && slot.unique && keeptCopy
        when(hit && c.fire){
          when(isAckData){
            slot.probe.waitAckData := True
          }
          when(!notEnough && keeptCopy){
            slot.probe.isShared := True
          }
          for((m, id) <- mapping) {
            when(sourceIdHits(m)) {
              ctx.inflight(id) := False
              //ctx.pending(id) setWhen (notEnough) //Redo the probe
            }
          }
          when(selfProbe && keeptCopy){
            slot.readMem := False
            slot.aquireBtoT := True
          }
        }
      }
    }

//    slots.foreach(slot =>
//      slot.probe.gotToN setWhen(rsps.map(_.onSlots(slot.id).orR))
//    )

  }


  val ADDRESS = Stageable(upBusParam.address)
  val SIZE = Stageable(upBusParam.size)
  val DOWN_SOURCE = Stageable(UInt(downPutBusParam.sourceWidth bits))
  val READ = Stageable(Bool())
  val WRITE = Stageable(Bool())
  val FROM_C = Stageable(Bool())
  val SOURCE = Stageable(upBusParam.source)

  val cDispatch = new Area{
    val input = upstream.c.merged.toCtrl
    val toCtrl = cloneOf(input)
    toCtrl.payload := input.payload

    val upD = Stream(ChannelD(upBusParam))
    upD.opcode := Opcode.D.RELEASE_ACK
    upD.param := 0
    upD.source := input.source
    upD.size := log2Up(blockSize)
    upD.sink.assignDontCare()
    upD.denied := False
    upD.data.assignDontCare()
    upD.corrupt := False

    toCtrl.valid := input.valid && !input.noData
    upD.valid   := input.valid && input.noData
    input.ready := input.noData ? upD.ready | toCtrl.ready

    val upDBuffered = upD.halfPipe()
    val upHits = upSourceToNodeOh(upDBuffered.source)
    dispatchD += DispatchD(upDBuffered, upHits)
  }

  val ctrl = new Area{
    val aRead = Stream(ChannelA(downGetBusParam))
    val aWrite = Stream(ChannelA(downPutBusParam))

    //    val aWriteThrough = Stream(ChannelA(downBusParam))
    val frontend = new Pipeline{
      //TODO avoid tasking something which will be blocked by an already busy $read / ?write pipeling
      val s0 = new Stage {
        val fromC = cDispatch.toCtrl
        val requests = slots.map(s => s.valid && !s.probe.waitAckData && s.tagsReaded && s.probe.filtred && s.probe.ports.map(_.empty).andR && (s.readMem && !s.readSent || s.writeMem && !s.writeSent))
        val arbiter = StreamArbiterFactory().noLock.roundRobin.build(NoData(), slotCount).io
        (arbiter.inputs, requests).zipped.foreach(_.valid := _)
        driveFrom(arbiter.output)
        DOWN_SOURCE := OHToUInt(arbiter.chosenOH).resized
        ADDRESS := slotAddress(arbiter.chosenOH)
        SIZE := slotSize(arbiter.chosenOH)
        READ := OhMux.or(arbiter.chosenOH, slots.map(s => s.readMem && !s.readSent ))
        WRITE := OhMux.or(arbiter.chosenOH, slots.map(s => s.writeMem && !s.writeSent ))
        SOURCE := slotSource(arbiter.chosenOH)

        when(isFireing && !FROM_C) {
          slots.onMask(arbiter.chosenOH) { s =>
            s.readSent setWhen (s.readMem)
            s.writeSent setWhen (s.writeMem)
          }
        }

        val oGet = io.ordering.toDownGet
        oGet.valid := isFireing && READ
        (oGet.upId,  oGet.upSource) := this(SOURCE)

        val oPut = io.ordering.toDownPut
        oPut.valid := isFireing && WRITE && !(FROM_C && fromC.probe)
        (oPut.upId,  oPut.upSource) := this(SOURCE)


        fromC.ready := isReady
        FROM_C := False
        when(fromC.valid){
          isValid := True
          ADDRESS := fromC.address
          DOWN_SOURCE := fromC.slot.resized
          SOURCE  := fromC.source
          SIZE    := log2Up(blockSize)
          READ    := False
          WRITE   := True
          FROM_C  := True
        }
      }
      val s1 = new Stage(Connection.DIRECT())
    }

    val stageables = List(DOWN_SOURCE, ADDRESS, SIZE, READ, WRITE, SOURCE, FROM_C)
    val readMem = new Pipeline{
      val s0 = new Stage{
        driveFrom(frontend.s1, READ, stageables)
      }
      val s1 = new Stage(Connection.M2S()){
        haltIt(aRead.isStall)
        aRead.valid   := isValid
        aRead.opcode  := Opcode.A.GET
        aRead.param   := 0
        aRead.source  := DOWN_SOURCE.resized
        aRead.size    := SIZE
        aRead.address := ADDRESS
      }
      build()
    }

    val writePip = new Pipeline{
      val s0 = new Stage{
        driveFrom(frontend.s1, WRITE, stageables)
      }
      val s1 = new Stage(Connection.M2S()){
        val counter = Reg(downPutBusParam.beat) init(0)
        val beatsMinusOne = sizeToBeatMinusOne(downGetBusParam, SIZE)
        val isLast = counter === beatsMinusOne
        val BEAT_ADDRESS = insert(ADDRESS | (counter << log2Up(downPutParam.m.dataBytes)).resized)

        val upBuffer = withDataA generate new Area{
          val read = upstream.a.withData.buffer.ram.readSyncPort()
          val hits = upstream.a.withData.buffer.source.map(_ === SOURCE).asBits & upstream.a.withData.buffer.allocated
          val id = OHToUInt(hits)
          read.cmd.valid := False
          read.cmd.payload := id @@ (BEAT_ADDRESS.resize(log2Up(p.blockSize)) >> log2Up(downPutParam.m.dataBytes))
        }

        forkIt(!isLast)

        when(isForked){
          if(withDataA) when(!FROM_C) {
            upBuffer.read.cmd.valid := True
          }
          counter := counter + 1
          when(isLast){
            counter := 0
            if(withDataA) when(!FROM_C) {
              upstream.a.withData.buffer.clear := upBuffer.hits
            }
          }
        }
      }

      val s2 = new Stage(Connection.M2S()){
        val BUFFER = withDataA generate insert(s1.upBuffer.read.rsp)
      }

      val s3 = new Stage(Connection.M2S()){
        haltIt(aWrite.isStall)
        val cData = upstream.c.merged.buffer
        aWrite.valid   := isValid
        aWrite.opcode  := Opcode.A.PUT_PARTIAL_DATA
        aWrite.param   := 0
        aWrite.size    := SIZE
        aWrite.address := s1.BEAT_ADDRESS
        aWrite.mask.setAll()
        aWrite.data := cData.payload
        aWrite.source := (cDownIdOffset | this(DOWN_SOURCE).resized).resized
        aWrite.corrupt := False

        cData.ready := False
        when(FROM_C){
          when(!cData.valid){
            haltIt()
            aWrite.valid := False
          }
          when(isValid) {
            cData.ready := isReady
          }
        }
        if(withDataA) when(!FROM_C){
          aWrite.mask := s2.BUFFER.mask
          aWrite.data := s2.BUFFER.data
          aWrite.source  := DOWN_SOURCE.resized
        }
      }

      build
    }

    frontend.build()
  }



  val downToUp = new Area{
    val get = new Area {
      val downD = io.downGet.d
      val isLast = downD.isLast()
      val slotId = downD.source.resize(log2Up(slotCount))
      val upSource = slotsMem.source.readAsync(slotId)
      val slotOpcode = slotsMem.opcode.readAsync(slotId)
      val isShared = slots.map(s => s.probe.isShared).read(slotId)
      val upHits = upSourceToNodeOh(upSource)
      val upD = Stream(ChannelD(upBusParam))
      upD.arbitrationFrom(downD)
      upD.size := downD.size
      upD.denied := downD.denied
      upD.data := downD.data
      upD.corrupt := downD.corrupt
      upD.source := upSource
      upD.sink := slotId
      upD.opcode := slotOpcode.muxListDc(List(
        Opcode.A.GET           -> Opcode.D.ACCESS_ACK_DATA(),
        Opcode.A.ACQUIRE_BLOCK -> Opcode.D.GRANT_DATA()
      ))
      upD.param := (isShared ? B(Param.Cap.toB, 3 bits) | B(Param.Cap.toT, 3 bits))
      dispatchD += DispatchD(upD, upHits)
      when(downD.fire && isLast) {
        slots.onSel(downD.source.resized) { s =>
          when(s.fireOnDownD){
            s.fire := True
          }
        }
      }
    }
    val put = new Area{
      val downD = io.downPut.d
      val fromC = downD.source >= cDownIdOffset
      val slotCRelease = slotsC.map(_.release).read(downD.source.resized)
      val slotCProbeId = slotsC.map(_.probe.id).read(downD.source.resized)
      val toUp = !fromC || slotCRelease
      val slotId = downD.source.resize(log2Up(slotCount))
      val upSourceFromSlot = slotsMem.source.readAsync(slotId)
      val upSourceFromSlotC = slotsC.map(_.source).read(downD.source.resized)
      val upSource = fromC ? upSourceFromSlotC | upSourceFromSlot
      val upHits = upSourceToNodeOh(upSource)
      val upD = Stream(ChannelD(upBusParam))
      upD << downD.takeWhen(toUp).translateWith(downD.withDontCareData())
      upD.source.removeAssignments() := upSource
      upD.sink.removeAssignments() := 0 //TODO
      when(fromC){
        upD.opcode := Opcode.D.RELEASE_ACK
      }
      dispatchD += DispatchD(upD, upHits)
      when(downD.fire) {
        when(fromC){
          slotsC.onSel(downD.source.resized) { s =>
            s.fire := True
          }
          when(!slotCRelease){
            slots.onSel(slotCProbeId){slot =>
              slot.probe.waitAckData := False
            }
          }
        } otherwise {
          slots.onSel(downD.source.resized) { s =>
            s.fire := True
          }
        }
      }
    }
  }

  val bToT = new Area{
    val hits = B(slots.map(s => s.valid && s.aquireBtoT && s.probe.ports.map(_.empty).andR && !s.bToTRspSent))
    val oh = OHMasking.firstV2(hits)
    val source = slotSource(oh)
    val slotsId = OHToUInt(oh)
    val upHits = upSourceToNodeOh(source)
    val upD = Stream(TupleBundle(ChannelD(upBusParam), Bits(p.nodes.size bits)))
    upD.valid := hits.orR
    upD._1.opcode  := Opcode.D.GRANT
    upD._1.param   := Param.Cap.toT
    upD._1.source  := source
    upD._1.sink    := slotsId
    upD._1.size    := log2Up(blockSize)
    upD._1.denied  := False
    upD._1.corrupt := False
    upD._1.data.assignDontCare()
    upD._2 := upHits
    when(upD.ready){
      slots.onMask(oh){ s =>
        s.bToTRspSent := True
      }
    }

    val buffer = upD.halfPipe()
    dispatchD += DispatchD(buffer.translateWith(buffer._1), buffer._2)

    val ord = io.ordering.bToT
    ord.valid := upD.fire
    (ord.upId,  ord.upSource) := source
  }

  val mergeDownA = new Area{
    io.downGet.a << StreamArbiterFactory().lowerFirst.lambdaLock[ChannelA](_.isLast()).on(List(ctrl.aRead))
    io.downPut.a << StreamArbiterFactory().lowerFirst.lambdaLock[ChannelA](_.isLast()).on(List(ctrl.aWrite))
  }

  val dispatchUpD = new Area{
//    val groups = (0 until p.nodes.size).map(List(_)) //Full connection for now
    val groups = List((0 until p.nodes.size)) //Single node for now

    val demuxes = for(d <- dispatchD) yield StreamDemuxOh(d.d, groups.map(_.map(d.oh.apply).orR))
    val logics = for((group, groupId) <- groups.zipWithIndex) yield new Area{
      val inputs = demuxes.map(_(groupId))
      val arbiter = StreamArbiterFactory().lowerFirst.lambdaLock[ChannelD](_.isLast()).build(ChannelD(upBusParam), inputs.size).io
      (arbiter.inputs, inputs).zipped.map(_ << _)
      val nodeOh = (dispatchD, arbiter.chosenOH.asBools).zipped.map(_.oh.andMask(_)).reduceBalancedTree(_ | _)
      val dispatch = StreamDemuxOh(arbiter.output, group.map(nodeOh.apply))
      (group.map(upstream.buses.apply), dispatch).zipped.foreach{(s,m) =>
        s.d << m
        s.d.sink.removeAssignments() := m.sink.resized
      }
    }
  }
}




object CoherentHub{

  def downGetM2s(name : Nameable,
                 addressWidth : Int,
                 dataWidth : Int,
                 blockSize : Int,
                 slotCount : Int) = M2sParameters(
    addressWidth = addressWidth,
    dataWidth = dataWidth,
      masters = List(M2sAgent(
        name = name,
        mapping = List(M2sSource(
          id = SizeMapping(0, slotCount),
          emits = M2sTransfers(
            get = SizeRange.upTo(blockSize)
          )
        )
      ))
    )
  )

  def downPutM2s(name : Nameable,
                 addressWidth : Int,
                 dataWidth : Int,
                 blockSize : Int,
                 slotCount : Int,
                 cSourceCount : Int) = M2sParameters(
    addressWidth = addressWidth,
    dataWidth = dataWidth,
    masters = List(M2sAgent(
      name = name,
      mapping = List(M2sSource(
        id = SizeMapping(0, (1 << log2Up(slotCount max cSourceCount)) + cSourceCount),
        emits = M2sTransfers(
          putPartial = SizeRange.upTo(blockSize),
          putFull = SizeRange.upTo(blockSize)
        )
      )
      ))
    )
  )


  def randomConfig() = {
    val slotsCount = Random.nextInt(1)+1
    var r : CoherentHubParameters = null
    do {
      r = CoherentHubParameters(
        nodes = List.fill(Random.nextInt(8) + 1) {
          val m = M2sParameters(
            addressWidth = 13,
            dataWidth = 64,
            masters = {
              var offset = 0
              List.tabulate(Random.nextInt(4) + 1) { mId =>
                M2sAgent(
                  name = null,
                  mapping = List.fill(Random.nextInt(3) + 1) {
                    val idSize = Random.nextInt(3) + 1
                    val s = M2sSource(
                      emits = Random.nextInt(3) match {
                        case 0 => M2sTransfers(
                          get = SizeRange(64),
                          putFull = SizeRange(64),
                          putPartial = SizeRange(64),
                          acquireT = SizeRange(64),
                          acquireB = SizeRange(64),
                          probeAckData = SizeRange(64)
                        )
                        case 1 => M2sTransfers(
                          acquireT = SizeRange(64),
                          acquireB = SizeRange(64),
                          probeAckData = SizeRange(64)
                        )
                        case 2 => M2sTransfers(
                          get = SizeRange(64),
                          putFull = SizeRange(64),
                          putPartial = SizeRange(64)
                        )
                      },
                      id = SizeMapping(offset, idSize)
                    )
                    offset += idSize + Random.nextInt(5)
                    s
                  }
                )
              }
            }
          )

          NodeParameters(
            m = m,
            s = S2mParameters(List(
              S2mAgent(
                name = null,
                emits = S2mTransfers(
                  probe = if(m.withBCE) SizeRange(64) else SizeRange.none
                ),
                sinkId = SizeMapping(0, slotsCount)
              )
            ))
          )
        },
        slotCount = slotsCount,
        cacheSize = 1024,
        wayCount = 2,
        lineSize = 64
      )
    } while(!r.nodes.exists(_.m.masters.exists(_.mapping.exists(_.emits.withBCE))))
    r
  }

}
object CoherentHubGen extends App{
  def basicConfig(slotsCount : Int = 2, fullCount : Int = 1, coherentOnlyCount : Int = 0, dmaOnlyCount : Int = 0) = {
    CoherentHubParameters(
      nodes     = List.fill(fullCount)(
        NodeParameters(
          m = M2sParameters(
            addressWidth = 13,
            dataWidth = 64,
            masters = List.tabulate(4)(mId =>
            M2sAgent(
              name = null,
              mapping = List.fill(1)(M2sSource(
                emits = M2sTransfers(
                  get = SizeRange(64),
                  putFull = SizeRange(64),
                  putPartial = SizeRange(64),
                  acquireT = SizeRange(64),
                  acquireB = SizeRange(64),
                  probeAckData = SizeRange(64)
                ),
                id = SizeMapping(mId*4, 4)
              ))
            ))
          ),
          s = S2mParameters(List(
            S2mAgent(
              name = null,
              emits = S2mTransfers(
                probe = SizeRange(64)
              ),
              sinkId = SizeMapping(0, slotsCount)
            )
          ))
        )
      ) ++  List.fill(coherentOnlyCount)(
        NodeParameters(
          m = M2sParameters(
            addressWidth = 13,
            dataWidth = 64,
            masters = List.tabulate(4)(mId =>
            M2sAgent(
              name = null,
              mapping = List.fill(1)(M2sSource(
                emits = M2sTransfers(
                  acquireT = SizeRange(64),
                  acquireB = SizeRange(64),
                  probeAckData = SizeRange(64)
                ),
                id = SizeMapping(mId*4, 4)
              ))
            ))
          ),
          s = S2mParameters(List(
            S2mAgent(
              name = null,
              emits = S2mTransfers(
                probe = SizeRange(64)
              ),
              sinkId = SizeMapping(0, slotsCount)
            )
          ))
        )
      ) ++  List.fill(dmaOnlyCount)(
        NodeParameters(
          m = M2sParameters(
            addressWidth = 13,
            dataWidth = 64,
            masters = List.tabulate(4)(mId =>
            M2sAgent(
              name = null,
              mapping = List.fill(1)(M2sSource(
                emits = M2sTransfers(
                  get = SizeRange(64),
                  putFull = SizeRange(64),
                  putPartial = SizeRange(64)
                ),
                id = SizeMapping(mId*4, 4)
              ))
            ))
          ),
          s = S2mParameters(Nil)
        )
      ),
      slotCount = slotsCount,
      cacheSize = 1024,
      wayCount  = 2,
      lineSize  = 64
    )
  }
  def gen = new CoherentHub(basicConfig(8))
  SpinalVerilog(gen)

}
/*
TODO warning :
- probe_data need to be properly handled
- If C to D data bypass is implemented, then the slot should be hold until dirty data is cleanedup. Also Future C on matching address should be holded until data is cleanedup !
 */