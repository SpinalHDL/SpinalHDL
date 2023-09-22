package spinal.lib.bus.tilelink.coherent

import spinal.core._
import spinal.lib._
import spinal.lib.bus.misc.SizeMapping
import spinal.lib.bus.tilelink._
import spinal.lib.pipeline._

import scala.collection.mutable.ArrayBuffer

object Hub{
  def downM2s(name : Nameable,
              addressWidth : Int,
              dataWidth : Int,
              blockSize : Int,
              downPendingMax : Int) = M2sParameters(
    addressWidth = addressWidth,
    dataWidth = dataWidth,
    masters = List(M2sAgent(
      name = name,
      mapping = List(M2sSource(
        id = SizeMapping(0, 1 << log2Up(downPendingMax)),
        emits = M2sTransfers(
          putPartial = SizeRange.upTo(blockSize),
          putFull = SizeRange.upTo(blockSize),
          get = SizeRange.upTo(blockSize)
        )
      )
      ))
    )
  )

  def upS2m(name : Nameable,
            blockSize : Int,
            setCount : Int) = S2mParameters(List(
    S2mAgent(
      name = name,
      emits = S2mTransfers(
        probe = SizeRange(blockSize)
      ),
      sinkId = SizeMapping(0, setCount*2)
    )
  ))
}

case class HubParameters(var unp : NodeParameters,
                         var downPendingMax : Int,
                         var sets: Int,
                         var wayCount: Int,
                         var blockSize : Int,
                         var probeCount : Int = 8,
                         var aBufferCount: Int = 4,
                         var probeRegion : UInt => Bool) {
  def cacheSize = sets*wayCount*blockSize
  def addressWidth = unp.m.addressWidth
  def dataWidth = unp.m.dataWidth
  def dataBytes = dataWidth/8
  def waySize = cacheSize/wayCount
  def linePerWay = waySize/lineSize
  def tagRange = addressWidth-1 downto log2Up(linePerWay*lineSize)
  def lineRange = tagRange.low-1 downto log2Up(lineSize)
  def wordRange = log2Up(lineSize)-1 downto log2Up(dataBytes)
  def refillRange = tagRange.high downto lineRange.low
  def blockRange = addressWidth-1 downto log2Up(lineSize)
  def lineSize = blockSize
  def setsRange = lineRange
}

case class OrderingCmd(bytesMax : Int) extends Bundle{
  val debugId = DebugId()
  val bytes = UInt(log2Up(bytesMax+1) bits)
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
- writes without ownership on hit
- downstream D (on refill)
- release hit

drive upstream D
- cache hit read
- downstream D (include cache miss refill)


A transaction will always :
- Wait no slot conflicts
- [Fetch tags]
- [Probe stuff]
- [wait probe completion]
- aquire:
  - [writeback/fill cache cmd]
  - [Read data from $]/[GrandData down D to up D]/[grant] + [update tags]
  - wait up.E grant ack [and writeback] completion
- get / put
  - [writeback/fill cache cmd]/[down A]
  - [Read data from $]/[down D to up D]

C : release data
- [Fetch tags]
- on hit [writeback cache] else write write to main mem
- wait completion


down.D may trigger:
- up.D redirection (on up get/put/aquireBlock with data/releaseData), need source context
- conflict release (on up get/put), need sets context
- probe bypass (on ProbeData), need probeId context

up.E will do a conflict release (aquireAck)


$ =>
- [dma no allocate]
- [release allocate]
- [victim buffer]
- [inclusive]

Inclusive tricky things :
- Allocation require making room
- Allocation while a release is also on the way (but easily solved by not allowing same set in the pipeline)

non inclusive :
- Probe filtering via a Set counter memory

 */


/*
Currently when a io region access is done, the HUB will still lock the memory block (but that's not necessary)
 */
class Hub(p : HubParameters) extends Component{
  import p._
  val ubp = p.unp.toBusParameter()
  val dbp = NodeParameters(
    m = Hub.downM2s(
      name           = this,
      addressWidth   = addressWidth,
      dataWidth      = dataWidth,
      blockSize      = blockSize,
      downPendingMax = downPendingMax
    ),
    s = S2mParameters(slaves = Nil)
  ).toBusParameter()

  val io = new Bundle{
    val up = slave(Bus(ubp))
    val down = master(Bus(dbp))
    val backendOrdering = master(Flow(OrderingCmd(up.p.sizeBytes)))
    val probeOrdering = master(Flow(OrderingCmd(up.p.sizeBytes)))
  }

  this.addTag(OrderingTag(io.backendOrdering))
  this.addTag(OrderingTag(io.probeOrdering))


  case class DataPayload() extends Bundle {
    val mask = ubp.mask()
    val data = ubp.data()
  }

  val FROM_C = Stageable(Bool())
  val ADDRESS  = Stageable(ubp.address)
//  val CAP  = Stageable(UInt(2 bits))
  val SET_ID  = Stageable(UInt(setsRange.size bits))
  val PROBE_ID  = Stageable(UInt(log2Up(probeCount) bits))
  val SLOT_ALLOCATE = Stageable(Bool())
  val SIZE  = Stageable(ubp.size)
  val SOURCE  = Stageable(ubp.source)
  val BUFFER_ID = Stageable(UInt(log2Up(aBufferCount) bits))
  val PARTIAL_DATA = Stageable(Bool())
  val WRITE_DATA = Stageable(Bool())
  val READ_DATA = Stageable(Bool())
  val LAST, FIRST = Stageable(Bool())
  val PAYLOAD_C, PAYLOAD = Stageable(DataPayload())
  val SLOT_ID = Stageable(UInt(log2Up(downPendingMax) bits))
  val TO_DOWN = Stageable(Bool())
  val FROM_NONE = Stageable(Bool())
  val UNIQUE = Stageable(Bool())
  val CONFLICT_CTX = Stageable(Bool())

  val initializer = new Area{
    val initCycles = p.sets
    val counter = Reg(UInt(log2Up(initCycles) + 1 bits)) init(0)
    val done = counter.msb
    when(!done) {
      counter := counter + 1
    }
  }

  val slots = new Area{
    val valids = Reg(Bits(downPendingMax bits)) init(0)
    val freeOh = OHMasking.firstV2(~valids)
    val freeId = OHToUInt(freeOh)
    val full = valids.andR
    val allocate = Bool()
    val release = Flow(UInt(log2Up(downPendingMax) bits))
    valids := (valids | freeOh.andMask(allocate)) & ~(UIntToOh(release.payload).andMask(release.valid))
    val ctx = Mem.fill(downPendingMax)(Bits((widthOf(CtxA()) max widthOf(CtxC())) + 1 bits))
    val ctxWrite = ctx.writePort()
  }

  val setsBusy = new Area{
    class MemArea extends Area {
      val mem = Mem.fill(sets)(Bool())
      val write = mem.writePort()
      write.valid := !initializer.done
      write.address := initializer.counter.resized
      write.data := False
    }
    val target = new MemArea
    val hit = new MemArea {
      val upE = Stream(write.payload)
      val downD = Stream(write.payload)
      val arbiter = StreamArbiterFactory().noLock.lowerFirst.onArgs(upE, downD).toFlow
      when(initializer.done) {
        write << arbiter
      }
    }
  }

  val frontendA = new Pipeline{
    val stages = newChained(2, Connection.M2S())

    val PAYLOAD = Stageable(DataPayload())
    val BUFFER_ID = Stageable(UInt(log2Up(aBufferCount) bits))

    val buffer =  ubp.withDataA generate new Area{
      val ram = Mem.fill(aBufferCount*p.blockSize/p.unp.m.dataBytes)(DataPayload())
      val allocated = Reg(Bits(aBufferCount bits)) init(0)
      val set, clear = B(0, aBufferCount bits)
      val firstFree = OHToUInt(OHMasking.firstV2(~allocated))
      val full = allocated.andR
      val source = Vec.fill(aBufferCount)(Reg(ubp.source))
      val write = ram.writePort()
      allocated := (allocated | set) & ~clear
    }

    val spawn = new Area{
      val stage = stages(0)
      import stage._

      driveFrom(io.up.a)
      haltWhen(!initializer.done)

      val CMD = insert(io.up.a.payload.asNoData())
      for(i <- 0 until ubp.sizeMax) CMD.address(i) clearWhen(CMD.size > i)
      val LAST = io.up.a.isLast()
      if(ubp.withDataA) {
        PAYLOAD.data := io.up.a.data
        PAYLOAD.mask := io.up.a.mask
      }
      val dataSplit = ubp.withDataA generate new Area{
        val withBeats = io.up.a.payload.withBeats
        val hazard = withBeats && buffer.full
        haltIt(hazard)
        throwIt(!hazard && !LAST)

        val locked = RegInit(False) setWhen(buffer.write.valid)
        val lockedValue = RegNextWhen(buffer.firstFree, !locked)
        BUFFER_ID := locked ? lockedValue | buffer.firstFree
        buffer.write.valid := valid && withBeats && !hazard
        buffer.write.address := BUFFER_ID @@ io.up.a.payload.address(wordRange)
        buffer.write.data := PAYLOAD
        when(isFireing && LAST && withBeats){
          buffer.set(BUFFER_ID) := True
          buffer.source.onSel(BUFFER_ID)(_ := io.up.a.payload.source)
          locked := False
        }
      }
    }

    val readConflicts = new Area{
      val stage = stages(0)
      import stage._

      val readAddress = spawn.CMD.address(setsRange)
      val JUST_BUSY = insert(setsBusy.target.write.valid && setsBusy.target.write.address === readAddress)
      val JUST_FREE = insert(setsBusy.hit.write.valid && setsBusy.hit.write.address === readAddress)
    }

    val checkConflicts = new Area{
      val stage = stages(1)
      import stage._
      import readConflicts._

      val hitPort = setsBusy.hit.mem.readSyncPort()
      hitPort.cmd.valid := !stage.isStuck
      hitPort.cmd.payload := readAddress
      hitPort.writeFirstAndUpdate(setsBusy.hit.write)

      val targetPort = setsBusy.target.mem.readSyncPort()
      targetPort.cmd.valid := !stage.isStuck
      targetPort.cmd.payload := readAddress
      targetPort.writeFirstAndUpdate(setsBusy.target.write)

      val hit    = hitPort.rsp
      val target = targetPort.rsp
      val hazard = hit =/= target
      haltIt(hazard)

      when(initializer.done) {
        setsBusy.target.write.valid := isFireing
        setsBusy.target.write.address := spawn.CMD.address(setsRange)
        setsBusy.target.write.data := !target
      }

      CONFLICT_CTX := !target
    }
  }

  val coherentMasters = unp.m.masters.filter(_.emits.withBCE)
  val coherentMasterCount = coherentMasters.size
  val coherentMasterToSource = coherentMasters.map(_.bSourceId)
  def MasterId() = UInt(log2Up(coherentMasterCount) bits)
  class ProbeCtx extends Bundle{
    val opcode  = Opcode.A()
    val address = ubp.address()
    val size    = ubp.size()
    val toTrunk = Bool()
    val source  = ubp.source()
    val debugId  = DebugId()
    val bufferId = ubp.withDataA generate UInt(log2Up(aBufferCount) bits)
    val conflictCtx = CONFLICT_CTX()
  }
  class ProbeCtxFull extends ProbeCtx{
    val probeId = UInt(log2Up(probeCount) bits)
  }

  case class ProbeCmd() extends Bundle {
    val ctx = new ProbeCtx()
    val fromNone = Bool()
    val selfMask = Bits(coherentMasterCount bits)
    val mask = Bits(coherentMasterCount bits)
  }

  case class CtxA() extends Bundle{
    val source = ubp.source()
    val set = SET_ID()
    val getPut = Bool()
    val toTrunk = Bool()
    val conflictCtx = CONFLICT_CTX()
  }
  case class CtxC() extends Bundle{
    val source = ubp.source()
    val isProbeData = Bool()
    def isReleaseData = !isProbeData
    val probeId = PROBE_ID()
  }

  val probe = new Area{
    val isl = frontendA.stages.last
    val push = Stream(ProbeCmd())
    val pushCmd = isl(frontendA.spawn.CMD)
    val isAcquire = Opcode.A.isAcquire(pushCmd.opcode)
    push.valid := isl.isValid && !isl.internals.request.halts.orR
    isl.haltIt(!push.ready)
    push.ctx.opcode     := pushCmd.opcode
    push.ctx.address    := pushCmd.address
    push.ctx.toTrunk    := pushCmd.param =/= Param.Grow.NtoB
    push.ctx.debugId    := pushCmd.debugId
    if(ubp.withDataA) push.ctx.bufferId := isl(frontendA.BUFFER_ID)
    push.ctx.conflictCtx := isl(CONFLICT_CTX)
    push.ctx.size       := pushCmd.size
    push.ctx.source     := pushCmd.source
    push.fromNone := pushCmd.param =/= Param.Grow.BtoT && isAcquire
    push.selfMask   := B(coherentMasters.map(_.sourceHit(pushCmd.source))).andMask(isAcquire)
    push.mask := ~push.selfMask
    when(!push.fromNone){
      push.mask.setAll()
    }
    val isProbeRegion = p.probeRegion(pushCmd.address)
    when(!isProbeRegion){
      push.mask.clearAll()
    }


    val slots = for(i <- 0 until probeCount) yield new Area{
      val fire = False
      val valid = RegInit(False) clearWhen(fire)
      val set = Reg(UInt(setsRange.size bits))
      val pending = Reg(UInt(log2Up(coherentMasterCount+1) bits))
      val fromNone = Reg(Bool())
      val unique = Reg(Bool())
      val probeAckDataCompleted = Reg(Bool())
      val selfId = Reg(UInt(log2Up(coherentMasterCount) bits))
      val done = valid && (pending & ~U(probeAckDataCompleted, widthOf(pending) bits)) === 0
    }

    val ctxMem = Mem.fill(probeCount)(new ProbeCtx())
    val ctxWrite = ctxMem.writePort()

    val cmd = new Area{
      val sloted = RegInit(False)
      val full = slots.map(_.valid).andR
      val freeMask = OHMasking.firstV2(B(slots.map(!_.valid)))
      val freeId = OHToUInt(freeMask)
      val pending = CountOne(push.mask)
      val selfId = OHToUInt(push.selfMask)

      ctxWrite.valid := False
      ctxWrite.address := freeId
      ctxWrite.data := push.ctx
      when(push.valid && !sloted && !full){
        slots.onMask(freeMask){s =>
          s.valid := True
          s.set := push.ctx.address(setsRange)
          s.pending := pending
          s.fromNone := push.fromNone
          s.unique := True
          s.probeAckDataCompleted := False
          s.selfId := selfId
        }
        ctxWrite.valid := True
        sloted := True
      }

      val bus = io.up.b
      val halted = push.haltWhen(!sloted && full)
      val fired = Reg(Bits(coherentMasterCount bits)) init(0)
      val isPut = Opcode.A.isPut(halted.ctx.opcode)
      val isAcquire = Opcode.A.isAcquire(halted.ctx.opcode)
      val requests = push.mask & ~fired
      val masterOh = OHMasking.firstV2(requests)
      val selfProbe = (masterOh & halted.selfMask).orR
      val removeBranches = isPut || isAcquire && halted.ctx.toTrunk && !selfProbe
      bus.valid   := halted.valid && requests.orR
      bus.opcode  := Opcode.B.PROBE_BLOCK
      bus.param   := removeBranches ? B(Param.Cap.toN, 3 bits) | B(Param.Cap.toB, 3 bits)
      bus.source  := OhMux(masterOh, coherentMasterToSource.map(id => U(id, ubp.sourceWidth bits)).toList)
      bus.address := halted.ctx.address(blockRange) @@ U(0, blockRange.low bits)
      bus.size    := log2Up(p.blockSize)
      halted.ready := requests === 0

      when(bus.fire){
        fired.asBools.onMask(masterOh)(_ := True)
      }

      when(halted.ready){
        fired := 0
        sloted := False
      }
    }

    val rsp = new Area{
      val bypass = Flow(UInt(log2Up(probeCount) bits))
      val input = io.up.c.haltWhen(bypass.valid)
      val ack = input.fire && input.opcode === Opcode.C.PROBE_ACK
      val hitOh = slots.map(s => s.valid && s.set === input.address(setsRange))
      val hitId = OHToUInt(hitOh)
      val pending = slots.map(_.pending).read(hitId)
      val selfId = slots.map(_.selfId).read(hitId)
      val pendingNext = pending - 1
      val masterOh = coherentMasterToSource.map(input.source === _).asBits
      val masterId = OHToUInt(masterOh)
      val isSelf = selfId === masterId
      val lostIt = isSelf && input.param === Param.Report.NtoN

      when(bypass.valid){
        hitId := bypass.payload
        lostIt := False
      }

      when(ack || bypass.valid) {
        slots.onSel(hitId) { s =>
          s.pending := pendingNext
          s.fromNone setWhen (lostIt)
        }
      }

      when(input.fire && (input.opcode === Opcode.C.PROBE_ACK || input.opcode === Opcode.C.PROBE_ACK_DATA)){
        when(!isSelf && Param.reportPruneKeepCopy(input.param)){
          slots.onSel(hitId) { s =>
            s.unique := False
          }
        }
      }

      val toBackend = cloneOf(input)
      val toBackendProbeId = hitId
      val hitBackend = !Opcode.C.withoutData(input.opcode)
      toBackend.valid := input.valid && hitBackend
      toBackend.payload := input.payload

      val hitUpD = input.opcode === Opcode.C.RELEASE
      val toUpD = Stream(io.up.d.payloadType)
      toUpD.valid := input.valid && hitUpD
      toUpD.opcode  := Opcode.D.RELEASE_ACK
      toUpD.param   := 0
      toUpD.source  := input.source
      toUpD.size    := input.size
      toUpD.denied  := False
      toUpD.corrupt := False
      toUpD.data.assignDontCare()
      toUpD.sink.assignDontCare()

      input.ready := !(hitBackend && !toBackend.ready) && !(hitUpD && !toUpD.ready)
    }

    val wake = new Pipeline{
      val stages = newChained(2, Connection.M2S())
      val CTX = Stageable(new ProbeCtx())
      val toBackend = Stream(new ProbeCtxFull())
      val toUpD = cloneOf(io.up.d)

      val insertion = new Area {
        val stage = stages(0)
        import stage._

        val hits = slots.map(_.done).asBits
        valid := hits.orR
        val hitOh = OHMasking.roundRobinNext(hits, isFireing)
        val hitId = OHToUInt(hitOh)
        PROBE_ID := hitId
        when(isFireing){
          slots.onMask(hitOh){ s =>
            s.fire := True
          }
        }
        FROM_NONE := OhMux.or(hitOh, slots.map(_.fromNone))
        UNIQUE := OhMux.or(hitOh, slots.map(_.unique))
      }

      val ctxFetcher = new Area {
        val stage = stages(1)
        import stage._

        stage(CTX) := ctxMem.readSync(insertion.hitId, !isStuck)
        val hitUpD = CTX.opcode === Opcode.A.ACQUIRE_PERM ||
                     CTX.opcode === Opcode.A.ACQUIRE_BLOCK && !FROM_NONE

        toBackend.valid := isValid && !hitUpD
        toBackend.payload.assignSomeByName(CTX)
        toBackend.probeId := PROBE_ID
        when(UNIQUE){
          toBackend.toTrunk := True
        }

        toUpD.valid := isValid && hitUpD
        toUpD.opcode  := Opcode.D.GRANT
        toUpD.param   := 0
        toUpD.source  := CTX.source
        toUpD.size    := CTX.size
        toUpD.sink    := U(CTX.conflictCtx ## CTX.address(setsRange))
        toUpD.denied  := False
        toUpD.corrupt := False
        toUpD.data.assignDontCare()

        io.probeOrdering.valid := toUpD.fire
        io.probeOrdering.debugId := CTX.debugId
        io.probeOrdering.bytes := (U(1) << toUpD.size).resized

        haltWhen(hitUpD ? !toUpD.ready | !toBackend.ready)
      }
    }
  }



  val backend = new Pipeline {
    val stages = newChained(3, Connection.M2S())

    val insertion = new Area{
      val stage = stages(0)
      import stage._
      val c = probe.rsp.toBackend
      val cProbeId = probe.rsp.toBackendProbeId
      val a = probe.wake.toBackend

      val lockValid = RegInit(False) setWhen(valid) clearWhen(isFireing && LAST)
      val lockSel = Reg(Bool())
      val selC = lockValid ? lockSel | c.valid
      lockSel := selC


      val counter = Reg(UInt(wordRange.size bits)) init(0)
      when(isFireing){
        counter := counter + 1
        when(LAST){
          counter := 0
        }
      }

      valid := selC ? c.valid | a.valid
      FROM_C :=  selC
      when(selC){
        WRITE_DATA := c.withBeats
        READ_DATA := False
        PARTIAL_DATA := False
        ADDRESS := c.address
        SIZE := log2Up(blockSize)
        SOURCE := c.source
        PROBE_ID := cProbeId
      } otherwise {
        WRITE_DATA := Opcode.A.isPut(a.opcode)
        READ_DATA  := Opcode.A.isGet(a.opcode) || a.opcode === Opcode.A.ACQUIRE_BLOCK
        PARTIAL_DATA := a.opcode === Opcode.A.PUT_PARTIAL_DATA
        ADDRESS := a.address
        ADDRESS(wordRange) := a.address(wordRange) + counter
        SIZE := a.size
        SOURCE := a.source
        PROBE_ID := a.probeId
      }
      LAST := READ_DATA || WRITE_DATA && counter === sizeToBeatMinusOne(ubp, SIZE)
      if(ubp.withDataA) BUFFER_ID := a.bufferId
      CONFLICT_CTX := a.conflictCtx

      c.ready := isReady &&  selC
      a.ready := isReady && !selC && LAST

      PAYLOAD_C.data := c.data
      PAYLOAD_C.mask.setAll()

      val first = RegNextWhen[Bool](LAST, isFireing) init(True)
      FIRST := first

      val A_GET_PUT  = insert(Opcode.A.isGetPut(a.opcode))
      val A_TO_TRUNK = insert(a.toTrunk)
      val C_IS_PROBE_DATA = insert(c.opcode === Opcode.C.PROBE_ACK_DATA)

      //need update when $
      TO_DOWN := WRITE_DATA | READ_DATA
      SLOT_ALLOCATE := TO_DOWN

      io.backendOrdering.valid := isFireing && !FROM_C
      io.backendOrdering.debugId := a.debugId
      io.backendOrdering.bytes := (U(1) << a.size).resized
    }

    val aPayloadSyncRead = new Area{
      val s0 = stages(0)
      val s1 = stages(1)
      val wordAddress = s0(BUFFER_ID) @@ s0(ADDRESS)(wordRange)

      s1(PAYLOAD) := s1(PAYLOAD_C)
      if(ubp.withDataA) {
        when(!s1(FROM_C)){
          s1(PAYLOAD) := frontendA.buffer.ram.readSync(wordAddress, !s1.isStuck)
        }
        when(s1.isFireing && s1(LAST) && !s1(FROM_C) && s1(WRITE_DATA)){
          frontendA.buffer.clear(s1(BUFFER_ID)) := True
        }
      }
    }

    val allocateSlot = new Area{
      val stage = stages(0)
      import stage._

      val slotLock = RegNextWhen(!LAST, isFireing) init(False)
      val slotReg = RegNextWhen(slots.freeId, !slotLock)
      SLOT_ID := slotLock ? slotReg | slots.freeId
      haltWhen(SLOT_ALLOCATE && slots.full)

      val a = CtxA()
      a.source := SOURCE
      a.set    := ADDRESS(setsRange)
      a.getPut  := insertion.A_GET_PUT
      a.toTrunk := insertion.A_TO_TRUNK
      a.conflictCtx := CONFLICT_CTX

      val c = CtxC()
      c.source      := SOURCE
      c.isProbeData := insertion.C_IS_PROBE_DATA
      c.probeId     := PROBE_ID

      slots.allocate := isFireing && SLOT_ALLOCATE && FIRST
      slots.ctxWrite.valid := isFireing && SLOT_ALLOCATE
      slots.ctxWrite.address := SLOT_ID
      slots.ctxWrite.data.assignDontCare()
      slots.ctxWrite.data(0) := FROM_C
      when(FROM_C){
        slots.ctxWrite.data(1, widthOf(c) bits) := B(c)
      } otherwise {
        slots.ctxWrite.data(1, widthOf(a) bits) := B(a)
      }
      assert(widthOf(a) < widthOf(slots.ctxWrite.data))
      assert(widthOf(c) < widthOf(slots.ctxWrite.data))
    }

    val toDown = new Area{
      val stage = stages.last
      import stage._
      haltWhen(TO_DOWN && !io.down.a.ready)
      io.down.a.valid := valid && TO_DOWN
      io.down.a.opcode := WRITE_DATA ? (PARTIAL_DATA ? Opcode.A.PUT_PARTIAL_DATA | Opcode.A.PUT_FULL_DATA) | Opcode.A.GET
      io.down.a.param := 0
      io.down.a.source := SLOT_ID
      io.down.a.address := ADDRESS
      io.down.a.size := SIZE
      io.down.a.data := PAYLOAD.data
      io.down.a.mask := PAYLOAD.mask
      io.down.a.corrupt := False
      io.down.a.debugId := DebugId.withPostfix(io.down.a.source)
    }
  }

  val downD = new Pipeline{
    val stages = newChained(2, Connection.M2S())
    val D_PAYLOAD = Stageable(io.down.d.payloadType)
    val CTX = Stageable(slots.ctx.wordType)

    val insert = new Area{
      val stage = stages.head
      stage.driveFrom(io.down.d)
      stage(D_PAYLOAD) := io.down.d
      stage(LAST) := io.down.d.isLast()
    }

    val ctxReadSync = new Area{
      val s0 = stages(0)
      val s1 = stages(1)
      val wordAddress = s0(D_PAYLOAD).source
      s1(CTX) := slots.ctx.readSync(wordAddress, !s1.isStuck)
    }

    def decodeCtx(ctx : Bits) = new Area{
      val fromC = ctx(0)
      val a = (ctx >> 1).resized.as(CtxA())
      val c = (ctx >> 1).resized.as(CtxC())
      val hit = fromC ? (!c.isProbeData) | True
    }

    val slotRelease = new Area{
      val stage = stages(1)
      import stage._
      val ctx = decodeCtx(CTX)

      val fired = RegInit(False) setWhen(isValid) clearWhen(isReady)

      slots.release.valid := valid && !fired && LAST
      slots.release.payload := D_PAYLOAD.source
    }

    val toSetsBusy = new Area{
      val stage = stages(1)
      import stage._

      val ctx = decodeCtx(CTX)
      val fired = RegInit(False) setWhen(setsBusy.hit.downD.fire) clearWhen(isReady)
      val hit = !ctx.fromC && ctx.a.getPut && LAST && !fired

      setsBusy.hit.downD.valid := valid && hit
      setsBusy.hit.downD.address := ctx.a.set
      setsBusy.hit.downD.data := ctx.a.conflictCtx

      haltIt(hit && !setsBusy.hit.downD.ready)
    }

    val toProbe = new Area{
      val stage = stages(1)
      import stage._

      val ctx = decodeCtx(CTX)
      val hit = ctx.fromC && ctx.c.isProbeData
      val fired = RegInit(False) setWhen(probe.rsp.bypass.fire) clearWhen(isReady)
      probe.rsp.bypass.valid := valid && hit && ! fired
      probe.rsp.bypass.payload := ctx.c.probeId
    }

    val toUp = new Area{
      val stage = stages(1)
      import stage._

      val upD = cloneOf(io.up.d)

      val ctx = decodeCtx(CTX)
      val hit = ctx.fromC ? (!ctx.c.isProbeData) | True
      val fired = RegInit(False) setWhen(upD.fire) clearWhen(isReady)
      upD.valid := isValid && hit && !fired
      upD.payload := D_PAYLOAD
      upD.param.removeAssignments() := 0
      upD.source.removeAssignments()
      upD.sink.removeAssignments() := U(ctx.a.conflictCtx ## ctx.a.set)
      when(ctx.fromC){
        upD.opcode := Opcode.D.RELEASE_ACK
        upD.source := ctx.c.source
      } otherwise {
        upD.source := ctx.a.source
        when(!ctx.a.getPut) {
          upD.opcode := Opcode.D.GRANT_DATA
          upD.param := ctx.a.toTrunk ? B(Param.Cap.toT, 3 bits) | B(Param.Cap.toB, 3 bits)
        }
      }
      haltIt(!fired && hit && !upD.ready)
    }
  }

  val upD = new Area{
    val arbitred = StreamArbiterFactory().lowerFirst.lambdaLock[ChannelD](_.isLast()).onArgs(
      downD.toUp.upD,
      probe.rsp.toUpD.stage(),
      probe.wake.toUpD.stage()
    )
    io.up.d << arbitred
  }

  val upE = new Area{
    setsBusy.hit.upE.arbitrationFrom(io.up.e)
    setsBusy.hit.upE.address := io.up.e.sink.resized
    setsBusy.hit.upE.data := io.up.e.sink.msb
  }

  frontendA.build()
  probe.wake.build()
  backend.build()
  downD.build()
}



object HubGen extends App{
  def basicConfig(probeCount : Int = 8, downPendingMax : Int = 16, masterPerChannel : Int = 4, dataWidth : Int = 64, addressWidth : Int = 13, setCount : Int = 256, wayCount : Int = 2, lineSize : Int = 64) = {
    val blockSize = 64
    HubParameters(
      unp = NodeParameters(
        m = M2sParameters(
          addressWidth = addressWidth,
          dataWidth = dataWidth,
          masters = List.tabulate(masterPerChannel)(mId =>
            M2sAgent(
              name = null,
              mapping = List.fill(1)(M2sSource(
                emits = M2sTransfers(
                  get = SizeRange(64),
                  putFull = SizeRange(64),
                  putPartial = SizeRange(64),
                  acquireT = SizeRange(64),
                  acquireB = SizeRange(64)
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
            sinkId = SizeMapping(0, setCount*2)
          )
        ))
      ),
      downPendingMax = downPendingMax,
      probeCount = probeCount,
      sets = setCount,
      wayCount  = wayCount,
      blockSize = blockSize,
      probeRegion = _ => True
    )
  }
  def gen = new Hub(HubGen.basicConfig(8, masterPerChannel = 4, dataWidth = 16, addressWidth = 32))
  SpinalVerilog(gen)

}

object HubSynt extends App{
  import spinal.lib.eda.bench._
  val rtls = ArrayBuffer[Rtl]()
  for(probeCount <- List(2, 8, 16)) { //Rtl.ffIo
    rtls += Rtl(SpinalVerilog((new Hub(HubGen.basicConfig(probeCount = probeCount, masterPerChannel = 4, dataWidth = 16, addressWidth = 32)).setDefinitionName(s"Hub$probeCount"))))
  }
  val targets = XilinxStdTargets().take(2) ++ AlteraStdTargets(quartusCycloneIIPath = null)

  Bench(rtls, targets)
}


object HubSyntLight extends App{
  import spinal.lib.eda.bench._
  val rtls = ArrayBuffer[Rtl]()
  rtls += Rtl(SpinalVerilog((new Hub(HubGen.basicConfig(
    probeCount = 2,
    downPendingMax = 4,
    masterPerChannel = 2,
    dataWidth = 16,
    addressWidth = 32
  )).setDefinitionName(s"Hub"))))

  val targets = XilinxStdTargets().take(2) ++ AlteraStdTargets(quartusCycloneIIPath = null)

  Bench(rtls, targets)
  /*

Artix 7 -> 180 Mhz 336 LUT 381 FF
Artix 7 -> 315 Mhz 404 LUT 381 FF
Cyclone V -> FAILED
Cyclone IV -> 152 Mhz 488 LUT 524 FF

   */
}

/*
Hub16 ->
Artix 7 -> 163 Mhz 164 LUT 215 FF
Artix 7 -> 268 Mhz 180 LUT 215 FF
Cyclone V -> FAILED
Cyclone IV -> 181 Mhz 217 LUT 214 FF

Hub16 ->
Artix 7 -> 165 Mhz 160 LUT 215 FF
Artix 7 -> 269 Mhz 174 LUT 215 FF
Cyclone V -> FAILED
Cyclone IV -> 181 Mhz 217 LUT 214 FF

Hub16 ->
Artix 7 -> 198 Mhz 218 LUT 182 FF
Artix 7 -> 280 Mhz 250 LUT 182 FF
Cyclone V -> 192 Mhz 155 ALMs
Cyclone IV -> 169 Mhz 300 LUT 215 FF

Artix 7 -> 145 Mhz 251 LUT 206 FF
Artix 7 -> 253 Mhz 281 LUT 206 FF
Cyclone V -> 183 Mhz 180 ALMs
Cyclone IV -> 162 Mhz 341 LUT 239 FF




Hub2 ->
Artix 7 -> 212 Mhz 167 LUT 237 FF
Artix 7 -> 357 Mhz 179 LUT 237 FF
Cyclone V -> 214 Mhz 109 ALMs
Cyclone IV -> 180 Mhz 224 LUT 307 FF
Hub8 ->
Artix 7 -> 151 Mhz 296 LUT 333 FF
Artix 7 -> 234 Mhz 326 LUT 333 FF
Cyclone V -> 193 Mhz 201 ALMs
Cyclone IV -> 147 Mhz 550 LUT 625 FF
Hub16 ->
Artix 7 -> 117 Mhz 476 LUT 461 FF
Artix 7 -> 205 Mhz 540 LUT 461 FF
Cyclone V -> FAILED
Cyclone IV -> 135 Mhz 569 LUT 420 FF


Hub2 ->
Artix 7 -> 178 Mhz 225 LUT 327 FF
Artix 7 -> 327 Mhz 247 LUT 327 FF
Cyclone V -> FAILED
Cyclone IV -> 176 Mhz 319 LUT 351 FF
Hub8 ->
Artix 7 -> 162 Mhz 351 LUT 423 FF
Artix 7 -> 241 Mhz 385 LUT 423 FF
Cyclone V -> FAILED
Cyclone IV -> 152 Mhz 641 LUT 681 FF
Hub16 ->
Artix 7 -> 103 Mhz 526 LUT 551 FF
Artix 7 -> 191 Mhz 607 LUT 551 FF
Cyclone V -> FAILED
Cyclone IV -> 138 Mhz 654 LUT 458 FF

Hub2 ->
Artix 7 -> 216 Mhz 238 LUT 327 FF
Artix 7 -> 363 Mhz 255 LUT 327 FF
Cyclone V -> FAILED
Cyclone IV -> 184 Mhz 315 LUT 349 FF
Hub8 ->
Artix 7 -> 144 Mhz 345 LUT 423 FF
Artix 7 -> 250 Mhz 386 LUT 423 FF
Cyclone V -> FAILED
Cyclone IV -> 151 Mhz 650 LUT 673 FF
Hub16 ->
Artix 7 -> 124 Mhz 598 LUT 551 FF
Artix 7 -> 188 Mhz 698 LUT 551 FF
Cyclone V -> FAILED
Cyclone IV -> 133 Mhz 674 LUT 459 FF


Hub2 ->
Artix 7 -> 153 Mhz 276 LUT 338 FF
Artix 7 -> 339 Mhz 313 LUT 338 FF
Cyclone V -> FAILED
Cyclone IV -> 147 Mhz 351 LUT 389 FF
Hub8 ->
Artix 7 -> 103 Mhz 392 LUT 436 FF
Artix 7 -> 228 Mhz 448 LUT 436 FF
Cyclone V -> FAILED
Cyclone IV -> 123 Mhz 689 LUT 721 FF
Hub16 ->
Artix 7 -> 119 Mhz 635 LUT 565 FF
Artix 7 -> 188 Mhz 738 LUT 565 FF
Cyclone V -> FAILED
Cyclone IV -> 115 Mhz 707 LUT 499 FF

Process finished with exit code 0

Hub2 ->
Artix 7 -> 116 Mhz 284 LUT 359 FF
Artix 7 -> 309 Mhz 324 LUT 359 FF
Cyclone V -> FAILED
Cyclone IV -> 134 Mhz 372 LUT 412 FF
Hub8 ->
Artix 7 -> 153 Mhz 395 LUT 457 FF
Artix 7 -> 243 Mhz 444 LUT 457 FF
Cyclone V -> FAILED
Cyclone IV -> 117 Mhz 726 LUT 768 FF
Hub16 ->
Artix 7 -> 118 Mhz 572 LUT 586 FF
Artix 7 -> 194 Mhz 661 LUT 586 FF
Cyclone V -> FAILED
Cyclone IV -> 115 Mhz 726 LUT 510 FF

Process finished with exit code 0

INFO: [Common 17-206] Exiting Vivado at Thu Jun 15 08:31:51 2023...
Hub2 ->
Artix 7 -> 101 Mhz 319 LUT 371 FF
Artix 7 -> 305 Mhz 381 LUT 371 FF
Cyclone V -> FAILED
Cyclone IV -> 148 Mhz 396 LUT 427 FF
Hub8 ->
Artix 7 -> 97 Mhz 428 LUT 468 FF
Artix 7 -> 233 Mhz 493 LUT 468 FF
Cyclone V -> FAILED
Cyclone IV -> 121 Mhz 754 LUT 795 FF
Hub16 ->
Artix 7 -> 122 Mhz 656 LUT 597 FF
Artix 7 -> 189 Mhz 792 LUT 597 FF
Cyclone V -> FAILED
Cyclone IV -> 117 Mhz 745 LUT 519 FF

Process finished with exit code 0

INFO: [Common 17-206] Exiting Vivado at Thu Jun 15 09:27:01 2023...
Hub2 ->
Artix 7 -> 160 Mhz 316 LUT 379 FF
Artix 7 -> 312 Mhz 382 LUT 379 FF
Cyclone V -> FAILED
Cyclone IV -> 132 Mhz 413 LUT 435 FF
Hub8 ->
Artix 7 -> 154 Mhz 433 LUT 477 FF
Artix 7 -> 248 Mhz 498 LUT 477 FF
Cyclone V -> FAILED
Cyclone IV -> 125 Mhz 773 LUT 803 FF
Hub16 ->
Artix 7 -> 118 Mhz 667 LUT 606 FF
Artix 7 -> 184 Mhz 820 LUT 606 FF
Cyclone V -> FAILED
Cyclone IV -> 118 Mhz 765 LUT 527 FF

Process finished with exit code 0

INFO: [Common 17-206] Exiting Vivado at Thu Jun 15 12:24:32 2023...
Hub2 ->
Artix 7 -> 180 Mhz 377 LUT 406 FF
Artix 7 -> 311 Mhz 446 LUT 406 FF
Cyclone V -> FAILED
Cyclone IV -> 121 Mhz 512 LUT 475 FF
Hub8 ->
Artix 7 -> 136 Mhz 508 LUT 510 FF
Artix 7 -> 238 Mhz 588 LUT 510 FF
Cyclone V -> FAILED
Cyclone IV -> 115 Mhz 893 LUT 855 FF
Hub16 ->
Artix 7 -> 119 Mhz 734 LUT 647 FF
Artix 7 -> 199 Mhz 835 LUT 647 FF
Cyclone V -> FAILED
Cyclone IV -> 119 Mhz 879 LUT 578 FF

Process finished with exit code 0


Hub2 ->
Artix 7 -> 126 Mhz 387 LUT 412 FF
Artix 7 -> 292 Mhz 461 LUT 412 FF
Cyclone V -> FAILED
Cyclone IV -> 123 Mhz 529 LUT 481 FF
Hub8 ->
Artix 7 -> 160 Mhz 515 LUT 514 FF
Artix 7 -> 230 Mhz 598 LUT 514 FF
Cyclone V -> FAILED
Cyclone IV -> 113 Mhz 907 LUT 859 FF
Hub16 ->
Artix 7 -> 116 Mhz 737 LUT 650 FF
Artix 7 -> 193 Mhz 852 LUT 650 FF
Cyclone V -> FAILED
Cyclone IV -> 114 Mhz 898 LUT 581 FF

Process finished with exit code 0

 */