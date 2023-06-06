package spinal.lib.bus.tilelink.coherent

import spinal.core._
import spinal.lib._
import spinal.lib.bus.misc.SizeMapping
import spinal.lib.bus.tilelink._
import spinal.lib.pipeline._

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

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
}

case class HubParameters(unp : NodeParameters,
                         slotCount : Int,
                         cacheSize: Int,
                         wayCount: Int,
                         lineSize : Int,
                         probeCount : Int = 8,
                         aBufferCount: Int = 4,
                         cBufferCount: Int = 2,
                         cSourceCount: Int = 4) {
  val addressWidth = unp.m.addressWidth
  val dataWidth = unp.m.dataWidth
  val dataBytes = dataWidth/8
  val waySize = cacheSize/wayCount
  val sets = cacheSize/64/wayCount
  val linePerWay = waySize/lineSize
  val tagRange = addressWidth-1 downto log2Up(linePerWay*lineSize)
  val lineRange = tagRange.low-1 downto log2Up(lineSize)
  val wordRange = log2Up(lineSize)-1 downto log2Up(dataBytes)
  val refillRange = tagRange.high downto lineRange.low
  val blockRange = addressWidth-1 downto log2Up(lineSize)
  val blockSize = lineSize
  val setsRange = lineRange
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
 */

class Hub(p : HubParameters) extends Component{
  import p._

  //  val slotDownOffset = 0
  //  val cDownIdOffset = slotDownOffset + (1 << log2Up(slotCount max p.cSourceCount))
  //  val downIdCount = cDownIdOffset + p.cSourceCount
  //
  //  val bps = p.nodes.map(_.toBusParameter())
  //  val upParam = NodeParameters.mergeMasters(p.nodes)
  //  val upParamPerNodeSourceWidth = nodes.map(_.m.sourceWidth).max
  //  val upBusParam = upParam.toBusParameter()
  //
  //  val downPutParam = NodeParameters(
  //    m = CoherentHub.downPutM2s(
  //      name           = this,
  //      addressWidth   = addressWidth,
  //      dataWidth      = dataWidth,
  //      blockSize      = blockSize,
  //      slotCount      = slotCount,
  //      cSourceCount   = cSourceCount
  //    ),
  //    s = S2mParameters(slaves = Nil)
  //  )
  //  val downGetParam = NodeParameters(
  //    m = CoherentHub.downGetM2s(
  //      name           = this,
  //      addressWidth   = addressWidth,
  //      dataWidth      = dataWidth,
  //      blockSize      = blockSize,
  //      slotCount      = slotCount
  //    ),
  //    s = S2mParameters(slaves = Nil)
  //  )
  //  val downPutBusParam = downPutParam.toBusParameter()
  //  val downGetBusParam = downGetParam.toBusParameter()
  //  val wordsPerBlock = blockSize / upBusParam.dataBytes
  //  def upSourceToNodeOh(source : UInt) = UIntToOh(source.takeHigh(log2Up(p.nodes.size)).asUInt, nodes.size)



  val ubp = p.unp.toBusParameter()
  val dbp = NodeParameters(
    m = Hub.downM2s(
      name           = this,
      addressWidth   = addressWidth,
      dataWidth      = dataWidth,
      blockSize      = blockSize,
      downPendingMax = slotCount
    ),
    s = S2mParameters(slaves = Nil)
  ).toBusParameter()

  val io = new Bundle{
    val up = slave(Bus(ubp))
    val down = master(Bus(dbp))
  }



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
  val LAST = Stageable(Bool())
  val PAYLOAD_C, PAYLOAD = Stageable(DataPayload())
  val SLOT_ID = Stageable(UInt(log2Up(slotCount) bits))
  val TO_DOWN = Stageable(Bool())
  val FROM_NONE = Stageable(Bool())
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
    val valids = Reg(Bits(slotCount bits)) init(0)
    val freeOh = OHMasking.firstV2(~valids)
    val freeId = OHToUInt(freeOh)
    val full = valids.andR
    val allocate = Bool()
    val release = Flow(UInt(log2Up(slotCount) bits))
    valids := (valids | freeOh.andMask(allocate)) & UIntToOh(release.payload).orMask(release.valid)
    val ctx = Mem.fill(slotCount)(Bits((widthOf(CtxA()) max widthOf(CtxC())) + 1 bits))
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
    val PAYLOAD = Stageable(DataPayload())
    val BUFFER_ID = Stageable(UInt(log2Up(aBufferCount) bits))

    val buffer =  new Area{
      val ram = Mem.fill(aBufferCount*p.blockSize/p.unp.m.dataBytes)(DataPayload())
      val allocated = Reg(Bits(aBufferCount bits)) init(0)
      val set, clear = B(0, aBufferCount bits)
      val firstFree = OHToUInt(OHMasking.firstV2(~allocated))
      val full = allocated.andR
      val source = Vec.fill(aBufferCount)(Reg(ubp.source))
      val write = ram.writePort()
      allocated := (allocated | set) & ~clear
    }

    val s0 = new Stage{
      driveFrom(io.up.a)
      val CMD = insert(io.up.a.payload.asNoData())
      val LAST = io.up.a.isLast()
      if(ubp.withDataA) {
        PAYLOAD.data := io.up.a.data
        PAYLOAD.mask := io.up.a.mask
      }
      val dataSplit = ubp.withDataA generate new Area{
        val withBeats = CMD.withBeats
        val hazard = withBeats && buffer.full
        haltIt(hazard)
        throwIt(!hazard && !LAST)

        val locked = RegInit(False) setWhen(buffer.write.valid)
        val lockedValue = RegNextWhen(buffer.firstFree, !locked)
        BUFFER_ID := locked ? lockedValue | buffer.firstFree
        buffer.write.valid := valid && withBeats && !hazard
        buffer.write.address := BUFFER_ID @@ CMD.address(wordRange)
        buffer.write.data := PAYLOAD
        when(isFireing && LAST && withBeats){
          buffer.set(BUFFER_ID) := True
          buffer.source.onSel(BUFFER_ID)(_ := CMD.source)
          locked := False
        }
      }
    }

    val s1 = new Stage(Connection.DIRECT()){
      val readAddress = s0.CMD.address(setsRange)
      val JUST_BUSY = insert(setsBusy.target.write.valid && setsBusy.target.write.address === readAddress)
      val JUST_FREE = insert(setsBusy.hit.write.valid && setsBusy.hit.write.address === readAddress)
    }
    val s2 = new Stage(Connection.M2S()){
      val hit    = setsBusy.hit.mem.readSync( s1.readAddress, isReady)
      val target = setsBusy.target.mem.readSync( s1.readAddress, isReady)
      val freed = RegNext(setsBusy.hit.write.valid && setsBusy.hit.write.address === s0.CMD.address(setsRange)) clearWhen(isReady)
      val hazard = (hit =/= target || s1.JUST_BUSY) && !s1.JUST_FREE && !freed
      haltIt(hazard)

      val newTarget = s1.JUST_BUSY ? hit | !target
      when(initializer.done) {
        setsBusy.target.write.valid := isFireing
        setsBusy.target.write.address := s0.CMD.address(setsRange)
        setsBusy.target.write.data := newTarget
      }
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
    val bufferId = UInt(log2Up(aBufferCount) bits)
    val conflictCtx = CONFLICT_CTX()
  }
  class ProbeCtxFull extends ProbeCtx{
    val fromNone = Bool()
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
    val push = Stream(ProbeCmd())
    val pushCmd = frontendA.s2(frontendA.s0.CMD)
    val isAcquire = Opcode.A.isAcquire(pushCmd.opcode)
    push.valid := frontendA.s2.isValid
    frontendA.s2.haltIt(!push.ready)
    push.ctx.opcode     := pushCmd.opcode
    push.ctx.address    := pushCmd.address
    push.ctx.toTrunk    := pushCmd.param =/= Param.Grow.NtoB
    push.ctx.bufferId   := frontendA.s2(frontendA.BUFFER_ID)
    push.ctx.conflictCtx := frontendA.s2.newTarget
    push.ctx.size       := pushCmd.size
    push.ctx.source     := pushCmd.source
    push.fromNone := pushCmd.param =/= Param.Grow.BtoT && isAcquire
    push.selfMask   := B(coherentMasters.map(_.sourceHit(pushCmd.source))).andMask(isAcquire)
    push.mask := ~push.selfMask


    val slots = for(i <- 0 until probeCount) yield new Area{
      val fire = False
      val valid = RegInit(False) clearWhen(fire)
      val set = Reg(UInt(setsRange.size bits))
      val pending = Reg(UInt(log2Up(coherentMasterCount+1) bits))
      val fromNone = Reg(Bool())
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

      val withoutData = Opcode.C.withoutData(input.opcode)
      val output = input.throwWhen(withoutData)
    }

    val wake = new Pipeline{
      val stages = newChained(2, Connection.M2S())
      val CTX = Stageable(new ProbeCtx())
      val output = Stream(new ProbeCtxFull())

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
        FROM_NONE := OhMux(hitOh, slots.map(_.fromNone))
      }

      val ctxFetcher = new Area {
        val stage = stages(1)
        import stage._

        stage(CTX) := ctxMem.readSync(insertion.hitId, isReady)

        output.valid := isValid
        output.payload.assignSomeByName(CTX)
        output.fromNone := FROM_NONE
        output.probeId := PROBE_ID

        haltWhen(!output.ready)
      }
    }
  }



  val backend = new Pipeline {
    val stages = newChained(3, Connection.M2S())

    val insertion = new Area{
      val stage = stages(0)
      import stage._
      val c = probe.rsp.output
      val a = probe.wake.output

      val lockValid = RegInit(False) setWhen(valid) clearWhen(isFireing)
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

      valid := probe.rsp.output.valid || a.valid
      FROM_C :=  selC
      when(selC){
        WRITE_DATA := c.withBeats
        READ_DATA := False
        PARTIAL_DATA := False
        ADDRESS := c.address
        SIZE := log2Up(blockSize)
        SOURCE := c.source
      } otherwise {
        WRITE_DATA := Opcode.A.isPut(a.opcode)
        READ_DATA  := Opcode.A.isGet(a.opcode) || a.opcode === Opcode.A.ACQUIRE_BLOCK && a.fromNone
        PARTIAL_DATA := a.opcode === Opcode.A.PUT_PARTIAL_DATA
        ADDRESS := a.address
        ADDRESS(wordRange) := a.address(wordRange) + counter
        SIZE := a.size
        SOURCE := a.source
      }
      LAST := WRITE_DATA && counter === sizeToBeatMinusOne(ubp, a.size)
      BUFFER_ID := a.bufferId
      PROBE_ID := a.probeId
      CONFLICT_CTX := a.conflictCtx

      c.ready := isReady &&  selC
      a.ready := isReady && !selC && LAST

      PAYLOAD_C.data := c.data
      PAYLOAD_C.mask.setAll()

      val A_GET_PUT  = insert(Opcode.A.isGetPut(a.opcode))
      val A_TO_TRUNK = insert(a.toTrunk)
      val C_IS_PROBE_DATA = insert(c.opcode === Opcode.C.PROBE_ACK_DATA)

      //need update when $
      TO_DOWN := WRITE_DATA | READ_DATA
      SLOT_ALLOCATE := TO_DOWN
    }

    val aPayloadSyncRead = new Area{
      val s0 = stages(0)
      val s1 = stages(1)
      val wordAddress = s0(BUFFER_ID) @@ s0(ADDRESS)(wordRange)

      s1(PAYLOAD) := s1(PAYLOAD_C)
      when(!s1(FROM_C)){
        s1(PAYLOAD) := frontendA.buffer.ram.readSync(wordAddress, s1.isReady)
      }
    }

    val allocateSlot = new Area{
      val stage = stages(0)
      import stage._
      SLOT_ID := slots.freeId
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

      slots.allocate := isFireing && SLOT_ALLOCATE
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
    }
  }

  val downD = new Pipeline{
    val stages = newChained(2, Connection.M2S())
    val D_PAYLOAD = Stageable(io.down.d.payloadType)
    val CTX = Stageable(slots.ctx.wordType)

    val insert = new Area{
      stages.head.driveFrom(io.down.d)
      stages.head(D_PAYLOAD) := io.down.d
    }

    val ctxReadSync = new Area{
      val s0 = stages(0)
      val s1 = stages(1)
      val wordAddress = s0(D_PAYLOAD).source
      s1(CTX) := slots.ctx.readSync(wordAddress, s1.isReady)
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

      slots.release.valid := isFireing && !fired
      slots.release.payload := D_PAYLOAD.source
    }

    val toSetsBusy = new Area{
      val stage = stages(1)
      import stage._

      val ctx = decodeCtx(CTX)
      val hit = !ctx.fromC && ctx.a.getPut
      val fired = RegInit(False) setWhen(setsBusy.hit.downD.fire) clearWhen(isReady)

      setsBusy.hit.downD.valid := valid && hit && !fired
      setsBusy.hit.downD.address := ctx.a.set
      setsBusy.hit.downD.data := ctx.a.conflictCtx
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

      val ctx = decodeCtx(CTX)
      val hit = ctx.fromC ? (!ctx.c.isProbeData) | True
      val fired = RegInit(False) setWhen(io.up.d.fire) clearWhen(isReady)
      io.up.d.valid := isValid && hit && fired
      io.up.d.payload := D_PAYLOAD
      io.up.d.sink.removeAssignments() := U(ctx.a.conflictCtx ## ctx.a.set)
      when(ctx.fromC){
        io.up.d.opcode := Opcode.D.RELEASE_ACK
      } otherwise {
        when(!ctx.a.getPut) {
          io.up.d.opcode := Opcode.D.GRANT_DATA
        }
      }
      haltIt(!fired && hit && !io.up.d.ready)
    }
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
  def basicConfig(probeCount : Int = 8, masterPerChannel : Int = 4, dataWidth : Int = 64, addressWidth : Int = 13, cacheSize : Int = 1024, wayCount : Int = 2, lineSize : Int = 64) = {
    val setCount = cacheSize / wayCount / lineSize
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
            sinkId = SizeMapping(0, setCount*2)
          )
        ))
      ),
      slotCount = 16,
      probeCount = probeCount,
      cacheSize = cacheSize,
      wayCount  = wayCount,
      lineSize  = lineSize
    )
  }
  def gen = new Hub(HubGen.basicConfig(8, masterPerChannel = 4, dataWidth = 16, addressWidth = 32, cacheSize = 32*1024))
  SpinalVerilog(gen)

}

object HubSynt extends App{
  import spinal.lib.eda.bench._
  val rtls = ArrayBuffer[Rtl]()
  for(probeCount <- List(2, 8, 16)) { //Rtl.ffIo
    rtls += Rtl(SpinalVerilog((new Hub(HubGen.basicConfig(probeCount = probeCount, masterPerChannel = 4, dataWidth = 16, addressWidth = 32, cacheSize = 32*1024)).setDefinitionName(s"Hub$probeCount"))))
  }
  val targets = XilinxStdTargets().take(2) ++ AlteraStdTargets(quartusCycloneIIPath = null)

  Bench(rtls, targets)
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


 */