package spinal.lib.bus.tilelink.coherent

import spinal.core._
import spinal.lib._
import spinal.lib.bus.misc.SizeMapping
import spinal.lib.bus.tilelink._
import spinal.lib.pipeline._

case class DirectoryParam(var unp : NodeParameters,
                          var downPendingMax : Int,
                          var cacheWays: Int,
                          var cacheBytes: Int,
                          var blockSize : Int,
                          var cacheBanks : Int = 1,
                          var probeCount : Int = 8,
                          var aBufferCount: Int = 4,
                          var ctrlLoopbackDepth : Int = 4,
                          var generalSlotCount : Int = 8,
                          var probeRegion : UInt => Bool) {
  assert(isPow2(cacheBytes))

  def lockSets = cacheSets //TODO min trackedSets !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  def cacheLines = cacheBytes / blockSize
  def cacheSets = cacheLines / cacheWays
  def addressWidth = unp.m.addressWidth
  def dataWidth = unp.m.dataWidth
  def dataBytes = dataWidth/8
  def tagRange = addressWidth-1 downto log2Up(cacheBytes/cacheWays)
  def lineRange = tagRange.low-1 downto log2Up(lineSize)
  def wordRange = log2Up(lineSize)-1 downto log2Up(dataBytes)
  def refillRange = tagRange.high downto lineRange.low
  def blockRange = addressWidth-1 downto log2Up(lineSize)
  def lineSize = blockSize
  def setsRange = lineRange

  def addressCheckRange = setsRange.high downto log2Up(lineSize)
  def addressCheckWidth = addressCheckRange.size
//  def lockSetsRange = log2Up(lineSize*lockSets)-1 downto log2Up(lineSize)
}


class BankedMem[T <: Data](wordType: HardType[T], words: Int, banks : Int) extends Area{
  def createWritePort(maskWidth : Int = -1): Stream[MemWriteCmd[T]] = {
    ???
  }

  def createReadSyncPort(): MemReadStreamFlowPort[T] = {
    ???
  }

  def build() = new Area{

  }
}



/*

get / put
- lock
- [probe victim]
- backend
- ack

acquire
- lock
- [probe hit]
- [update tags], [probe victim]
- backend
- ack

probeAck
- get context from probe slots
- [update tags]

probeAckData
- get context from probe slots
- [update tags], [probe victim]
- backend
- probes update

release
- [update tags] (as release -> probe can be out of order)
- ack

releaseData
- update tags, [probe victim]
- backend
- ack


Probes to deal with :
- All toN : from get / put / acquire / releaseData / probeData which need room
- Others toN : from acquire XtoT
- single TtoB : from acquire NtoB while line is in a unique state


get : [probe all toN] => read $/down, down may as well refill $
put : [probe all toN] => write down else refill cache then write it
acquire : [probe] => read $/down, down may as well refill $
upC data : write $/down => [wake probe]


Stuff which need concurrent protection :
- Cache line refill
- address inflight (to avoid read <> write coherency issue)
- address being probed (this will also ensure probe_ack_data are good)

Backend ops :
- up.a/c to $/down
- $ to up
- $ to victim [wait up.c], then write victim to down
- down to $/up [then write up.a to $]

down.D can trigger :
- to $
- to up.D
- up.a to $ (up.a.put allocating a cache line)
- if non inclusive cache : probe completion (initiated from probe_data_ack)

 */

class Directory(val p : DirectoryParam) extends Component {

  import p._

  val ubp = p.unp.toBusParameter()
  val dbp = NodeParameters(
    m = Hub.downM2s(
      name = this,
      addressWidth = addressWidth,
      dataWidth = dataWidth,
      blockSize = blockSize,
      downPendingMax = downPendingMax
    ),
    s = S2mParameters(slaves = Nil)
  ).toBusParameter()

  val io = new Bundle {
    val up = slave(Bus(ubp))
    val down = master(Bus(dbp))
  }

  val coherentMasters = unp.m.masters.filter(_.emits.withBCE)
  val coherentMasterCount = coherentMasters.size
  val coherentMasterToSource = coherentMasters.map(_.bSourceId)

  val SET_ID = Stageable(UInt(log2Up(lockSets) bits))
  val LOCK_CTX = Stageable(Bool())
  val ADDRESS = Stageable(ubp.address)
  val GS_ID = Stageable(UInt(log2Up(generalSlotCount) bits))

  val emptyId = 0
  def isBlockEmpty(blockId : UInt) = blockId === emptyId
  def isBlockWithData(blockId : UInt) = blockId.msb

  val initializer = new Area {
    val initCycles = Math.max(p.lockSets, p.cacheWays)
    val counter = Reg(UInt(log2Up(initCycles) + 1 bits)) init (0)
    val done = counter.msb
    when(!done) {
      counter := counter + 1
    }
  }


  case class Tags(val withData : Boolean) extends Bundle {
    val loaded = Bool()
    val tag = UInt(tagRange.size bits)
    val dirty = withData generate Bool()
    val trunk = Bool()
    val owners = Bits(coherentMasterCount bits)
  }




  class LineCtrl(bytes : Int, ways: Int, withData : Boolean) extends Area {
    val tags = new Area {
      val sets = bytes / blockSize / ways
      val ram = Mem.fill(sets)(Vec.fill(ways)(Tags(withData)))
      val read = ram.readSyncPort
      val writeRaw = ram.writePortWithMask(ways)
      val write = new Area{
        val address = ram.addressType()
        val mask = Bits(ways bits)
        val data = Tags(withData)

        writeRaw.valid := False
        writeRaw.address := address
        writeRaw.mask := mask
        writeRaw.data.foreach(_:= data)
      }
    }
    val data = withData generate new Area{
      val ram = new BankedMem(Bits(p.dataWidth bits), cacheBytes/p.dataBytes, cacheBanks)
      val longWrite = ram.createWritePort(p.dataBytes)
      val longRead = ram.createReadSyncPort()
      val refillWrite = ram.createWritePort()
    }
  }

  val cache = new LineCtrl(cacheBytes, cacheWays, true)



  val setsLock = new Area {
    class MemArea extends Area {
      val mem = Mem.fill(lockSets)(Bool())
      val write = mem.writePort()
      write.valid := !initializer.done
      write.address := initializer.counter.resized
      write.data := False
    }

    val target = new MemArea
    val hit = new MemArea {
//      val upE = Stream(write.payload)
//      val downD = Stream(write.payload)
//      val arbiter = StreamArbiterFactory().noLock.lowerFirst.onArgs(upE, downD).toFlow
//      when(initializer.done) {
//        write << arbiter
//      }
    }

    val unlockEvent = False
  }

  val CtrlOpcode = new SpinalEnum{
    val ACQUIRE_BLOCK, ACQUIRE_PERM, RELEASE, RELEASE_DATA, PUT_PARTIAL_DATA, PUT_FULL_DATA, PROBE_ACK_DATA, GET, EVICT = newElement()
  }
  class CtrlCmd() extends Bundle {
    val opcode = CtrlOpcode()
    val args = Bits(1 bits)
    val address = ubp.address()
    val size = ubp.size()
    val source = ubp.source()
    val bufferAId = BUFFER_A_ID()

    def toTrunk = args(0)
    def toNone = args(0)
  }

  class ProberCmd() extends CtrlCmd {
    val mask = Bits(coherentMasterCount bits)
    val probeToN = Bool()
  }


  class ReadDownCmd() extends Bundle {
    val gsId = GS_ID()
    val address = ubp.address()
    val size = ubp.size()
  }

//  val VICTIME_ID = Stageable(UInt(-1 bits)) ;???

  class WriteBackendCmd() extends Bundle {
    val fromUpA = Bool() //else from upC
    val toDownA = Bool() //else to cache
    val toUpD = Bool() //else to cache

    val gsId = GS_ID()
    val partialUpA = Bool()
    val address = ubp.address()
    val size = ubp.size()
    val wayId = UInt(log2Up(cacheWays) bits)
    val bufferAId = BUFFER_A_ID()
  }

  class ReadBackendCmd() extends Bundle {
    val toUpD = Bool() // else to victim

    val gsId = GS_ID()
    val address = ubp.address()
    val size = ubp.size()
    val wayId = UInt(log2Up(cacheWays) bits)
//    val victimId = VICTIME_ID()
    val upD = new Area {
      val opcode = Opcode.D()
      val param  = Bits(3 bits)
      val source = ubp.source()
    }
  }


  val CTRL_CMD = Stageable(new CtrlCmd())


  class Slot extends Area{
    val fire = False
    val valid = RegInit(False) clearWhen(fire)
  }
  class SlotPool[T <: Slot](slotsCount : Int)(gen : => T) extends Area{
    val slots = for(i <- 0 until slotsCount) yield gen
    val allocate = new Area{
      val full = slots.map(_.valid).andR
      val oh = OHMasking.firstV2(Vec(slots.map(!_.valid)))
      val id = OHToUInt(oh)
    }
  }


  class GeneralSlot extends Slot{
    val refill = Reg(Bool())
    val write = Reg(Bool()) // Else assume it's a read
    val address = Reg(UInt(addressCheckWidth bits))
    val way = Reg(UInt(log2Up(cacheWays) bits))
    val pending = new Area{
      val victim, downD, upE = Reg(Bool())
      fire setWhen(List(victim, downD, upE).norR)
    }
  }

  val gs = new SlotPool(generalSlotCount)(new GeneralSlot){
    case class CtxDownD() extends Bundle {
      val toUpD, toCache = Bool()
      val toProbe, mergeBufferA = Bool()
      val probeId = UInt(log2Up(probeCount) bits)
      val bufferAId = BUFFER_A_ID()
      val bufferAOffset, bufferAWords = UInt(wordRange.size bits)
      val sourceId = io.up.p.source()
      val setId = UInt(setsRange.size bits)
      val wayId = UInt(log2Up(cacheWays) bits)
    }
    val ctxDownD = Mem.fill(generalSlotCount)(CtxDownD())
  }


  val bufferA = new ChannelDataBuffer(
    entries = aBufferCount,
    blockSize = blockSize,
    dataBytes = ubp.dataBytes
  ){
    val read = ram.readSyncPort()

    val pusher = push(io.up.a)
    val toCtrl = pusher.down.swapPayload(new CtrlCmd())
    toCtrl.opcode := pusher.down.opcode.muxDc(
      Opcode.A.GET -> CtrlOpcode.GET(),
      Opcode.A.PUT_PARTIAL_DATA -> CtrlOpcode.PUT_PARTIAL_DATA(),
      Opcode.A.PUT_FULL_DATA -> CtrlOpcode.PUT_FULL_DATA(),
      Opcode.A.ACQUIRE_PERM -> CtrlOpcode.ACQUIRE_PERM(),
      Opcode.A.ACQUIRE_BLOCK -> CtrlOpcode.ACQUIRE_BLOCK()
    )
    toCtrl.toTrunk := pusher.down.param === Param.Grow.BtoT
    toCtrl.address := pusher.down.address
    toCtrl.size := pusher.down.size
    toCtrl.source := pusher.down.source
  }

  val BUFFER_A_ID = Stageable(UInt(log2Up(aBufferCount) bits))

  val victimBuffer = new Area{
    val ram = Mem.fill(???)(io.up.p.data)
    val write = ram.writePort()
    val read = ram.readSyncPort()
  }


  val upCSplit = new Area{
    val (cmdFork, dataFork) = StreamFork2(io.up.c)
    val cmd = cmdFork.translateWith(io.up.c.asNoData()).takeWhen(io.up.c.isFirst())
    val data = dataFork.takeWhen(io.up.c.withBeats).translateWith(dataFork.data)
    val dataPop = data.m2sPipe().m2sPipe()
  }

  class ProberSlot extends Slot{
    val address = Reg(UInt(addressCheckWidth bits))
    val pending = Reg(UInt(log2Up(coherentMasterCount + 1) bits))
    val probeAckDataCompleted = Reg(Bool())
    val unique = Reg(Bool())
    val done = valid && (pending & ~U(probeAckDataCompleted, widthOf(pending) bits)) === 0
  }

  val prober = new SlotPool(probeCount)(new ProberSlot){
    val ctx = new Area{
      val ram = Mem.fill(probeCount)(new CtrlCmd())
      val write = ram.writePort()
    }

    val cmd = Stream(new ProberCmd())

    val upB = new Area{
      val push = cmd.m2sPipe()
      val sloted = RegInit(False)
      val full = slots.map(_.valid).andR
      val pending = CountOne(push.mask)

      ctx.write.valid := False
      ctx.write.address := allocate.id
      ctx.write.data.assignSomeByName(push.payload)

      when(push.valid && !sloted && !allocate.full) {
        slots.onMask(allocate.oh) { s =>
          s.valid := True
          s.address := cmd.address(addressCheckRange)
          s.pending := pending
          s.unique := True
          s.probeAckDataCompleted := False
        }
        ctx.write.valid := True
        sloted := True
      }

      val bus = io.up.b
      val halted = push.haltWhen(!sloted && full)
      val fired = Reg(Bits(coherentMasterCount bits)) init (0)
      val requests = push.mask & ~fired
      val masterOh = OHMasking.firstV2(requests)
      bus.valid := halted.valid && requests.orR
      bus.opcode := Opcode.B.PROBE_BLOCK
      bus.param := halted.probeToN ? B(Param.Cap.toN, 3 bits) | B(Param.Cap.toB, 3 bits)
      bus.source := OhMux(masterOh, coherentMasterToSource.map(id => U(id, ubp.sourceWidth bits)).toList)
      bus.address := halted.address(blockRange) @@ U(0, blockRange.low bits)
      bus.size := log2Up(p.blockSize)
      halted.ready := requests === 0

      when(bus.fire) {
        fired.asBools.onMask(masterOh)(_ := True)
      }

      when(halted.ready) {
        fired := 0
        sloted := False
      }
    }

    val upC = new Area {
      val input = upCSplit.cmd
      val isProbeNoData = input.opcode === Opcode.C.PROBE_ACK
      val isProbe = Opcode.C.isProbe(input.opcode)
      val hitOh = slots.map(s => s.valid && s.address === input.address(addressCheckRange))
      val hitId = OHToUInt(hitOh)
      val pending = slots.map(_.pending).read(hitId)
      val pendingNext = pending - 1
      val masterOh = coherentMasterToSource.map(input.source === _).asBits
      val masterId = OHToUInt(masterOh)
      val keptCopy = isProbe && Param.reportPruneKeepCopy(input.param)

      when(input.fire) {
        slots.onSel(hitId) { s =>
          s.unique.clearWhen(keptCopy)
          when(isProbeNoData) {
            s.pending := pendingNext
          }
        }
      }

      val filtred = input.throwWhen(upCSplit.cmd.opcode === Opcode.C.PROBE_ACK)
      val down = filtred.swapPayload(new CtrlCmd)
      down.opcode   := upCSplit.cmd.opcode.muxDc(
        Opcode.C.RELEASE -> CtrlOpcode.RELEASE(),
        Opcode.C.RELEASE_DATA -> CtrlOpcode.RELEASE_DATA(),
        Opcode.C.PROBE_ACK_DATA -> CtrlOpcode.PROBE_ACK_DATA()
      )
      down.toNone   := !Param.reportPruneKeepCopy(input.param)
      down.address  := input.address
      down.size     := input.size
      down.source   := input.source
    }

    val wake = new Area {
      val down = Stream(new CtrlCmd)
      val hits = slots.map(_.done).asBits
      val hitOh = OHMasking.roundRobinNext(hits, down.fire)
      val probeId = OHToUInt(hitOh)

      down.valid := hits.orR
      down.payload := ctx.ram.readAsync(probeId)

      when(down.fire) {
        slots.onMask(hitOh) { s =>
          s.fire := True
        }
      }
    }

    val arbiter = new Area{
      val arbitred = StreamArbiterFactory().noLock.lowerFirst.onArgs(upC.down, wake.down)
      val down = arbitred.pipelined(m2s = true, s2m = true)
    }
  }

  val ctrl = new Pipeline{
    val stages = newChained(3, Connection.M2S())
    val inserterStage = stages(0)
    val addressStage = stages(0)
    val dataStage = stages(1)
    val tagStage = stages(1)
    val prepStage = stages(1)
    val processStage = stages(2)

    import CtrlOpcode._


    val loopback = new Area{
      val occupancy = new CounterUpDown(ctrlLoopbackDepth, handleOverflow = false)
      ???
      val allowUpA = !occupancy.mayOverflow
      val fifo = StreamFifo(new CtrlCmd, ctrlLoopbackDepth, forFMax = true)
    }

    val inserter = new Area {
      import inserterStage._

      def hazardHalt(that : Stream[CtrlCmd]) = {
        val hits = stages.tail.map(s => s.valid && s(CTRL_CMD).address(setsRange) === that.address(setsRange))
        that.haltWhen(hits.orR)
      }

      val arbiter = StreamArbiterFactory().roundRobin.noLock.build(CTRL_CMD, 2)
      arbiter.io.inputs(0) << prober.arbiter.down
      arbiter.io.inputs(1) << loopback.fifo.io.pop
      arbiter.io.inputs(2) << bufferA.toCtrl.continueWhen(loopback.allowUpA)

      driveFrom(hazardHalt(arbiter.io.output))
      inserterStage(CTRL_CMD) := arbiter.io.output

      haltWhen(loopback.occupancy.mayOverflow)
      loopback.occupancy.incrementIt.setWhen(isFireing)

      val SOURCE_OH = insert(B(coherentMasters.map(_.sourceHit(CTRL_CMD.source))))
    }

    cache.tags.read.cmd.valid := addressStage.isFireing
    cache.tags.read.cmd.payload := addressStage(CTRL_CMD).address(lineRange)
    val CACHE_TAGS = dataStage.insert(cache.tags.read.rsp)

    val tags = new Area{
      import tagStage._
      val read = tagStage(CACHE_TAGS)
      val CACHE_HITS  = insert(read.map(t => t.loaded && t.tag === CTRL_CMD.address(tagRange)).asBits)
      val SOURCE_HITS = insert(read.map(t => (t.owners & inserter.SOURCE_OH).orR).asBits)
    }

    val preCtrl = new Area{
      import prepStage._
      val ALLOCATE_ON_MISS = insert(False) //TODO
      val FROM_C = insert(List(RELEASE(), RELEASE_DATA(), PROBE_ACK_DATA()).sContains(CTRL_CMD.opcode))
      val GET_PUT = insert(List(GET(), PUT_FULL_DATA(), PUT_PARTIAL_DATA()).sContains(CTRL_CMD.opcode))
      val ACQUIRE = insert(List(ACQUIRE_PERM(), ACQUIRE_BLOCK()).sContains(CTRL_CMD.opcode))
      val IS_GET = insert(List(GET()).sContains(CTRL_CMD.opcode))
      val IS_PUT_FULL_BLOCK = insert(CTRL_CMD.opcode === Opcode.A.PUT_FULL_DATA && CTRL_CMD.size === log2Up(blockSize))
      val WRITE_DATA = insert(List(PUT_PARTIAL_DATA(), PUT_FULL_DATA(), RELEASE_DATA(), PROBE_ACK_DATA()).sContains(CTRL_CMD.opcode))
    }

    val process = new Area{
      import processStage._

      val hazard = False
      throwIt(hazard)

      val askProbe = False
      val askReadDown = False
      val askReadBackend = False
      val askWriteBackend = False
      val askGs = False //TODO use it
      val askUpD = False

      hazard setWhen (askGs && gs.allocate.full)

      val toReadDown = forkStream(askReadDown && !hazard).swapPayload(new ReadDownCmd)
      val toReadBackend = forkStream(askReadBackend && !hazard).swapPayload(new ReadBackendCmd)
      val toWriteBackend = forkStream(askWriteBackend && !hazard).swapPayload(new WriteBackendCmd)
      val toUpD = forkStream(askUpD && !hazard).swapPayload(io.up.d.payloadType)

      val CACHE_HIT = insert(tags.CACHE_HITS.orR)
      val CACHE_HIT_WAY_ID = insert(OHToUInt(tags.CACHE_HITS))
      val SOURCE_HIT = insert((tags.CACHE_HITS & tags.SOURCE_HITS).orR)
      val CACHE_LINE = insert(OhMux.or(tags.CACHE_HITS, CACHE_TAGS))
      val OTHERS = insert(CACHE_LINE.owners & ~inserter.SOURCE_OH)
      val OTHER = insert(OTHERS.orR)

      //ACQUIRE_BLOCK, ACQUIRE_PERM, RELEASE, RELEASE_DATA, PUT_PARTIAL_DATA, PUT_FULL_DATA, PROBE_ACK_DATA, GET

      cache.tags.write.address := CTRL_CMD.address(setsRange)
      cache.tags.write.mask := 0
      cache.tags.write.data := CACHE_LINE
      cache.tags.write.data.tag.removeAssignments() := CTRL_CMD.address(setsRange)

      val owners = new Area{
        val add, remove, clean = False
        val next = (CACHE_LINE.owners.andMask(!clean) | inserter.SOURCE_OH.andMask(add)) & ~inserter.SOURCE_OH.andMask(remove)
        cache.tags.write.data.owners.removeAssignments() := next
      }

      //TODO all the halt arbitration (probe, backend, ...)
      //TODO don't forget to ensure that a victim get out of the cache before downD/upA erase it

      val acquireParam = (CACHE_LINE.owners & ~inserter.SOURCE_OH).orR.mux[Bits](Param.Cap.toB, Param.Cap.toT)
      toUpD.valid   := askUpD
      toUpD.opcode  := Opcode.D.GRANT
      toUpD.source  := CTRL_CMD.source
      toUpD.sink    := gs.allocate.id
      toUpD.size    := log2Up(blockSize)
      toUpD.param   := acquireParam
      toUpD.denied  := False
      toUpD.corrupt := False
      toUpD.data.assignDontCare()

      prober.cmd.valid := askProbe
      prober.cmd.payload.assignSomeByName(CTRL_CMD)
      prober.cmd.mask.assignDontCare()
      prober.cmd.probeToN.assignDontCare()
      //TODO probe hazard

      loopback.fifo.io.push.valid := prober.cmd.isStall
      loopback.fifo.io.push.payload := CTRL_CMD
      hazard setWhen(prober.cmd.isStall)

      val doIt = isFireing && !isRemoved
      val askEvict = False //Will handle victim

      val olderWay = new Area{
        val unlocked = Bool() //TODO check that nothing is going on on that way
        val wayId = UInt()
        val tags = CACHE_TAGS(wayId)
        val address = tags.tag @@ CTRL_CMD.address(setsRange) @@ U(0, log2Up(blockSize) bits)
      }


      toReadBackend.gsId := gs.allocate.id
      toWriteBackend.gsId := gs.allocate.id

      toReadDown.gsId    := gs.allocate.id
      toReadDown.address := CTRL_CMD.address
      toReadDown.size    := CTRL_CMD.size

      when(askEvict && olderWay.tags.loaded && olderWay.unlocked){
        askReadBackend setWhen(olderWay.tags.dirty)
        toReadBackend.toUpD     := False
        toReadBackend.address   := olderWay.address
        toReadBackend.size      := log2Up(blockSize)
        toReadBackend.wayId  := olderWay.wayId

        askProbe := olderWay.tags.owners.orR
        prober.cmd.opcode := CtrlOpcode.EVICT
        prober.cmd.address := olderWay.address
        prober.cmd.size := log2Up(blockSize)
        prober.cmd.mask := olderWay.tags.owners
        prober.cmd.probeToN := True
      }

      when(preCtrl.FROM_C){
        //Update tags
        owners.remove setWhen (CTRL_CMD.toNone)
        cache.tags.write.data.trunk := False
        when(CACHE_HIT) {
          cache.tags.write.mask := tags.CACHE_HITS
        }

        //Write to backend
        when(preCtrl.WRITE_DATA){
          askEvict := !CACHE_HIT && preCtrl.ALLOCATE_ON_MISS
          askWriteBackend := True
          toWriteBackend.fromUpA    := False
          toWriteBackend.toDownA    := !CACHE_HIT && (!preCtrl.ALLOCATE_ON_MISS || !olderWay.unlocked)
          toWriteBackend.address    := CTRL_CMD.address
          toWriteBackend.size       := CTRL_CMD.size
          toWriteBackend.wayId      := olderWay.wayId
        }
      }
      when(preCtrl.GET_PUT){
        when(CACHE_HIT && (!preCtrl.IS_GET || CACHE_LINE.trunk)){
          //Need to be probed out of coherent masters
          askProbe := True
          prober.cmd.mask := CACHE_LINE.owners
          prober.cmd.probeToN := !preCtrl.IS_GET
          //TODO ensure that once the probe is done, the initial request isn't overtaken by another one (ex acquire)
        } otherwise{
          when(CACHE_HIT) {
            askReadBackend := preCtrl.IS_GET
            toReadBackend.toUpD      := True
            toReadBackend.address    := CTRL_CMD.address
            toReadBackend.size       := CTRL_CMD.size
            toReadBackend.wayId   := CACHE_HIT_WAY_ID
            toReadBackend.upD.opcode := Opcode.D.ACCESS_ACK_DATA
            toReadBackend.upD.param  := 0
            toReadBackend.upD.source := CTRL_CMD.source

            askWriteBackend := !preCtrl.IS_GET
            toWriteBackend.fromUpA    := True
            toWriteBackend.toDownA    := False
            toWriteBackend.partialUpA := CTRL_CMD.opcode === Opcode.A.PUT_PARTIAL_DATA
            toWriteBackend.address    := CTRL_CMD.address
            toWriteBackend.size       := CTRL_CMD.size
            toWriteBackend.wayId      := CACHE_HIT_WAY_ID
            toWriteBackend.bufferAId  := CTRL_CMD.bufferAId
          }otherwise {
            askEvict := preCtrl.ALLOCATE_ON_MISS
            askReadDown := (preCtrl.ALLOCATE_ON_MISS && !preCtrl.IS_PUT_FULL_BLOCK) || preCtrl.IS_GET
          }
        }
      }
      when(preCtrl.ACQUIRE){
        when(CACHE_HIT){
          when(CACHE_LINE.trunk || CTRL_CMD.toTrunk) {
            //Need to probe the others first
            askProbe := True
            prober.cmd.mask := OTHERS
            prober.cmd.probeToN := CTRL_CMD.toTrunk
          } otherwise {
            when(CTRL_CMD.opcode === CtrlOpcode.ACQUIRE_BLOCK) {
              askReadBackend := True
              toReadBackend.toUpD := True
              toReadBackend.address := CTRL_CMD.address
              toReadBackend.size := CTRL_CMD.size
              toReadBackend.wayId := CACHE_HIT_WAY_ID
              toReadBackend.upD.opcode := Opcode.D.GRANT_DATA
              toReadBackend.upD.param := acquireParam
              toReadBackend.upD.source := CTRL_CMD.source
            } otherwise {
              askUpD := True
            }
          }
        } otherwise {
          askEvict := True
          askReadDown := preCtrl.ALLOCATE_ON_MISS
        }
      }

      when(!doIt){
        cache.tags.write.mask := 0
      }
    }
  }

  val readDown = new Area {
    val cmd = ctrl.process.toReadDown.pipelined(m2s = true)
    val toDownA = cmd.swapPayload(io.down.a.payloadType())

    toDownA.opcode := Opcode.A.GET
    toDownA.param := 0
    toDownA.source := cmd.gsId
    toDownA.address := cmd.address
    toDownA.size := cmd.size
    toDownA.mask.assignDontCare()
    toDownA.data.assignDontCare()
    toDownA.corrupt := False
  }


  val writeBackend = new Pipeline {
    val stages = newChained(3, Connection.M2S())
    val inserterStage = stages(0)
    val fetchStage = stages(0)
    val readStage = stages(1)
    val processStage = stages(2)

    val CMD = Stageable(new WriteBackendCmd())


    val inserter = new Area {
      import inserterStage._

      val cmd = ctrl.process.toWriteBackend.pipelined(m2s = true)
      val counter = Reg(io.up.p.beat()) init (0)
      val LAST = insert(counter === sizeToBeatMinusOne(io.up.p, cmd.size))

      cmd.ready := isReady && LAST
      valid := cmd.valid
      inserterStage(CMD) := cmd.payload
      CMD.address.removeAssignments() := (cmd.address | (counter << log2Up(p.dataBytes))).resized

      when(isFireing) {
        counter := counter + 1
        when(LAST) {
          counter := 0
        }
      }
    }


    bufferA.read.cmd.valid := fetchStage.isFireing
    bufferA.read.cmd.payload := fetchStage(CMD).bufferAId @@ fetchStage(CMD).address(wordRange)
    val BUFFER_A = readStage.insert(bufferA.read.rsp)

    val process = new Area {
      import processStage._

      val fromUpC = !CMD.fromUpA
      val UP_DATA = insert(CMD.fromUpA.mux(BUFFER_A.data, upCSplit.dataPop.payload))
      val UP_MASK = insert(BUFFER_A.mask.orMask(fromUpC))
      val hazardUpC = fromUpC && !upCSplit.dataPop.valid
      upCSplit.dataPop.ready := fromUpC && isFireing

      val toCacheFork = forkStream(!CMD.toDownA)
      cache.data.longWrite.arbitrationFrom(toCacheFork.haltWhen(hazardUpC))
      cache.data.longWrite.address := CMD.wayId @@ CMD.address(setsRange.high downto wordRange.low)
      cache.data.longWrite.data := UP_DATA
      cache.data.longWrite.mask := UP_MASK

      val toDownAFork = forkStream(CMD.toDownA)
      val toDownA = toDownAFork.haltWhen(hazardUpC).swapPayload(io.down.a.payloadType)
      toDownA.opcode := (CMD.fromUpA && CMD.partialUpA).mux(Opcode.A.PUT_PARTIAL_DATA, Opcode.A.PUT_FULL_DATA)
      toDownA.param := 0
      toDownA.source := U"0" @@ CMD.gsId
      toDownA.address := CMD.address
      toDownA.size := CMD.size
      toDownA.mask := UP_DATA
      toDownA.data := UP_MASK
      toDownA.corrupt := False
    }
  }

  val readBackend = new Pipeline {
    val stages = newChained(3, Connection.M2S())
    val inserterStage = stages(0)
    val fetchStage = stages(0)
    val readStage = stages(1)
    val processStage = stages(2)

    val CMD = Stageable(new ReadBackendCmd())


    val inserter = new Area {

      import inserterStage._

      val cmd = ctrl.process.toReadBackend.pipelined(m2s = true)
      val counter = Reg(io.up.p.beat()) init (0)
      val LAST = insert(counter === sizeToBeatMinusOne(io.up.p, cmd.size))

      cmd.ready := isReady && LAST
      valid := cmd.valid
      inserterStage(CMD) := cmd.payload
      CMD.address.removeAssignments() := (cmd.address | (counter << log2Up(p.dataBytes))).resized

      when(isFireing) {
        counter := counter + 1
        when(LAST) {
          counter := 0
        }
      }
    }

    val fetcher = new Area {
      import fetchStage._
      def cacheCmd = cache.data.longRead.cmd

      cacheCmd.valid := isFireing
      cacheCmd.payload := CMD.wayId @@ CMD.address(setsRange.high downto wordRange.low)
    }

    val CACHED = ??? //readStage.insert(cache.data.longRead.rsp)

    val process = new Area {
      import processStage._

      val toUpDFork = forkStream(CMD.toUpD)
      val toUpD = toUpDFork swapPayload io.up.d.payloadType()
      toUpD.opcode := CMD.upD.opcode
      toUpD.param := CMD.upD.param
      toUpD.source := CMD.upD.source
      toUpD.sink := CMD.gsId
      toUpD.size := CMD.size
      toUpD.denied := False
      toUpD.data := CACHED
      toUpD.corrupt := False

      victimBuffer.write.valid := isValid && !CMD.toUpD
      victimBuffer.write.address := CMD.victimId @@ CMD.address(wordRange)
      victimBuffer.write.data := CACHED
    }
  }

  val toDownA = new Area{
    val arbiter = StreamArbiterFactory().lowerFirst.lambdaLock[ChannelA](_.isLast()).build(io.down.a.payloadType, 3)
    arbiter.io.inputs(0) << readDown.toDownA
    arbiter.io.inputs(1) << ??? //victim
    arbiter.io.inputs(2) << writeBackend.process.toDownA
    io.down.a << arbiter.io.output
  }

  val toUpD = new Area{
    val arbiter = StreamArbiterFactory().lowerFirst.lambdaLock[ChannelD](_.isLast()).build(io.up.d.payloadType, 3)
    arbiter.io.inputs(0) << readBackend.process.toUpD
    arbiter.io.inputs(1) << ???
    arbiter.io.inputs(2) << ctrl.process.toUpD.m2sPipe()

    io.up.d << arbiter.io.output
  }

  ctrl.build()
  readBackend.build()
  writeBackend.build()

  when(!initializer.done) {
    cache.tags.write.mask.setAll()
    cache.tags.write.address := initializer.counter.resized
    cache.tags.write.data.loaded := False
  }

  io.up.d.setIdle()
  io.up.e.ready := False

  io.down.d.ready := False
}


object DirectoryGen extends App{
  def basicConfig(probeCount: Int = 8,
                  downPendingMax: Int = 16,
                  masterPerChannel: Int = 4,
                  dataWidth: Int = 64,
                  addressWidth: Int = 32,
                  lockSets: Int = 64*1024/64,
                  cacheBytes : Int = 64*1024,
                  cacheWays : Int = 8) = {
    val blockSize = 64
    DirectoryParam(
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
                id = SizeMapping(mId * 4, 4)
              ))
            )
          )
        ),
        s = S2mParameters(List(
          S2mAgent(
            name = null,
            emits = S2mTransfers(
              probe = SizeRange(64)
            ),
            sinkId = SizeMapping(0, lockSets * 2)
          )
        ))
      ),

      cacheWays = cacheWays,
      cacheBytes = cacheBytes,
      aBufferCount = 4,
      downPendingMax = downPendingMax,
      probeCount = probeCount,
      blockSize = blockSize,
      probeRegion = _ => True
    )
  }

  SpinalVerilog(new Directory(basicConfig()))
}