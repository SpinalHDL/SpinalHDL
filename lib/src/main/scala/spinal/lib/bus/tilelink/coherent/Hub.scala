package spinal.lib.bus.tilelink.coherent

import spinal.core._
import spinal.lib._
import spinal.lib.bus.misc.SizeMapping
import spinal.lib.bus.tilelink._
import spinal.lib.pipeline._

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

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
  val io = new Bundle{
    val up = slave(Bus(ubp))
    //    val downPut = master(Bus(downPutBusParam))
    //    val downGet = master(Bus(downGetBusParam))

    //    val ordering = new Bundle{
    //      val toDownGet = master Flow(CoherentHubOrdering(p))
    //      val toDownPut = master Flow(CoherentHubOrdering(p))
    //      val bToT = master Flow(CoherentHubOrdering(p))
    //      def all = List(toDownGet, toDownPut, bToT)
    //    }

  }



  case class DataPayload() extends Bundle {
    val mask = ubp.mask()
    val data = ubp.data()
  }

  val initializer = new Area{
    val initCycles = p.sets
    val counter = Reg(UInt(log2Up(initCycles) + 1 bits)) init(0)
    val done = counter.msb
    when(!done) {
      counter := counter + 1
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

    val setsBusy = new Area{
      val hit, target = new Area {
        val mem = Mem.fill(sets)(Bool())
        val write = mem.writePort()
        write.valid := !initializer.done
        write.address := initializer.counter.resized
        write.data := False
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

      when(initializer.done) {
        setsBusy.target.write.valid := isFireing
        setsBusy.target.write.address := s0.CMD.address(setsRange)
        setsBusy.target.write.data := (s1.JUST_BUSY ? hit | !target)
      }
    }
  }

  val coherentMasters = unp.m.masters.filter(_.emits.withBCE)
  val coherentMasterCount = coherentMasters.size
  val coherentMasterToSource = coherentMasters.map(_.bSourceId)
  def MasterId() = UInt(log2Up(coherentMasterCount) bits)
  case class ProbeCtx() extends Bundle{
    val opcode  = Opcode.A()
    val address = ubp.address()
    val size    = ubp.size()
    val toTrunk = Bool()
    val source  = ubp.source()
    val bufferId = UInt(log2Up(aBufferCount) bits)
  }
  case class ProbeCmd() extends Bundle {
    val ctx = ProbeCtx()
    val fromBranch = Bool()
    val selfMask = Bits(coherentMasterCount bits)
    val mask = Bits(coherentMasterCount bits)
  }
  case class BackendCmd() extends Bundle{
    val address = ubp.address()
    val size    = ubp.size()
    val toTrunk = Bool()
    val source  = ubp.source()
  }

  val probe = new Area{
    val push = Stream(ProbeCmd())
    val pushCmd = frontendA.s2(frontendA.s0.CMD)
    push.valid := frontendA.s2.isValid
    frontendA.s2.haltIt(!push.ready)
    push.ctx.opcode     := pushCmd.opcode
    push.ctx.address    := pushCmd.address
    push.ctx.toTrunk    := pushCmd.param =/= Param.Grow.NtoB
    push.ctx.bufferId   := frontendA.s2(frontendA.BUFFER_ID)
    push.ctx.size       := pushCmd.size
    push.ctx.source     := pushCmd.source
    push.fromBranch := pushCmd.param === Param.Grow.BtoT
    push.selfMask   := B(coherentMasters.map(_.sourceHit(pushCmd.source)))
    push.mask := ~push.selfMask


    val slots = for(i <- 0 until probeCount) yield new Area{
      val fire = False
      val valid = RegInit(False) clearWhen(fire)
      val set = Reg(UInt(setsRange.size bits))
      val pending = Reg(UInt(log2Up(coherentMasterCount+1) bits))
      val lostIt = Reg(Bool())
      val probeAckDataCompleted = Reg(Bool())
      val selfId = Reg(UInt(log2Up(coherentMasterCount) bits))
      val done = valid && (pending & ~U(probeAckDataCompleted, widthOf(pending) bits)) === 0
    }

    val ctxMem = Mem.fill(probeCount)(ProbeCtx())
    val ctxWrite = ctxMem.writePort()

    val cmd = new Area{
      val sloted = RegInit(False)
      val full = slots.map(_.valid).andR
      val freeMask = OHMasking.firstV2(B(slots.map(!_.valid)))
      val freeId = OHToUInt(freeMask)
      val requestsUnmasked = (push.mask & ~push.selfMask) | push.selfMask.andMask(push.fromBranch)
      val pending = CountOne(requestsUnmasked)
      val selfId = OHToUInt(push.selfMask)

      ctxWrite.valid := False
      ctxWrite.address := freeId
      ctxWrite.data := push.ctx
      when(push.valid && !sloted && !full){
        slots.onMask(freeMask){s =>
          s.valid := True
          s.set := push.ctx.address(setsRange)
          s.pending := pending
          s.lostIt := False
          s.probeAckDataCompleted := False
          s.selfId := selfId
        }
        ctxWrite.valid := True
        sloted := True
      }

      val bus = io.up.b
      val halted = push.haltWhen(!sloted && full)
      val fired = Reg(Bits(coherentMasterCount bits)) init(0)
      val requests = requestsUnmasked & ~fired
      val masterOh = OHMasking.firstV2(requests)
      val selfProbe = (masterOh & halted.selfMask).orR
      bus.valid   := halted.valid && requests.orR
      bus.opcode  := Opcode.B.PROBE_BLOCK
      bus.param   := (halted.ctx.toTrunk && !selfProbe) ? B(Param.Cap.toN, 3 bits) | B(Param.Cap.toB, 3 bits)
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
      val ack = io.up.c.fire && io.up.c.opcode === Opcode.C.PROBE_ACK
      val hitOh = slots.map(s => s.valid && s.set === io.up.c.address(setsRange))
      val hitId = OHToUInt(hitOh)
      val pending = slots.map(_.pending).read(hitId)
      val selfId = slots.map(_.selfId).read(hitId)
      val pendingNext = pending - 1
      val masterOh = coherentMasterToSource.map(io.up.c.source === _).asBits
      val masterId = OHToUInt(masterOh)
      val isSelf = selfId === masterId
      val lostIt = isSelf && io.up.c.param === Param.Report.NtoN

      when(ack) {
        slots.onMask(hitOh) { s =>
          s.pending := pendingNext
          s.lostIt setWhen (lostIt)
        }
      }
    }

    val wake = new Pipeline{
      val s0 = new Stage{
        val hits = slots.map(_.done).asBits
        valid := hits.orR
        val hitOh = OHMasking.roundRobinNext(hits, isFireing)
        val hitId = OHToUInt(hitOh)
        when(isFireing){
          slots.onMask(hitOh){ s =>
            s.fire := True
          }
        }
      }
      val s1 = new Stage(Connection.M2S()){
        val readed = ctxMem.readSync(s0.hitId, isReady)
        val CTX = insert(readed)

        val counter = Reg(UInt(wordRange.size bits)) init(0)
        val LAST = insert(!Opcode.A.withData(CTX.opcode) ||  counter === sizeToBeatMinusOne(ubp, CTX.size))
        CTX.address(wordRange) := readed.address(wordRange) + counter
        val wordAddress = CTX.bufferId @@ CTX.address(wordRange)

        when(isForked){
          counter := counter + 1
          when(LAST){
            counter := 0
          }
        }
        forkIt(!LAST)
      }
      val s2 = new Stage(Connection.M2S()){
        val DATA = insert(frontendA.buffer.ram.readSync(s1.wordAddress, isReady))
      }
    }
  }


  val backend = new Pipeline{
//    val
  }




//  probe.slots.foreach(e => out(e.pendings))
//  master(probe.rsp.outputC.halfPipe())
  io.up.c.ready := True
  out(probe.wake.s2.isFireing, probe.wake.s2(probe.wake.s2.DATA))
  out(frontendA.s2.isFireing)
  out(frontendA.setsBusy.hit.write)
  out(probe.slots.map(_.lostIt).asBits)
  io.up.d.setIdle()
  io.up.e.ready := False

  probe.wake.s2.haltIt(in(Bool()))

  frontendA.build()
  probe.wake.build()
}



object HubGen extends App{
  def basicConfig(slotsCount : Int = 2, masterPerChannel : Int = 4, dataWidth : Int = 64, addressWidth : Int = 13, cacheSize : Int = 1024) = {
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
            sinkId = SizeMapping(0, slotsCount)
          )
        ))
      ),
      slotCount = slotsCount,
      cacheSize = cacheSize,
      wayCount  = 2,
      lineSize  = 64
    )
  }
  def gen = new Hub(HubGen.basicConfig(8, masterPerChannel = 4, dataWidth = 16, addressWidth = 32, cacheSize = 32*1024))
  SpinalVerilog(gen)

}

object HubSynt extends App{
  import spinal.lib.eda.bench._
  val rtls = ArrayBuffer[Rtl]()
  for(slots <- List(16)) { //Rtl.ffIo
    rtls += Rtl(SpinalVerilog((new Hub(HubGen.basicConfig(slots, masterPerChannel = 4, dataWidth = 16, addressWidth = 32, cacheSize = 32*1024)).setDefinitionName(s"Hub$slots"))))
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

 */