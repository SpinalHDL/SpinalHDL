package spinal.lib.bus.amba4.axi

import spinal.core._
import spinal.lib._
import spinal.lib.pipeline._

object Axi4WriteOnlyAligner{
  def getDownConfig(upConfig: Axi4Config, slotsCount : Int) = upConfig.copy(idWidth = log2Up(slotsCount))
}
// split bursts in bytesMax chunk, aligned on their size, may expend the head / last burst a bit to stay aligned
// Assume only one pending burst per ID
// Assume multi beat burst always have size to its max
// INCR only
class Axi4WriteOnlyAligner(upConfig: Axi4Config, bytesMax : Int, slotsCount : Int) extends Component {
  assert(isPow2(bytesMax))
  assert(isPow2(slotsCount))

  val downConfig = Axi4WriteOnlyAligner.getDownConfig(upConfig, slotsCount)

  val io = new Bundle {
    val up = slave port Axi4WriteOnly(upConfig)
    val down = master port Axi4WriteOnly(downConfig)
  }

  val downWordsMax = bytesMax/upConfig.bytePerWord
  val DownWordsType = HardType(UInt(log2Up(downWordsMax) bits))


  val blockRange = log2Up(bytesMax) - 1 downto 0
  val blockWordRange = log2Up(bytesMax) - 1 downto log2Up(upConfig.bytePerWord)

  var ptr = new Area{
    val cmd, fetch, rsp = Reg(UInt(log2Up(slotsCount)+1 bits)) init(0)
    val full = (cmd ^ rsp ^ slotsCount) === 0
  }

  val slots = for(i <- 0 until slotsCount) yield new Area{
    val done = Reg(Bool())
    val error = Reg(Bool())
  }

  case class Context() extends Bundle {
    val last = Bool()
    val id = upConfig.idType
  }
  val context = new Area{
    val mem = Mem.fill(slotsCount)(Context())
    val write = mem.writePort()
    val read = mem.readSyncPort()
  }

  case class WCmd() extends Bundle {
    val last = Bool()
    val header, ups, len = UInt(log2Up(downWordsMax) bits) //ups / len => -1
  }

  val frontend = new Pipeline{
    val split = new Stage {
      driveFrom(io.up.aw)
      val AW = insert(io.up.aw.payload)


      val spliter = new Area {
        val boundedStart = AW.addr.resize(Axi4.boundaryWidth)
        val boundedEnd = (AW.addr + (AW.len << log2Up(upConfig.bytePerWord))).resize(Axi4.boundaryWidth)
        val blockStart = boundedStart >> log2Up(bytesMax)
        val blockEnd = boundedEnd >> log2Up(bytesMax)
        val blockCount = blockEnd-blockStart

        val blockCounter = Reg(UInt(Axi4.boundaryWidth - log2Up(bytesMax) bits)) init(0)
        val LAST = insert(blockCounter === blockCount)
        val FIRST = insert(blockCounter === 0)

        val BOUNDED_BLOCK = insert(blockStart + blockCounter)

        val chunkWordStart = FIRST.mux(boundedStart(blockWordRange), U(0))
        val chunkWordEnd = LAST.mux(boundedEnd(blockWordRange).resized, U(downWordsMax-1))
        val CHUNKS_WORDS = insert(chunkWordEnd-chunkWordStart)

        forkIt(!LAST)
        when(isForked) {
          blockCounter := blockCounter + 1
          when(LAST) {
            blockCounter := 0
          }
        }
      }

      val aligner = new Area {
        val sizeToMask = AW.size.muxListDc((0 until log2Up(upConfig.bytePerWord)).map(e => e -> U((1 << e) - 1, log2Up(upConfig.bytePerWord) bits)))
        val mask = PropagateOnes.toLsb(spliter.chunkWordStart ^ spliter.chunkWordEnd) @@ (AW.len =/= 0).mux[UInt](upConfig.bytePerWord - 1, sizeToMask)
        val CHUNK_START = insert((spliter.boundedStart.resized & ~mask).andMask(spliter.FIRST))
        val CHUNK_END = insert((spliter.boundedEnd.resized | mask).orMask(!spliter.LAST))
        val LEN = insert(CHUNK_END(blockWordRange) - CHUNK_START(blockWordRange))
      }
    }

    import split.{AW}
    import split.spliter.{FIRST, LAST, BOUNDED_BLOCK}
    import split.aligner.{CHUNK_START, LEN}

    val allocateSlot = new Stage(Connection.M2S()){
      haltWhen(ptr.full)

      context.write.valid := False
      context.write.address := ptr.cmd.resized
      context.write.data.last := LAST
      context.write.data.id := AW.id
      when(isFireing) {
        context.write.valid := True
        slots.onSel(ptr.cmd.resized) { slot =>
          slot.done := False
          slot.error := False
        }
        ptr.cmd := ptr.cmd + 1
      }

      haltWhen(!io.down.aw.ready)
      io.down.aw.valid := valid && !ptr.full
      io.down.aw.payload := AW
      io.down.aw.addr(blockRange) := CHUNK_START
      io.down.aw.addr(Axi4.boundaryWidth-1 downto blockRange.high + 1) := BOUNDED_BLOCK
      io.down.aw.len.removeAssignments() := LEN.resized
      io.down.aw.id.removeAssignments() := ptr.cmd.resized

      val wCmd = Stream(WCmd())
      wCmd.arbitrationFrom(forkStream(!ptr.full))
      wCmd.last := LAST
      wCmd.header := (AW.addr.resized - CHUNK_START)(blockWordRange).andMask(FIRST)
      wCmd.ups := split.spliter.CHUNKS_WORDS
      wCmd.len := LEN
    }

    build()
  }

  val downW = new Area{
    val cmd = frontend.allocateSlot.wCmd.pipelined(m2s = true, s2m = true)
    val header, ups, len = Reg(UInt(log2Up(downWordsMax) bits)) init(0)
    val headerDone = header === cmd.header
    val upsDone = RegInit(False)
    val last = len === cmd.len

    io.down.w.valid := False
    io.down.w.data := io.up.w.data
    io.down.w.strb := io.up.w.strb.andMask(headerDone && !upsDone)
    io.down.w.last := last

    io.up.w.ready := False
    cmd.ready := False
    when(cmd.valid){
      when(!headerDone){
        io.down.w.valid := True
        header := header + io.down.w.ready.asUInt
      } elsewhen (!upsDone) {
        io.down.w.arbitrationFrom(io.up.w)
        ups := ups + io.down.w.ready.asUInt
        upsDone setWhen(ups === cmd.ups && io.down.w.ready)
      } otherwise {
        io.down.w.valid := True
      }
    }

    when(io.down.w.fire) {
      len := len + 1
      when(last) {
        List(header, ups, len).foreach(_ := 0)
        cmd.ready := True
        upsDone := False
      }
    }
  }

  val downB = new Area{
    io.down.b.ready := True
    when(io.down.b.fire) {
      slots.onSel(io.down.b.id) { slot =>
        slot.done := True
        slot.error := !io.down.b.isOKAY()
      }
    }
  }

  val upB = new Pipeline{
    val fetch = new Stage{
      valid := ptr.cmd =/= ptr.fetch
      context.read.cmd.valid := False
      context.read.cmd.payload := ptr.fetch.resized
      when(isFireing){
        context.read.cmd.valid := True
        ptr.fetch := ptr.fetch + 1
      }
    }
    val handle = new Stage(Connection.M2S()){
      val CTX = insert(context.read.rsp)
      val reader = slots.reader(ptr.rsp.resized)
      val done = reader(_.done)
      val error = reader(_.error)
      val errorAcc = RegInit(False) setWhen(isFireing && error)

      io.up.b.valid := False
      io.up.b.id := CTX.id
      io.up.b.setOKAY()
      when(error || errorAcc){ io.up.b.setSLVERR() }
      haltWhen(!done)
      haltWhen(io.up.b.isStall)
      when(isFireing){
        ptr.rsp := ptr.rsp + 1
      }
      when(valid && done) {
        when(CTX.last) {
          io.up.b.valid := True
          when(io.up.b.ready) {
            errorAcc := False
          }
        }
      }
    }
    build()
  }
}


object Axi4WriteOnlyAlignerGen extends App{
  SpinalVerilog(new Axi4WriteOnlyAligner(
    Axi4Config(16,32, 4),
    64,
    4
  ))
}