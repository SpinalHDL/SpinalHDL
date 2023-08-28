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
    val bufferize = new Stage() {
      driveFrom(io.up.aw)
      val AW = insert(io.up.aw.payload)
    }

    import bufferize.AW

    val split = new Stage(Connection.M2S()) {
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
        val sizeToMask = AW.size.muxListDc((0 to log2Up(upConfig.bytePerWord)).map(e => e -> U((1 << e) - 1, log2Up(upConfig.bytePerWord) bits)))
        val mask = PropagateOnes.toLsb(spliter.chunkWordStart ^ spliter.chunkWordEnd) @@ (AW.len =/= 0).mux[UInt](upConfig.bytePerWord - 1, sizeToMask) // Which address bits we need to remove
        val CHUNK_START = insert((spliter.boundedStart.resized & ~mask).andMask(spliter.FIRST))
        val CHUNK_END = insert((spliter.boundedEnd.resized | mask).orMask(!spliter.LAST))
        val LEN = insert(CHUNK_END(blockWordRange) - CHUNK_START(blockWordRange))
      }
    }

    import split.spliter.{FIRST, LAST, BOUNDED_BLOCK}
    import split.aligner.{CHUNK_START, CHUNK_END, LEN}

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

      val bytes = CHUNK_END - CHUNK_START
      val smallSize = (0 until upConfig.bytePerWord).map(e => U(log2Up(e+1))).read(bytes.take(log2Up(upConfig.bytePerWord)).asUInt)
      when(LEN === 0){
        io.down.aw.size := smallSize.resized
      }

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

object Axi4ReadOnlyAligner{
  def getDownConfig(upConfig: Axi4Config, slotsCount : Int) = upConfig.copy(idWidth = log2Up(slotsCount))
}

// split bursts in bytesMax chunk, aligned on their size, may expend the head / last burst a bit to stay aligned
// Assume only one pending burst per ID
// Assume multi beat burst always have size to its max
// INCR only
// Assume no R beat interleaving
class Axi4ReadOnlyAligner(upConfig: Axi4Config, bytesMax : Int, slotsCount : Int) extends Component {
  assert(isPow2(bytesMax))
  assert(isPow2(slotsCount))

  val downConfig = Axi4ReadOnlyAligner.getDownConfig(upConfig, slotsCount)

  val io = new Bundle {
    val up = slave port Axi4ReadOnly(upConfig)
    val down = master port Axi4ReadOnly(downConfig)
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
    val header, ups = UInt(log2Up(downWordsMax) bits)
  }
  val context = new Area{
    val mem = Mem.fill(slotsCount)(Context())
    val write = mem.writePort()
    val read = mem.readSyncPort()
  }
  val rData = new Area {
    val mem = Mem.fill(slotsCount*downWordsMax)(Bits(upConfig.dataWidth bits))
    val write = mem.writePort()
    val read = mem.readSyncPort()
  }



  val frontend = new Pipeline{
    val bufferize = new Stage(){
      driveFrom(io.up.ar)
      val AR = insert(io.up.ar.payload)
    }
    import bufferize.AR

    val split = new Stage(Connection.M2S()) {
      val spliter = new Area {
        val boundedStart = AR.addr.resize(Axi4.boundaryWidth)
        val boundedEnd = (AR.addr + (AR.len << log2Up(upConfig.bytePerWord))).resize(Axi4.boundaryWidth)
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
        val sizeToMask = AR.size.muxListDc((0 to log2Up(upConfig.bytePerWord)).map(e => e -> U((1 << e) - 1, log2Up(upConfig.bytePerWord) bits)))
        val mask = PropagateOnes.toLsb(spliter.chunkWordStart ^ spliter.chunkWordEnd) @@ (AR.len =/= 0).mux[UInt](upConfig.bytePerWord - 1, sizeToMask) // Which address bits we need to remove
        val CHUNK_START = insert((spliter.boundedStart.resized & ~mask).andMask(spliter.FIRST))
        val CHUNK_END = insert((spliter.boundedEnd.resized | mask).orMask(!spliter.LAST))
        val LEN = insert(CHUNK_END(blockWordRange) - CHUNK_START(blockWordRange))
      }
    }

    import split.spliter.{FIRST, LAST, BOUNDED_BLOCK}
    import split.aligner.{CHUNK_START, CHUNK_END, LEN}

    val allocateSlot = new Stage(Connection.M2S()){
      haltWhen(ptr.full)

      context.write.valid := False
      context.write.address := ptr.cmd.resized
      context.write.data.last := LAST
      context.write.data.id := AR.id
      context.write.data.header := (AR.addr.resized - CHUNK_START)(blockWordRange).andMask(FIRST)
      context.write.data.ups := split.spliter.CHUNKS_WORDS
      when(isFireing) {
        context.write.valid := True
        slots.onSel(ptr.cmd.resized) { slot =>
          slot.done := False
          slot.error := False
        }
        ptr.cmd := ptr.cmd + 1
      }

      haltWhen(!io.down.ar.ready)
      io.down.ar.valid := valid && !ptr.full
      io.down.ar.payload := AR
      io.down.ar.addr(blockRange) := CHUNK_START
      io.down.ar.addr(Axi4.boundaryWidth-1 downto blockRange.high + 1) := BOUNDED_BLOCK
      io.down.ar.len.removeAssignments() := LEN.resized
      io.down.ar.id.removeAssignments() := ptr.cmd.resized
      val bytes = CHUNK_END - CHUNK_START
      val smallSize = (0 until upConfig.bytePerWord).map(e => U(log2Up(e + 1))).read(bytes.take(log2Up(upConfig.bytePerWord)).asUInt)
      when(LEN === 0) {
        io.down.ar.size := smallSize.resized
      }
    }

    build()
  }

  val downR = new Area{
    val counter = Reg(UInt(blockWordRange.size bits)) init(0)
    io.down.r.ready := True
    rData.write.valid := False
    rData.write.address := io.down.r.id @@ counter
    rData.write.data := io.down.r.data
    when(io.down.r.fire) {
      rData.write.valid := True
      counter := counter + 1
      when(io.down.r.last) {
        counter := 0
        slots.onSel(io.down.r.id) { slot =>
          slot.done := True
          slot.error := !io.down.r.isOKAY()
        }
      }
    }
  }

  val upR = new Pipeline{
    val fetch = new Stage{
      valid := ptr.cmd =/= ptr.fetch
      context.read.cmd.valid := False
      context.read.cmd.payload := ptr.fetch.resized
      when(isFireing){
        context.read.cmd.valid := True
        ptr.fetch := ptr.fetch + 1
      }
    }

    val handle = new Stage(Connection.M2S()) {
      val CTX = insert(context.read.rsp)
      val reader = slots.reader(ptr.rsp.resized)
      val done = reader(_.done)
      val error = reader(_.error)
      val errorAcc = RegInit(False) setWhen (isFireing && error)
      val counter = Reg(UInt(blockWordRange.size bits)) init (0)
      val CHUNK_LAST = counter === CTX.ups
      val ERRORED = insert(error || errorAcc)

      haltWhen(!done)

      rData.read.cmd.valid := False
      rData.read.cmd.payload := U(ptr.rsp.dropHigh(1) ## (CTX.header + counter))
      when(valid && done) {
        when(!CHUNK_LAST) {
          forkIt()
        }
        when(isForked) {
          rData.read.cmd.valid := True
          counter := counter + 1
          when(CHUNK_LAST) {
            counter := 0
            errorAcc := False
            ptr.rsp := ptr.rsp + 1
          }
        }
      }
    }

    import handle.{ERRORED, CTX, CHUNK_LAST}
    val rsp = new Stage(Connection.M2S()) {
      haltWhen(io.up.r.isStall)
      io.up.r.valid := valid
      io.up.r.last := CTX.last && CHUNK_LAST
      io.up.r.id := CTX.id
      io.up.r.data := rData.read.rsp
      io.up.r.setOKAY()
      when(ERRORED){ io.up.r.setSLVERR() }
    }
    build()
  }
}


object Axi4ReadOnlyAlignerGen extends App{
  SpinalVerilog(new Axi4ReadOnlyAligner(
    Axi4Config(16,32, 4),
    64,
    4
  ))
}
