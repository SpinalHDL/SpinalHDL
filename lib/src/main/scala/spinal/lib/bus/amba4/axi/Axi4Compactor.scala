package spinal.lib.bus.amba4.axi

import spinal.core._
import spinal.lib._
import spinal.lib.pipeline._

class Axi4WriteOnlyCompactor(config: Axi4Config) extends Component {
  val io = new Bundle {
    val up = slave port Axi4WriteOnly(config)
    val down = master port Axi4WriteOnly(config)
  }

  val offsetRange = log2Up(config.bytePerWord)-1 downto 0

  case class Context() extends Bundle{
    val len = UInt(8 bits)
    val bytePerBeat = UInt(log2Up(config.bytePerWord) bits)
    val offset = UInt(offsetRange.size bits)
  }

  val onAw = new Area{
    val (downFork, wFork) = StreamFork2(io.up.aw)
    io.down.aw << downFork
    val sizeBytes = io.up.aw.size.muxListDc(
      (0 to log2Up(config.bytePerWord)).map(i =>
        i -> U((1 << i)-1, log2Up(config.bytePerWord) bits)
      )
    )
    val bytes = (io.up.aw.len << io.up.aw.size) + sizeBytes
    val bytesBeats = bytes + (io.up.aw.addr(log2Up(config.bytePerWord)-1 downto 0) & ~U(UIntToOhMinusOne(io.up.aw.size, log2Up(config.bytePerWord))))
    val smallSize = (0 until config.bytePerWord).map(e => U(log2Up(e+1))).read(bytesBeats.take(log2Up(config.bytePerWord)).asUInt)
    io.down.aw.size.removeAssignments() := io.down.aw.len.orR.mux[UInt](log2Up(config.bytePerWord), smallSize).resized
    io.down.aw.len.removeAssignments() := (bytesBeats >> log2Up(config.bytePerWord)).resized

    val toW = Stream(Context())
    toW.arbitrationFrom(wFork)
    toW.len := wFork.len
    toW.bytePerBeat := ((U(1) << wFork.size)-1).resized
    toW.offset := wFork.addr(offsetRange) & ~toW.bytePerBeat.resize(offsetRange.size)
  }

  val onW = new Area{
    val awS2m = onAw.toW.s2mPipe()
    val aw = awS2m.m2sPipe()
    val w = io.up.w
    val commitCmd = Stream(NoData)
    val commitDo = commitCmd.m2sPipe()
    val firstCycle = RegNext(False) init(True)
    val buffer = List.fill(config.bytePerWord)(new Area{
      val data = Reg(Bits(8 bits))
      val strb = Reg(Bool()) clearWhen(commitDo.fire || firstCycle)
    })
    val bufferLast = Reg(Bool())

    val mask = ~aw.bytePerBeat.resize(offsetRange.size)
    val counter = Reg(UInt(log2Up(config.bytePerWord) bits)) init(0)
    val counterPlus = counter +^ aw.bytePerBeat + 1
    val last = counterPlus.msb || w.last

    aw.ready := False
    w.ready := False
    commitCmd.valid := False

    when(aw.valid && w.valid){
      w.ready := !commitDo.isStall
      commitCmd.valid := last
    }

    when(w.fire){
      counter := counterPlus.resized
      for ((buf, i) <- buffer.zipWithIndex) {
        when(counter === (U(i) & mask)) {
          buf.data := w.data(i * 8, 8 bits)
          buf.strb := w.strb(i)
        }
      }
      bufferLast := w.last
      aw.ready := w.last
    }

    when(awS2m.fire){
      counter := awS2m.offset
    }

    io.down.w.arbitrationFrom(commitDo)
    io.down.w.data := Cat(buffer.map(_.data))
    io.down.w.strb := Cat(buffer.map(_.strb))
    io.down.w.last := bufferLast
  }

  io.up.b << io.down.b
}


object Axi4WriteOnlyCompactorGen extends App{
  SpinalVerilog(new Axi4WriteOnlyCompactor(
    Axi4Config(16,32, 4)
  ))
}



//Assume no interleaving
//Assume not more than one pending transaction per ID
class Axi4ReadOnlyCompactor(config: Axi4Config) extends Component {
  val io = new Bundle {
    val up = slave port Axi4ReadOnly(config)
    val down = master port Axi4ReadOnly(config)
  }

  val offsetRange = log2Up(config.bytePerWord)-1 downto 0

  case class Context() extends Bundle{
    val len = UInt(8 bits)
    val bytePerBeat = UInt(log2Up(config.bytePerWord) bits)
    val offset = UInt(offsetRange.size bits)
  }

  val context = new Area {
    val mem = Mem.fill(1 << config.idWidth)(Context())
    val write = mem.writePort()
    val read = mem.readSyncPort()
  }

  val onAr = new Area{
    io.down.ar << io.up.ar
    val sizeBytes = io.up.ar.size.muxListDc(
      (0 to log2Up(config.bytePerWord)).map(i =>
        i -> U((1 << i)-1, log2Up(config.bytePerWord) bits)
      )
    )
    val bytes = (io.up.ar.len << io.up.ar.size) + sizeBytes
    val bytesBeats = bytes + (io.up.ar.addr(log2Up(config.bytePerWord)-1 downto 0) & ~U(UIntToOhMinusOne(io.up.ar.size, log2Up(config.bytePerWord))))
    val smallSize = (0 until config.bytePerWord).map(e => U(log2Up(e+1))).read(bytesBeats.take(log2Up(config.bytePerWord)).asUInt)
    io.down.ar.size.removeAssignments() := io.down.ar.len.orR.mux[UInt](log2Up(config.bytePerWord), smallSize).resized
    io.down.ar.len.removeAssignments() := (bytesBeats >> log2Up(config.bytePerWord)).resized


    context.write.valid := io.down.ar.fire
    context.write.address := io.up.ar.id
    context.write.data.len := io.up.ar.len
    context.write.data.bytePerBeat := ((U(1) << io.up.ar.size)-1).resized
    context.write.data.offset := io.up.ar.addr(offsetRange) & ~context.write.data.bytePerBeat.resize(offsetRange.size)
  }


  val onR = new Pipeline{
    val fetch = new Stage{
      driveFrom(io.down.r)
      val R = insert(io.down.r.payload)
      context.read.cmd.valid := isFireing
      context.read.cmd.payload := io.down.r.id
    }

    val process = new Stage(Connection.M2S()){
      val CTX = insert(context.read.rsp)
      val first = RegInit(True)
      when(io.up.r.fire) {
        first := io.up.r.last
      }

      val lenCounter = Reg(UInt(8 bits)) init (0)
      val lenLast = lenCounter === CTX.len

      val wordCounter = Reg(UInt(log2Up(config.bytePerWord) bits)) init(0)
      val wordCounterPlus = wordCounter +^ CTX.bytePerBeat + CTX.offset.andMask(first) + 1
      val wordLast = wordCounterPlus.msb || lenLast

      haltWhen(!wordLast)
      haltWhen(!io.up.r.ready)

      io.up.r.valid := valid
      io.up.r.payload := fetch.R
      io.up.r.last.removeAssignments() := lenLast

      when(io.up.r.fire){
        wordCounter := wordCounterPlus.resized
        lenCounter := lenCounter + 1
        when(io.up.r.last) {
          lenCounter := 0
          wordCounter := 0
        }
      }
    }

    build()
  }
}


object Axi4ReadOnlyCompactorGen extends App{
  SpinalVerilog(new Axi4ReadOnlyCompactor(
    Axi4Config(16,32, 4)
  ))
}