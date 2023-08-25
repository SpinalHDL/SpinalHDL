package spinal.lib.bus.amba4.axi

import spinal.core._
import spinal.lib._

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
    val smallSize = OHToUInt(OHMasking.last(bytes.take(log2Up(config.bytePerWord))))
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