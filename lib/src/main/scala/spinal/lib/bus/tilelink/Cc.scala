package spinal.lib.bus.tilelink

import spinal.core._
import spinal.lib._

case class FifoCc(busParameter: BusParameter,
                  inputCd : ClockDomain,
                  outputCd : ClockDomain,
                  aDepth : Int,
                  bDepth : Int,
                  cDepth : Int,
                  dDepth : Int,
                  eDepth : Int) extends Component{
  val io = new Bundle{
    val input = slave(Bus(busParameter))
    val output = master(Bus(busParameter))
  }
  val a = StreamFifoCC(io.input.a, io.output.a, aDepth, inputCd, outputCd)
  val b = busParameter.withBCE generate StreamFifoCC(io.output.b, io.input.b, bDepth, outputCd, inputCd)
  val c = busParameter.withBCE generate StreamFifoCC(io.input.c, io.output.c, cDepth, inputCd, outputCd)
  val d = StreamFifoCC(io.output.d, io.input.d, dDepth, outputCd, inputCd)
  val e = busParameter.withBCE generate StreamFifoCC(io.input.e, io.output.e, eDepth, inputCd, outputCd)
}
