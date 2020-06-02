package spinal.lib.bus.bmb

import spinal.core._
import spinal.lib._

case class BmbCcFifo(p: BmbParameter,
                     cmdDepth : Int,
                     rspDepth : Int,
                     inputCd: ClockDomain,
                     outputCd: ClockDomain) extends Component{
  val io = new Bundle {
    val input = slave(Bmb(p))
    val output = master(Bmb(p))
  }

  io.output.cmd << io.input.cmd.queue(cmdDepth, inputCd, outputCd)
  io.input.rsp << io.output.rsp.queue(rspDepth, outputCd, inputCd)
}


case class BmbCcToggle(p: BmbParameter,
                     inputCd: ClockDomain,
                     outputCd: ClockDomain) extends Component{
  val io = new Bundle {
    val input = slave(Bmb(p))
    val output = master(Bmb(p))
  }

  io.output.cmd << io.input.cmd.ccToggle(inputCd, outputCd)
  io.input.rsp << io.output.rsp.ccToggle(outputCd, inputCd)
}
