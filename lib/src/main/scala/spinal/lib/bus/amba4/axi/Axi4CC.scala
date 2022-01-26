package spinal.lib.bus.amba4.axi

import spinal.core._
import spinal.lib._

case class Axi4ReadOnlyCC(axiConfig : Axi4Config,
                          inputCd : ClockDomain,
                          outputCd : ClockDomain,
                          arFifoSize : Int,
                          rFifoSize : Int) extends Component{
  val io = new Bundle {
    val input = slave(Axi4ReadOnly(axiConfig))
    val output = master(Axi4ReadOnly(axiConfig))
  }

  io.output.ar << io.input.ar.queue(arFifoSize, inputCd, outputCd)
  io.input.r   << io.output.r.queue(rFifoSize, outputCd, inputCd)
}


case class Axi4WriteOnlyCC (axiConfig : Axi4Config,
                            inputCd : ClockDomain,
                            outputCd : ClockDomain,
                            awFifoSize : Int,
                            wFifoSize : Int,
                            bFifoSize : Int) extends Component{
  val io = new Bundle {
    val input = slave(Axi4WriteOnly(axiConfig))
    val output = master(Axi4WriteOnly(axiConfig))
  }

  io.output.aw << io.input.aw.queue(awFifoSize, inputCd, outputCd)
  io.output.w  << io.input.w.queue(wFifoSize, inputCd, outputCd)
  io.input.b   << io.output.b.queue(bFifoSize, outputCd, inputCd)
}



case class Axi4CC(axiConfig : Axi4Config,
                  inputCd : ClockDomain,
                  outputCd : ClockDomain,
                  arFifoSize : Int,
                  awFifoSize : Int,
                  rFifoSize : Int,
                  wFifoSize : Int,
                  bFifoSize : Int) extends Component{
  val io = new Bundle {
    val input = slave(Axi4(axiConfig))
    val output = master(Axi4(axiConfig))
  }

  io.output.ar << io.input.ar.queue(arFifoSize, inputCd, outputCd)
  io.input.r   << io.output.r.queue(rFifoSize, outputCd, inputCd)
  io.output.aw << io.input.aw.queue(awFifoSize, inputCd, outputCd)
  io.output.w  << io.input.w.queue(wFifoSize, inputCd, outputCd)
  io.input.b   << io.output.b.queue(bFifoSize, outputCd, inputCd)

//  val readOnlyCc = Axi4ReadyOnlyCC(
//    axiConfig = axiConfig,
//    inputCd = inputCd,
//    outputCd = outputCd,
//    arFifoSize = arFifoSize,
//    rFifoSize = rFifoSize
//  )
//
//  val writeOnlyCc = Axi4WriteOnlyCC(
//    axiConfig = axiConfig,
//    inputCd = inputCd,
//    outputCd = outputCd,
//    awFifoSize = awFifoSize,
//    wFifoSize = wFifoSize,
//    bFifoSize = bFifoSize
//  )
//
//  readOnlyCc.io.input << io.input
//  writeOnlyCc.io.input << io.input
//  readOnlyCc.io.output >> io.output
//  writeOnlyCc.io.output >> io.output
}




case class Axi4SharedCC(axiConfig : Axi4Config,
                        inputCd : ClockDomain,
                        outputCd : ClockDomain,
                        arwFifoSize : Int,
                        rFifoSize : Int,
                        wFifoSize : Int,
                        bFifoSize : Int) extends Component{
  val io = new Bundle {
    val input = slave(Axi4Shared(axiConfig))
    val output = master(Axi4Shared(axiConfig))
  }

  io.output.arw << io.input.arw.queue(arwFifoSize, inputCd, outputCd)
  io.input.r   << io.output.r.queue(rFifoSize, outputCd, inputCd)
  io.output.w  << io.input.w.queue(wFifoSize, inputCd, outputCd)
  io.input.b   << io.output.b.queue(bFifoSize, outputCd, inputCd)
}
