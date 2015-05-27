package spinal.demo.mandelbrot

import spinal.core._
import spinal.lib.com.uart._
import spinal.lib._


case class SimpleMemoryWrite(addressWidth : Int,dataWidth : Int) extends Bundle{
  val address = UInt(addressWidth bit) //wordAddress
  val data = UInt(dataWidth bit)
}

class MandelbrotDemo (p : MandelbrotCoreParameters) extends Component{
  val io = new Bundle{
    val uart = master(Uart())
    val memoryWrite = master Handshake(SimpleMemoryWrite(32,32))
  }

  val uart = new Area {
    val ctrl = new UartCtrl()
    ctrl.io.clockDivider := BigInt((50e6 / 57.6e3 / 8).toLong)
    ctrl.io.config.dataLength := 7
    ctrl.io.config.parity := UartParityType.eParityNone
    ctrl.io.config.stop := UartStopType.eStop1bit
    ctrl.io.uart <> io.uart

    ctrl.io.write.valid := False
    ctrl.io.write.data := 0

    val (flowFragment, softReset) = ctrl.io.read.toFlowFragmentBitsAndReset()
  }

  val core = new Area{
    val mandelbrot = new MandelbrotCore(p)
    mandelbrot.io.cmdPort << uart.flowFragment

  }

  val memoryWrite = new Area{
    val pixelResult = core.mandelbrot.io.pixelResult
    val port = io.memoryWrite
    val counter = Reg(UInt(32 bit)) init(0)
    when(port.fire){
      counter := counter + 1
      when(pixelResult.last){
        counter := 0
      }
    }

    port.translateFrom(pixelResult)((to,from) => {
      to.address := counter
      to.data := from.fragment.iteration
    })
  }
}



//object MandelbrotDemo{
//  def main(args: Array[String]) {
//    SpinalVhdl(new MandelbrotDemo(new MandelbrotCoreParameters(64, 4, 64, 64, 7, 36)))
//  }
//}