package spinal.demo.mandelbrot

import spinal.core._
import spinal.lib._
import spinal.lib.bus.axilite._
import spinal.lib.com.uart._
import spinal.lib.graphic.Rgb
import spinal.lib.graphic.vga._


class MandelbrotDemo(p: MandelbrotCoreParameters) extends Component {
  val vgaAxiConfig = AxiLiteConfig(32, 32)
  val vgaCoreConfig = AxiLiteConfig(32, 32)
  val rgbType = Rgb(8, 8, 8)

  val io = new Bundle {
    val uart = master(Uart())
    val coreAxi = master(AxiLiteWriteOnly(vgaAxiConfig))
    val vgaAxi = master(AxiLiteReadOnly(vgaAxiConfig))
    val vga = master(Vga(rgbType))
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

  val core = new Area {
    val mandelbrot = new MandelbrotCore(p)
    mandelbrot.io.cmdPort << uart.flowFragment

    //Take mandelbrot pixelResults and translate them into simple memory access
    val counter = Reg(UInt(32 bit)) init (0)
    case class AddrData() extends Bundle {
      val address = UInt(vgaCoreConfig.addressWidth bit)
      val data = Bits(vgaCoreConfig.dataWidth bit)
    }

    val addrdata = Stream(AddrData()).translateFrom(mandelbrot.io.pixelResult)((to, from) => {
      to.address := counter
      to.data := toBits(from.fragment.iteration)
    })

    
    when(addrdata.fire) {
      counter := counter + 1
      when(mandelbrot.io.pixelResult.last) {
        counter := 0
      }
    }

    //Take the simple memory access (view precedent comment) and translate it into AXI-Lite access
    val (forkedCmd,forkedData) = StreamFork2(addrdata)
    io.coreAxi.writeCmd.translateFrom(forkedCmd)((to,from) => {
      to.addr := from.address
      to.setUnprivileged
    })
    io.coreAxi.writeData.translateFrom(forkedData)((to,from) => {
      to.data := from.data
      to.setStrb
    })
    io.coreAxi.writeRet.freeRun
  }

  val vga = new Area {
    //Create VGA controller
    val ctrl = new VgaCtrl(rgbType, 12)
    ctrl.io.softReset := False
    ctrl.io.timings.setAs_h640_v480_r60
    io.vga := Delay(ctrl.io.vga, 1)


    //Create the DMA for this VGA controller
    val dma = new AxiLiteSimpleReadDma(vgaAxiConfig)
    dma.io.run.translateFrom(ctrl.io.frameStart.genEvent)((to, from) => {
      to.offset := 0
      to.endAt := (p.screenResX * p.screenResY - 1) * vgaAxiConfig.dataByteCount
    })
    dma.io.axi >> io.vgaAxi
    ctrl.io.colorLink.translateFrom(dma.io.read)((to, from) => {
      to.assignFromBits(from)
    })
  }
}


object MandelbrotDemo {
  def main(args: Array[String]) {
    SpinalVhdl(new MandelbrotDemo(new MandelbrotCoreParameters(64, 4, 64, 64, 7, 36)))
  }
}