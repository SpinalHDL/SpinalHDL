package spinal.demo.mandelbrot

import spinal.core._
import spinal.lib._
import spinal.lib.bus.sbl._
import spinal.lib.bus.axilite._
import spinal.lib.com.uart._
import spinal.lib.graphic.Rgb
import spinal.lib.graphic.vga._


class MandelbrotSblDemo(frameAddressOffset : Int,p: MandelbrotCoreParameters,vgaClk : ClockDomain,coreClk : ClockDomain) extends Component {
  val memoryBusConfig = SblConfig(30, 32)
  val rgbType = Rgb(8, 8, 8)

  val io = new Bundle {
    val uart = master(Uart())
    val mandelbrotWriteCmd = master Stream(SblWriteCmd(memoryBusConfig))
    val vgaReadCmd = master Stream(SblReadCmd(memoryBusConfig))
    val vgaReadRet = slave Flow(SblReadRet(memoryBusConfig))
    val vga = master(Vga(rgbType))
  }





  val core = new ClockingArea(coreClk) {
    val uart = new Area {
      val ctrl = new UartCtrl()
      ctrl.io.clockDivider := BigInt((100e6 / 57.6e3 / 8).toLong)
      ctrl.io.config.dataLength := 7
      ctrl.io.config.parity := UartParityType.eParityNone
      ctrl.io.config.stop := UartStopType.eStop1bit
      ctrl.io.uart <> io.uart

      ctrl.io.write.valid := False
      ctrl.io.write.data := 0

      val (flowFragment, softReset) = ctrl.io.read.toFlowFragmentBitsAndReset()
    }

    val mandelbrot = new Area{
      val core = new MandelbrotCore(p)
      core.io.cmdPort << uart.flowFragment

      //Take mandelbrot pixelResults and translate them into simple memory access
      val counter = Reg(UInt(32 bit)) init (0)
      when(io.mandelbrotWriteCmd.fire) {
        counter := counter + 1
        when(core.io.pixelResult.last) {
          counter := 0
        }
      }
      io.mandelbrotWriteCmd.translateFrom(core.io.pixelResult)((to, from) => {
        to.address := counter
        to.data := toBits(from.fragment.iteration)
      })      
    }
  }

  val vga = new ClockingArea(vgaClk) {
    //Create VGA controller
    val ctrl = new VgaCtrl(rgbType, 12)
    ctrl.io.softReset := False
    ctrl.io.timings.setAs_h640_v480_r60  //Static timing for 640*480 pixel at 60HZ
    io.vga := Delay(ctrl.io.vga, 1)      //Delay is used to do some output pipelining

    //Create the DMA for this VGA controller
    val dma = new SblReadDma(memoryBusConfig)

    //Take the frameStart pulse from the VGA controller and translate it into a DMA command
    dma.io.cmd.translateFrom(ctrl.io.frameStart.genEvent)((to, from) => {
      to.offset := frameAddressOffset
      to.endAt := frameAddressOffset + p.screenResX * p.screenResY - 1
    })

    val pendingCmd = Reg(UInt(10 bit)) init(0)
    when(io.vgaReadCmd.fire !== ctrl.io.colorStream.fire){
      when(io.vgaReadCmd.fire){
        pendingCmd := pendingCmd + 1
      } otherwise{
        pendingCmd := pendingCmd - 1
      }
    }

    io.vgaReadCmd << dma.io.sblReadCmd.haltWhen(pendingCmd === 512)


    //Convert the memory read flow of bits into a flow of Rgb color
    val colorFlow = Flow(rgbType).translateFrom(io.vgaReadRet)((to, from) => {
      to.assignFromBits(from.data)
    })
    // Convert this color flow into a stream flow by using a fifo implementation
    // 256 Words, dmaHalt asserted when occupancy is higher than 196
    ctrl.io.colorStream << colorFlow.toStream.queue(512)
  }
}

object MandelbrotSblDemo {
  def main(args: Array[String]) {
    SpinalVhdl({
      val vgaClock = ClockDomain("vga")
      val coreClock = ClockDomain("core")
      new MandelbrotSblDemo(0,new MandelbrotCoreParameters(255, 1, 640, 480, 7, 36),vgaClock,coreClock)
    })
  }
}