package spinal.demo.mandelbrot


import java.awt.Color

import spinal.core._
import spinal.lib._
import spinal.lib.experimental.bus.sbl._
import spinal.lib.com.uart._
import spinal.lib.graphic.{RgbConfig, Rgb}
import spinal.lib.graphic.vga._


class MandelbrotSblDemo(frameAddressOffset: Int, p: MandelbrotCoreParameters, coreClk: ClockDomain, vgaMemoryClk: ClockDomain, vgaClk: ClockDomain) extends Component {
  val memoryBusConfig = SblConfig(30, 32)
  val rgbConfig = RgbConfig(8, 8, 8)

  val io = new Bundle {
    val uart = master(Uart())

    val mandelbrotWriteCmd = master Stream SblWriteCmd(memoryBusConfig)

    val vgaReadCmd = master Stream SblReadCmd(memoryBusConfig)
    val vgaReadRet = slave Flow SblReadRet(memoryBusConfig)

    val vga = master(Vga(rgbConfig))
  }
  val core = new ClockingArea(coreClk) {
    val uart = new Area {
      val ctrl = new UartCtrl()
      ctrl.io.config.clockDivider := BigInt((coreClk.frequency.getValue / 57.6e3 / 8).toLong)
      ctrl.io.config.frame.dataLength := 7
      ctrl.io.config.frame.parity := UartParityType.NONE
      ctrl.io.config.frame.stop := UartStopType.ONE
      ctrl.io.uart <> io.uart
      
      val (flowFragment, _) = ctrl.io.read.toFlow.toFlowFragmentBitsAndReset()
    }
    val mandelbrot = new Area {
      val core = new MandelbrotCore(p)
      core.io.cmdPort << uart.flowFragment
      core.io.retPort.toStreamBits() >> uart.ctrl.io.write

      //Mandelbrot iteration to color palette definition
      val palette = Seq(
        (0/255.0 -> new Color(0,0,0)),
        (64/255.0 -> new Color(128,0,255)),
        (100/255.0 -> new Color(180,32,0)),
        (128/255.0 -> new Color(200,64,0)),
        (150/255.0 -> new Color(220,100,0)),
        (255/255.0 -> new Color(255,255,0)))

      //Create the rom for the mandelbrot iteration count to color transformation
      val paletteRom = Mem((0 to p.iterationLimit).map(iteration => {
        val iterationFactor = 1.0 * iteration / (p.iterationLimit)
        val rgb = Rgb(rgbConfig)
        var palletOffset = Math.max(0, palette.indexWhere(_._1 >= iterationFactor) - 1)
        val palletPre = palette(palletOffset)
        val palletPost = palette(palletOffset + 1)
        val ratio = (iterationFactor - palletPre._1) / (palletPost._1 - palletPre._1)


        rgb.r := ((palletPre._2.getRed * (1.0 - ratio) + palletPost._2.getRed * ratio) / 255 * ((1 << rgbConfig.rWidth) - 1)).toInt
        rgb.g := ((palletPre._2.getGreen * (1.0 - ratio) + palletPost._2.getGreen * ratio) / 255 * ((1 << rgbConfig.gWidth) - 1)).toInt
        rgb.b := ((palletPre._2.getBlue * (1.0 - ratio) + palletPost._2.getBlue * ratio) / 255 * ((1 << rgbConfig.bWidth) - 1)).toInt

        rgb
      }))

      //Translate the pixelResult stream into a colorResult by using the rom
      val colorResult = paletteRom.streamReadSync(core.io.pixelResult.translateWith(core.io.pixelResult.fragment.iteration), core.io.pixelResult.last)

      //Take mandelbrot pixelResults and translate them into simple memory access
      val counter = Reg(UInt(memoryBusConfig.addressWidth bit)) init (0)
      when(io.mandelbrotWriteCmd.fire) {
        counter := counter + 1
        when(colorResult.linked) {
          counter := 0
        }
      }
      io.mandelbrotWriteCmd.translateFrom(colorResult)((to, from) => {
        to.address := counter
        to.data := (B(from.value)).resized
      })
    }
  }
  val vga = new ClockingArea(vgaClk) {
    //Create VGA controller
    val ctrl = new VgaCtrl(rgbConfig, 12)
    ctrl.io.softReset := False
    ctrl.io.timings.setAs_h640_v480_r60 //Static timing for 640*480 pixel at 60HZ
    io.vga := ctrl.io.vga

    val newFrameEvent = ctrl.io.frameStart.genEvent
  }
  val vgaMemory = new ClockingArea(vgaMemoryClk) {
    //Create the DMA for this VGA controller
    val dma = new SblReadDma(memoryBusConfig)

    //Synchronise the frameStart event from the VGA to the current clock domain
    val frameStart = StreamCCByToggle(vga.newFrameEvent, vgaClk, vgaMemoryClk)
    //Translate it into a DMA command and send it into the DMA
//    dma.io.cmd.translateFrom(frameStart)((to, from) => {
//      to.offset := frameAddressOffset
//      to.endAt := frameAddressOffset + p.screenResX * p.screenResY - 1
//    })

    dma.io.cmd.arbitrationFrom(frameStart)
    dma.io.cmd.offset := frameAddressOffset
    dma.io.cmd.endAt := frameAddressOffset + p.screenResX * p.screenResY - 1


    //Count pendings command on the vgaRead bus
    val pendingCmd = Reg(UInt(6 bit)) init (0)
    when(io.vgaReadCmd.fire =/= io.vgaReadRet.fire) {
      when(io.vgaReadCmd.fire) {
        pendingCmd := pendingCmd + 1
      } otherwise {
        pendingCmd := pendingCmd - 1
      }
    }

    //Translate bus memory bus read Flow into a color read flow
//    val colorFlow = Flow(rgbType).translateFrom(io.vgaReadRet)((to, from) => {
//      to.assignFromBits(from.data)
//    })

    val colorFlow = Flow(Rgb(rgbConfig))
    colorFlow.valid := io.vgaReadRet.valid
    colorFlow.payload.assignFromBits(io.vgaReadRet.payload.data)

    //Translate the color Flow ino a Stream and synchronise/bufferise to the VgaClk by using a cross clock fifo
    val fifoSize = 512
    val (colorStream, colorStreamOccupancy) = colorFlow.toStream.queueWithPushOccupancy(fifoSize, vgaMemoryClk, vgaClk)
    vga.ctrl.io.pixels << colorStream

    //Halt the vga read cmd stream if there is to mutch pending command or if the fifo is near than full
    io.vgaReadCmd << dma.io.sblReadCmd.haltWhen(pendingCmd === pendingCmd.maxValue || RegNext(colorStreamOccupancy) > fifoSize - 128)

  }
}


object MandelbrotSblDemo {
  def main(args: Array[String]) {
    for(i <- 0 until 1){
      SpinalVhdl({
        val vgaClock = ClockDomain.external("vga")
        val vgaMemoryClock = ClockDomain.external("vgaMemory")
        val coreClock = ClockDomain.external("core",frequency=FixedFrequency(100 MHz))
        new MandelbrotSblDemo(0, new MandelbrotCoreParameters(256, 6, 640, 480, 7, 17 * 3), coreClock, vgaMemoryClock, vgaClock)
      })
    }
  }
}
