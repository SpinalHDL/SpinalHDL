package spinal.lib.graphic.vga

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba3.apb.{Apb3SlaveFactory, Apb3Config, Apb3}
import spinal.lib.bus.amba4.axi.{Axi4ReadOnly, Axi4Config}
import spinal.lib.graphic.{Rgb, VideoDmaGeneric, VideoDma, RgbConfig}


case class Axi4VgaCtrlGenerics(axiAddressWidth : Int,
                               axiDataWidth : Int,
                               burstLength : Int,
                               frameSizeMax : Int,
                               fifoSize : Int,
                               rgbConfig: RgbConfig,
                               timingsWidth: Int = 12,
                               pendingRequestMax : Int = 8,
                               vgaClock : ClockDomain = ClockDomain.current){
  def axi4Config = Axi4Config(
    addressWidth = axiAddressWidth,
    dataWidth = axiDataWidth,
    useId = false,
    useLock = false,
    useRegion = false,
    useCache = false,
    useProt = false,
    useQos = false
  )

  def apb3Config = Apb3Config(
    addressWidth = axiAddressWidth,
    dataWidth = axiDataWidth,
    useSlaveError = false
  )
}


//frameSizeMax in byte
case class Axi4VgaCtrl(g : Axi4VgaCtrlGenerics) extends Component{
  import g._
  require(isPow2(burstLength))

  val io = new Bundle{
    val axi = Axi4ReadOnly(axi4Config)
    val apb = Apb3(apb3Config)
    val vga = Vga(rgbConfig)
  }

  val apbCtrl = Apb3SlaveFactory(io.apb)
  val softReset = apbCtrl.createWriteOnly(Bool,0x00) init(True)

  val dmaGenerics = VideoDmaGeneric(
    addressWidth      = axiAddressWidth - log2Up(axiDataWidth/8) - log2Up(burstLength),
    dataWidth         = axiDataWidth,
    beatPerAccess     = burstLength,
    sizeWidth         = log2Up(frameSizeMax) -log2Up(axiDataWidth/8) - log2Up(burstLength),
    pendingRequetMax  = pendingRequestMax,
    fifoSize          = fifoSize,
    frameClock        = vgaClock,
    frameFragmentType = Rgb(rgbConfig)
  )

  val dma  = VideoDma(dmaGenerics)
  dma.io.mem.toAxi4ReadOnly <> io.axi
  apbCtrl.drive(dma.io.size, 0x04)
  apbCtrl.drive(dma.io.base, 0x08)
  
  val vga = new ClockingArea(vgaClock) {
    val ctrl = VgaCtrl(rgbConfig, timingsWidth)
    ctrl.feedWith(dma.io.frame)
    ctrl.io.softReset setWhen(BufferCC(softReset))

    ctrl.io.vga <> io.vga
  }


  vga.ctrl.io.timings.driveFrom(apbCtrl,0x40)
  vga.ctrl.io.timings.addTag(crossClockDomain)

  dma.io.start := PulseCCByToggle(vga.ctrl.io.frameStart,clockIn = vgaClock,clockOut = ClockDomain.current) && !softReset
}
