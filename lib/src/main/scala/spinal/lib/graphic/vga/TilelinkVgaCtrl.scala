//package spinal.lib.graphic.vga
//
//import spinal.core._
//import spinal.core.fiber._
//import spinal.lib.bus.amba3.apb.{Apb3, Apb3SlaveFactory}
//import spinal.lib.bus.amba4.axi.Axi4ReadOnly
//import spinal.lib.bus.tilelink
//import spinal.lib.bus.tilelink._
//import spinal.lib.bus.bsb.{Bsb, BsbInterconnectGenerator, BsbParameter}
//import spinal.lib.graphic._
//import spinal.lib.generator._
//import spinal.lib._
//import spinal.lib.bus.misc.SizeMapping
//import spinal.lib.bus.tilelink.SlaveFactory
//import spinal.lib.com.uart.TilelinkUartCtrl
//import spinal.lib.graphic.hdmi.VgaToHdmiEcp5
//import spinal.lib.misc.slot.SlotPool
//
//
//
//object TilelinkVgaCtrl{
//  def getSupported(proposed: M2sSupport) = SlaveFactory.getSupported(
//    addressWidth = addressWidth,
//    dataWidth = 32,
//    allowBurst = false,
//    proposed = proposed
//  )
//  def addressWidth = 8
//}
//
//case class TilelinkVgaCtrlParameter(dmaParam : TilelinkVideoDmaParam,
//                                    rgbConfig: RgbConfig,
//                                    timingsWidth: Int = 12){
//
//}
//
//case class TilelinkVideoDmaParam(frameSize : Int,
//                                 pendingSize : Int,
//                                 accessSize : Int,
//                                 storageSize : Int){
//  val frameSizeWidth = log2Up(frameSize)
//}
//
//case class TilelinkVideoDma(param : TilelinkVideoDmaParam,
//                            memParam: BusParameter,
//                            vgaCd : ClockDomain,
//                            ) extends Component {
//  import param._
//  val io = new Bundle{
//    val start = in Bool()
//    val busy = out Bool()
//
//    val base = in UInt (memParam.addressWidth bits) //in byte
//    val size = in UInt (memParam.addressWidth bits) //in byte
//
//    val mem = master(Bus(memParam))
//    val frame = master(Stream(Fragment(Bits(memParam.dataWidth bits))))
//  }
//
//
//
//  val storage = new Area{
//    val ram = Mem.fill(storageSize/memParam.dataBytes)(memParam.data)
//    val write = ram.writePort()
//    val read = vgaCd(ram.readSyncPort(clockCrossing = true))
//  }
//
//  val push = new Area {
//    val isActive = RegInit(False)
//    val cmdActive = RegInit(False)
//    io.busy := isActive
//
//    val ptr = Reg(UInt(memParam.addressWidth bits))
//    val memCmdDone = ptr === io.size
//
//    val TARGET = HardType(UInt(log2Up(storageSize / accessSize) bits))
//    val TARGET_PTR = HardType(UInt(log2Up(storageSize / accessSize) + 1 bits))
//    val slots = new SlotPool(pendingSize)(
//      new Area {
//        val target = Reg(TARGET)
//      }
//    )
//
//    val pushTarget = Reg(TARGET_PTR) init (0)
//    val popTarget = TARGET_PTR()
//    val hazard = pushTarget.msb
//
//    val memA = cloneOf(io.mem.a)
//    io.mem.a <-< memA
//
//    memA.valid := False
//    memA.opcode := Opcode.A.GET
//    memA.param := 0
//    memA.source := slots.allocate.id
//    memA.address := io.base + ptr
//    memA.size := log2Up(accessSize)
//    when(memA.fire) {
//      ptr := ptr + accessSize
//      slots.allocate(_.target := pushTarget)
//    }
//
//    io.mem.d.ready := True
//    storage.write.valid := io.mem.d.valid
//    storage.write.address := slots.slots.map(_.target).read(io.mem.d.source) @@ io.mem.d.beatCounter()
//    storage.write.data := io.mem.d.data
//
//    when(!isActive) {
//      when(io.start) {
//        isActive := True
//      }
//    } otherwise {
//      when(!memCmdDone) {
//        memA.valid := True
//      } elsewhen (???) {
//        isActive := False
//      }
//    }
//
//    when(io.mem.cmd.fire) {
//      memCmdCounter := memCmdCounter + 1
//    }
//  }
//
//}
//
//case class TilelinkVgaCtrl(param : TilelinkVgaCtrlParameter,
//                           ctrlParam: BusParameter,
//                           dmaParam: BusParameter,
//                           vgaCd : ClockDomain) extends Component{
//
//  val io = new Bundle{
//    val ctrl = slave(Bus(ctrlParam))
//    val dma = master(Bus(dmaParam))
//    val vga = master(Vga(param.rgbConfig))
//  }
//
//  val ctrl = new SlaveFactory(io.ctrl, false)
//
//  val regs = new Area {
//    val run = ctrl.createReadAndWrite(Bool(), 0x00) init (False)
//    val timings = ctrl.createWriteOnly(VgaTimings(param.timingsWidth), 0x40)
//  }
//
//  val dma = new TilelinkVideoDma(
//    param    = param.dmaParam,
//    memParam = dmaParam,
//    vgaCd    = vgaCd
//  )
//
//  val vga = new ClockingArea(vgaCd) {
//    val input = io.input.toStreamFragment(omitMask = true)
//    val resized = Stream(Fragment(Bits(16 bits)))
//    StreamFragmentWidthAdapter(input, resized)
//    val adapted = Stream(Fragment(Rgb(param.rgbConfig)))
//    adapted.arbitrationFrom(resized)
//    adapted.last := resized.last
//    adapted.r := U(resized.fragment(15-param.rgbConfig.rWidth+1, param.rgbConfig.rWidth bits))
//    adapted.g := U(resized.fragment(10-param.rgbConfig.gWidth+1, param.rgbConfig.gWidth bits))
//    adapted.b := U(resized.fragment( 4-param.rgbConfig.bWidth+1, param.rgbConfig.bWidth bits))
//
//    val run = BufferCC(TilelinkVgaCtrl.this.run)
//    val ctrl = VgaCtrl(param.rgbConfig, param.timingsWidth)
//    ctrl.feedWith(adapted, resync = run.rise)
//    io.input.ready setWhen(!run) //Flush
//    ctrl.io.softReset := !run
//
//    ctrl.io.vga <> io.vga
//    ctrl.io.timings := regs.timings
//    ctrl.io.timings.addTag(crossClockDomain)
//  }
//
//
//}
//
//case class TilelinkVgaCtrlGenerator(ctrlOffset : Handle[BigInt] = Unset)
//                              (implicit val interconnect: TilelinkInterconnectGenerator, val bsbInterconnect : BsbInterconnectGenerator, decoder : TilelinkImplicitPeripheralDecoder = null) extends Area{
//
//  val ctrl = Handle(logic.io.ctrl)
//  val input = Handle(logic.io.input)
//  val output = Handle(logic.io.vga)
//  val parameter = Handle[TilelinkVgaCtrlParameter]
//  val vgaCd = Handle[ClockDomain]
//
//  val logic : Handle[TilelinkVgaCtrl] = Handle(TilelinkVgaCtrl(
//    param              = parameter,
//    ctrlParam  = accessRequirements.toTilelinkParameter(),
//    inputParameter = BsbParameter(
//      byteCount = is.byteCount,
//      sourceWidth = is.sourceWidth,
//      sinkWidth = is.sinkWidth,
//      withMask = is.withMask
//    ),
//    vgaCd          = vgaCd
//  ))
//
//  val accessSource = Handle[TilelinkAccessCapabilities]
//  val accessRequirements = Handle[TilelinkAccessParameter]
//  interconnect.addSlave(
//    accessSource = accessSource,
//    accessCapabilities = accessSource.derivate(TilelinkVgaCtrl.getTilelinkCapabilities),
//    accessRequirements = accessRequirements,
//    bus = ctrl,
//    mapping = ctrlOffset.derivate(SizeMapping(_, 1 << TilelinkVgaCtrl.addressWidth))
//  )
//  if(decoder != null) interconnect.addConnection(decoder.bus, ctrl)
//  val is = vgaCd on bsbInterconnect.addSlave(input)
//  is.sinkWidth.load(0)
//
//  def withRegisterPhy(withColorEn : Boolean) = output.produce{
//    val reg = out(vgaCd.get(Reg(Vga(output.rgbConfig, false))))
//    reg.assignSomeByName(output)
//    when(!output.colorEn){
//      reg.color.clear()
//    }
//    reg
//  }
//
//  def withHdmiEcp5(hdmiCd : Handle[ClockDomain]) = output produce new Area{
//    val bridge = VgaToHdmiEcp5(vgaCd, hdmiCd)
//    bridge.io.vga << output
//    val gpdi_dp, gpdi_dn = out Bits(4 bits)
//    gpdi_dp := bridge.io.gpdi_dp
//    gpdi_dn := bridge.io.gpdi_dn
//  }
//}
