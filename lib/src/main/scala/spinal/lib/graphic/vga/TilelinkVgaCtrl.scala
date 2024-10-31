package spinal.lib.graphic.vga

import spinal.core.{Reg, _}
import spinal.core.fiber._
import spinal.lib.bus.amba3.apb.{Apb3, Apb3SlaveFactory}
import spinal.lib.bus.amba4.axi.Axi4ReadOnly
import spinal.lib.bus.tilelink
import spinal.lib.bus.tilelink._
import spinal.lib.bus.bsb.{Bsb, BsbInterconnectGenerator, BsbParameter}
import spinal.lib.graphic._
import spinal.lib.generator._
import spinal.lib._
import spinal.lib.bus.misc.SizeMapping
import spinal.lib.bus.tilelink.SlaveFactory
import spinal.lib.com.uart.TilelinkUartCtrl
import spinal.lib.graphic.hdmi.VgaToHdmiEcp5
import spinal.lib.misc.slot.{Slot, SlotPool}

import scala.collection.mutable.ArrayBuffer



object TilelinkVgaCtrl{
  def getSupported(proposed: M2sSupport) = SlaveFactory.getSupported(
    addressWidth = addressWidth,
    dataWidth = 32,
    allowBurst = false,
    proposed = proposed
  )
  def addressWidth = 8
}

case class TilelinkVgaCtrlInits(run : Boolean,
                                base : BigInt,
                                size : BigInt,
                                timings : VgaTimingsScala)

case class TilelinkVgaCtrlMapping(offset : Int, width : Int){
  def range = offset+width-1 downto offset
}

case class TilelinkVgaCtrlMode(pixelWidth : Int,
                               mapping: Seq[TilelinkVgaCtrlMapping]){
}

case class TilelinkVgaCtrlParam(dmaParam : TilelinkVideoDmaParam,
                                rgbConfig: RgbConfig,
                                mods : Seq[TilelinkVgaCtrlMode],
                                inits : TilelinkVgaCtrlInits = null,
                                timingsWidth: Int = 12){

}

case class TilelinkVideoDmaParam(var addressWidth : Int,
                                 var dataWidth : Int,
                                 var pendingSize : Int,
                                 var accessSize : Int,
                                 var storageSize : Int){
}

object TilelinkVideoDma {
  def getDmaM2s(param : TilelinkVideoDmaParam, name : Nameable) = M2sParameters(
    addressWidth = param.addressWidth,
    dataWidth = param.dataWidth,
    masters = List(
      M2sAgent(
        name = name,
        mapping = M2sSource(
          id = SizeMapping(0, param.pendingSize),
          emits = M2sTransfers(
            get = SizeRange(param.accessSize)
          )
        )
      )
    )
  )
}

case class TilelinkVideoDma(param : TilelinkVideoDmaParam,
                            memParam: BusParameter,
                            vgaCd : ClockDomain
                            ) extends Component {
  import param._
  val io = new Bundle{
    val start = in Bool()
    val busy = out Bool()

    val base = in UInt (memParam.addressWidth bits) //in byte
    val size = in UInt (memParam.addressWidth bits) //in byte

    val mem = master(Bus(memParam))
    val frame = master(Stream(Fragment(Bits(memParam.dataWidth bits))))
  }

  val ptrWidth = log2Up(storageSize / accessSize) + 1
  val TARGET = HardType(UInt(ptrWidth-1 bits))
  val PTR = HardType(UInt(ptrWidth bits))

  val popToPushGray = Bits(ptrWidth bits)
  val pushToPopGray = Bits(ptrWidth bits)

  case class Word() extends Bundle{
    val data = memParam.data()
    val last = Bool()
  }

  val storage = new Area{
    val ram = Mem.fill(storageSize/memParam.dataBytes)(Word())
    val write = ram.writePort()
    val readCmd = Stream(ram.addressType)
    val readRsp = vgaCd(ram.streamReadSync(readCmd, crossClock = true))
  }

  val push = new Area {
    val isActive = RegInit(False)
    io.busy := isActive

    val offset = Reg(UInt(memParam.addressWidth bits))
    val offsetNext = offset + accessSize
    val cmdLast = offsetNext === io.size

    val slots = new SlotPool(pendingSize, true)(
      new Slot {
        val target = Reg(TARGET)
        val last = Reg(Bool())
      }
    )
    def isFull(a: Bits, b: Bits) = a(ptrWidth - 1 downto ptrWidth - 2) === ~b(ptrWidth - 1 downto ptrWidth - 2) && a(ptrWidth - 3 downto 0) === b(ptrWidth - 3 downto 0)

    val memA = cloneOf(io.mem.a)
    io.mem.a <-< memA

    val cmdPtr = Reg(PTR) init (0)
    memA.valid := False
    memA.opcode := Opcode.A.GET
    memA.param := Param.Hint.NO_ALLOCATE_ON_MISS
    memA.source := slots.allocate.id
    memA.address := io.base + offset
    memA.size := log2Up(accessSize)
    when(memA.fire) {
      slots.allocate { s =>
        s.target := cmdPtr.resized
        s.last := cmdLast
      }
      cmdPtr := cmdPtr + 1
      offset := offsetNext
    }

    io.mem.d.ready := True
    val slotsReader = slots.slots.reader(io.mem.d.source)
    storage.write.valid := io.mem.d.valid
    storage.write.address := slotsReader(_.target) @@ io.mem.d.beatCounter()
    storage.write.data.data := io.mem.d.data
    storage.write.data.last := slotsReader(_.last) && io.mem.d.isLast()
    when(io.mem.d.fire && io.mem.d.isLast()){
      slots.free(io.mem.d.source)
    }

    val pushPtr = Reg(PTR) init (0)
    val pushPtrGray = RegNext(toGray(pushPtr.getAheadValue())) init (0)
    when(pushPtr =/= cmdPtr && slots.slots.map(s => s.valid && s.target === pushPtr.resized).norR) {
      pushPtr := pushPtr + 1
    }

    val popPtrGray = BufferCC(popToPushGray, B(0, ptrWidth bits), inputAttributes = List(crossClockMaxDelay(1, useTargetClock = false)))
    val storageFull = isFull(toGray(cmdPtr), popPtrGray)

    when(!isActive) {
      when(io.start) {
        offset := 0
        isActive := True
      }
    } otherwise {
      when (!storageFull && !slots.allocate.full) {
        memA.valid := True
        when(memA.fire && cmdLast){
          isActive := False
        }
      }
    }
  }

  val pop = new ClockingArea(vgaCd){
    def isEmpty(a: Bits, b: Bits) = a === b

    val readPtr = Reg(UInt(storage.ram.addressWidth+1 bits)) init(0)

    val popPtr = readPtr >> log2Up(accessSize/memParam.dataBytes)
    val popPtrGray = toGray(popPtr)
    val pushPtrGray = BufferCC(pushToPopGray, B(0, ptrWidth bit), inputAttributes = List(crossClockMaxDelay(1, useTargetClock = false)))
    val ptrToPush = RegNext(popPtrGray) init (0)
    val empty = isEmpty(popPtrGray, pushPtrGray)

    storage.readCmd.valid := !empty
    storage.readCmd.payload := readPtr.resized
    when(storage.readCmd.fire) {
      readPtr := readPtr + 1
    }

    io.frame.arbitrationFrom(storage.readRsp)
    io.frame.last := storage.readRsp.last
    io.frame.fragment := storage.readRsp.data
  }

  pushToPopGray := push.pushPtrGray
  popToPushGray := pop.ptrToPush
}

case class TilelinkVgaCtrl(param : TilelinkVgaCtrlParam,
                           ctrlParam: BusParameter,
                           dmaParam: BusParameter,
                           vgaCd : ClockDomain) extends Component{
  import param._

  val io = new Bundle{
//    val ctrl = slave(Bus(ctrlParam))
    val dma = master(Bus(dmaParam))
    val vga = master(Vga(rgbConfig))
  }

//  val ctrl = new SlaveFactory(io.ctrl, false)

//  val regs = new Area {
//    val run = RegInit(False)
//    val timings = Reg(VgaTimings(param.timingsWidth))
//
////    val run = ctrl.createReadAndWrite(Bool(), 0x00) init (False)
////    val timings = ctrl.createWriteOnly(VgaTimings(param.timingsWidth), 0x40)
//  }

  val run = Bool()
  run := Bool(inits.run)

  val dma = new TilelinkVideoDma(
    param    = param.dmaParam,
    memParam = dmaParam,
    vgaCd    = vgaCd
  )
  dma.io.mem <> io.dma
  dma.io.base := inits.base
  dma.io.size := inits.size
  dma.io.start := run

  val vga = new ClockingArea(vgaCd) {
    assert(mods.size == 1)
    val mod = mods.head
    assert(mod.mapping.size == 3)

    val run = BufferCC(TilelinkVgaCtrl.this.run)
    val resized = Stream(Fragment(Bits(mod.pixelWidth bits)))
    val adapter = StreamFragmentWidthAdapter(dma.io.frame, resized)
    val adapted = Stream(Fragment(Rgb(param.rgbConfig)))

    adapted.arbitrationFrom(resized)
    adapted.last := resized.last
    adapted.r := U(resized.fragment(mod.mapping(0).range))
    adapted.g := U(resized.fragment(mod.mapping(1).range))
    adapted.b := U(resized.fragment(mod.mapping(2).range))

    val ctrl = VgaCtrl(param.rgbConfig, param.timingsWidth)
    val feed = ctrl.feedWith(adapted.stage(), resync = run.rise)
    ctrl.io.softReset := !run

    ctrl.io.vga <> io.vga
    ctrl.io.timings.assign(inits.timings)
    ctrl.io.timings.addTag(crossClockDomain)
  }
}


case class TilelinkVgaCtrlSpec(name : String,
                               param : TilelinkVgaCtrlParam)

object TilelinkVgaCtrlSpec{
  def addOption(parser: scopt.OptionParser[Unit], vga: ArrayBuffer[TilelinkVgaCtrlSpec]): Unit = {
    import parser._
    opt[Map[String, String]]("video").unbounded().action { (v, c) =>
      vga += TilelinkVgaCtrlSpec(
        name = v("name"),
        param = TilelinkVgaCtrlParam(
          dmaParam = TilelinkVideoDmaParam(
            addressWidth = 32,
            dataWidth = 0,
            pendingSize = 4,
            accessSize = 64,
            storageSize = 4096
          ),
//          rgbConfig = RgbConfig(5, 6, 5),
          rgbConfig = RgbConfig(8, 8, 8),
          mods = List(
            TilelinkVgaCtrlMode(
              pixelWidth = 32,
              mapping = Seq(
                TilelinkVgaCtrlMapping(0,8),
                TilelinkVgaCtrlMapping(8,8),
                TilelinkVgaCtrlMapping(16,8)
              )
            )
          ),
          inits = new TilelinkVgaCtrlInits(
            run = true,
            base = 0x40c00000,
            size = 800 * 600 * 4,
            timings = VgaTimingsScala.h800_v600_r60
//            size = 640*480*4,
//            timings = VgaTimingsScala.h640_v480_r60
          ),
          timingsWidth = 12
        )
      )
    } text (s"")
  }
}

case class TilelinkVgaCtrlFiber(param : TilelinkVgaCtrlParam, vgaCd : ClockDomain) extends Area{
  val dma = tilelink.fabric.Node.down()

  val logic = Fiber build new Area{
    dma.m2s.forceParameters(TilelinkVideoDma.getDmaM2s(param.dmaParam, TilelinkVgaCtrlFiber.this))
    dma.s2m.unsupported()
    val ctrl = TilelinkVgaCtrl(
      param = param,
      ctrlParam = null,
      dmaParam = dma.bus.p,
      vgaCd = vgaCd
    )
    ctrl.io.dma <> dma.bus
  }

//  def withRegisterPhy(withColorEn : Boolean) = output.produce{
//    val reg = out(vgaCd.get(Reg(Vga(output.rgbConfig, false))))
//    reg.assignSomeByName(output)
//    when(!output.colorEn){
//      reg.color.clear()
//    }
//    reg
//  }
}


object ColorConversion {

  def rgbToYCbCr(r: Int, g: Int, b: Int): (Int, Int, Int) = {
    require(r >= 0 && r <= 255, "R must be in the range 0 to 255")
    require(g >= 0 && g <= 255, "G must be in the range 0 to 255")
    require(b >= 0 && b <= 255, "B must be in the range 0 to 255")

    val y  = (0.299 * r + 0.587 * g + 0.114 * b).floor.toInt
    val cb = (128 - 0.168736 * r - 0.331264 * g + 0.5 * b).floor.toInt
    val cr = (128 + 0.5 * r - 0.418688 * g - 0.081312 * b).floor.toInt

    println(s"$y $cb $cr")
    def sat(x : Int) = x.min(255).max(0)
    (sat(y), sat(cb), sat (cr))
  }

  def main(args: Array[String]): Unit = {
//    for(i <- 0 until 32){
//      println(f"mww 0x${0x40c00000 + i*640*5*4}%x 0x${1 << i}%x ${640*5}")
//    }

//    println(s"mww 0x40c00000 0xFFFFFFFF ${640/2*7*35}")
//    for (i <- 0 until 32) {
//      println(f"mww 0x${0x40c00000 + i * 640/2*4*7}%x 0x${0x00001 << i}%x ${640/2 * 5}")
//    }

    var line = 0
    def show(r : Int, g : Int, b : Int) {
      val (y, cb, cr) = rgbToYCbCr(r, g, b)
      println(f"mww 0x${0x40c00000+line*640/2}%x ${f"0x${cr}%02x${y}%02x${cb}%02x${y}%02x"} ${640/2*20}")
      line += 20
    }

    show(0, 0, 0)
    show(255, 0, 0)
    show(0, 255, 0)
    show(0, 0, 255)
    show(255, 255, 255)
    show(0x80, 0, 0x80)
  }
}