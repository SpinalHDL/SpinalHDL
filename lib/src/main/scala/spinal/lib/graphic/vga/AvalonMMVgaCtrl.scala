package spinal.lib.graphic.vga


import spinal.core._
import spinal.lib._
import spinal.lib.bus.avalon.{AvalonMM, AvalonMMConfig}
import spinal.lib.experimental.bus.neutral.NeutralStreamDma
import spinal.lib.eda.altera.QSysify
import spinal.lib.graphic._


class AvalonMMVgaCtrl(cDma : NeutralStreamDma.Config,cColor : RgbConfig) extends Component{
  val io = new Bundle{
    val mem = master (AvalonMM(cDma.getAvalonConfig))
    val vga = master(Vga(cColor))
  }

  val dma = new NeutralStreamDma.Block(cDma)


  dma.io.ctrl.cmd.valid := RegNext(True) init(False)
  dma.io.ctrl.cmd.startAt := 0x100000/4
  dma.io.ctrl.cmd.memCmdCount := 640*480/8/2
  dma.io.ctrl.cmd.burstLength := 8

  val dmaMem = cloneOf(dma.io.mem)
  dmaMem.cmd <-< dma.io.mem.cmd
  dmaMem.rsp >> dma.io.mem.rsp
  dmaMem.toAvalon <> io.mem

  val vga = new ClockingArea(if(cDma.ctrlRspClock != null) cDma.ctrlRspClock else ClockDomain.current) {
    val ctrl = new VgaCtrl(cColor)
    val pixelStream = Stream(Fragment(Rgb(cColor)))

    val high = RegInit(False)
    when(pixelStream.fire){
      high := ! high
    }
    when(dma.io.ctrl.rsp.fire && dma.io.ctrl.rsp.last){
      high := False
    }

    pixelStream.valid := dma.io.ctrl.rsp.valid
    pixelStream.last :=  dma.io.ctrl.rsp.last && high
    val halfSelect = Mux(high,dma.io.ctrl.rsp.fragment(31 downto 16),dma.io.ctrl.rsp.fragment(15 downto 0))
    pixelStream.fragment.r := halfSelect(15 downto 11).asUInt << 3
    pixelStream.fragment.g := halfSelect(10 downto 5).asUInt << 2
    pixelStream.fragment.b := halfSelect(4 downto 0).asUInt << 3
    dma.io.ctrl.rsp.ready := pixelStream.ready && high


    ctrl.io.timings.setAs_h640_v480_r60
    ctrl.feedWith(pixelStream)
    io.vga := RegNext(ctrl.io.vga)
  }
}


object AvalonMMVgaCtrl{
  def main(args: Array[String]) {
    def gen = {
      val vgaClk = ClockDomain.external("vga")
      val dmaConfig = NeutralStreamDma.Config(
        addressWidth = 30,
        dataWidth = 32,
        memCmdCountMax = 1<<24,
        burstLengthMax = 8,
        fifoSize = 512,
        pendingRequetMax = 4,
        ctrlRspClock = vgaClk
      )
      val colorConfig = RgbConfig(8,8,8)
      new AvalonMMVgaCtrl(dmaConfig,colorConfig)
    }
    val toplevel = SpinalVhdl(gen).toplevel

    toplevel.io.mem.addTag(ClockDomainTag(toplevel.clockDomain))
    QSysify(toplevel)


    SpinalVerilog(gen)

//    SpinalVhdl({
//      val vgaClk = ClockDomain.external("vga")
//      val dmaConfig = NeutralStreamDma.Config(
//        addressWidth = 30,
//        dataWidth = 32,
//        memCmdCountMax = 1<<24,
//        burstLengthMax = 8,
//        fifoSize = 512,
//        pendingRequetMax = 4,
//        ctrlRspClock = vgaClk
//      )
//      val colorConfig = RgbConfig(8,8,8)
//      new AvalonVgaCtrl(dmaConfig,colorConfig).setDefinitionName("TopLevel")
//    })
  }
}


object AvalonVgaCtrlCCTest{
  def main(args: Array[String]) {
    SpinalVhdl({
      val pushClock = ClockDomain.external("pushClock")
      val popClock = ClockDomain.external("popClock")
      new StreamFifoCC(Bits(33 bit),512,pushClock,popClock).setDefinitionName("TopLevel")
    })

    SpinalVerilog({
      val pushClock = ClockDomain.external("pushClock")
      val popClock = ClockDomain.external("popClock")
      new StreamFifoCC(Bits(33 bit),512,pushClock,popClock).setDefinitionName("TopLevel")
    })
  }
}