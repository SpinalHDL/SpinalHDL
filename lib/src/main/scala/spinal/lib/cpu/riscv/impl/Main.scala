package spinal.lib.cpu.riscv.impl

import spinal.core._
import spinal.lib._
import spinal.lib.bus.avalon.mm._
import spinal.lib.tool.{InterruptReceiverTag, QSysify}

object CoreMain{
  import extension._

  class TopLevel extends Component{
    val io_interrupt = in Bool
    val cached = true
    val cacheParam = InstructionCacheParameters(  cacheSize =4096,
      bytePerLine =32,
      wayCount = 1,
      wrappedMemAccess = true,
      addressWidth = 32,
      cpuDataWidth = 32,
      memDataWidth = 32)

    implicit val p = CoreParm(
      pcWidth = 32,
      addrWidth = 32,
      startAddress = 0x200,
      regFileReadyKind = sync,
      branchPrediction = dynamic,
      bypassExecute0 = true,
      bypassExecute1 = true,
      bypassWriteBack0 = true,
      bypassWriteBack1 = true,
      collapseBubble = true,
      instructionBusKind = cmdStream_rspFlow_oneCycle,
      dataBusKind = cmdStream_rspFlow,
      fastFetchCmdPcCalculation = true,
      dynamicBranchPredictorCacheSizeLog2 = 7
    )

    if(cached) assert(p.instructionBusKind == cmdStream_rspFlow_oneCycle)
    p.add(new MulExtension)
    p.add(new DivExtension)
    p.add(new BarrelShifterFullExtension)
    p.add(new SimpleInterruptExtension(exceptionVector=0x0).addIrq(id=4,pin=io_interrupt,IrqUsage(isException=false),name="io_interrupt"))
    //      p.add(new BarrelShifterLightExtension)
    val io = new Bundle{
      val i = master(CoreInstructionBus())
      val d = master(CoreDataBus())
      val iCmdDrive = in Bool
      val iRspDrive = in Bool
      val dCmdDrive = in Bool
      val dRspDrive = in Bool
    }
    def StreamDelay[T <: Data](that : Stream[T]) = that.s2mPipe().s2mPipe().s2mPipe().s2mPipe().s2mPipe().s2mPipe()
    val core = new Core


    val cache = new InstructionCache()(cacheParam)

    val iLogic = if(cached) new Area{
      val i = CoreInstructionBus()
      io.i.cmd << StreamDelay(i.cmd.continueWhen(io.iCmdDrive))
      i.rsp << StreamDelay(io.i.rsp).continueWhen(io.iRspDrive)

      core.io.i.cmd.ready := True
      i.rsp.ready := True
      cache.io.cpu.cmd.valid := core.io.i.cmd.valid
      cache.io.cpu.cmd.address := core.io.i.cmd.pc
      core.io.i.rsp.valid := cache.io.cpu.rsp.valid
      core.io.i.rsp.instruction := cache.io.cpu.rsp.data

      val busy = RegInit(False)
      val burstCounter = Reg(UInt(log2Up(cacheParam.bytePerLine/4) bit))
      val wrapCounter = Reg(UInt(log2Up(cacheParam.bytePerLine/4) bit))
      val address = Reg(cache.io.mem.cmd.address)

      cache.io.mem.cmd.ready := False
      i.cmd.valid := False
      i.cmd.pc := address + (wrapCounter << 2)
      when(!busy){
        when(cache.io.mem.cmd.valid && io.iCmdDrive){
          busy := True
          cache.io.mem.cmd.ready := True
          address := cache.io.mem.cmd.address(31 downto 5) @@ U"00000"
          burstCounter := 0
          wrapCounter := cache.io.mem.cmd.address(4 downto 2)
        }
      }otherwise{
        i.cmd.valid := True
        when(i.cmd.ready){
          burstCounter := burstCounter + 1
          wrapCounter := wrapCounter + 1
          when(burstCounter === burstCounter.maxValue){
            busy := False
          }
        }
      }
      cache.io.mem.rsp.valid <> i.rsp.valid
      cache.io.mem.rsp.data <> i.rsp.instruction
    }else {

      p.instructionBusKind match {
        case `cmdStream_rspFlow_oneCycle` =>
          io.i.cmd << core.io.i.cmd
          core.io.i.rsp << io.i.rsp
        case _ =>
          io.i.cmd << StreamDelay(core.io.i.cmd.continueWhen(io.iCmdDrive))
          core.io.i.rsp << StreamDelay(io.i.rsp).continueWhen(io.iRspDrive)
      }
    }
    io.d.cmd << StreamDelay(core.io.d.cmd.continueWhen(io.dCmdDrive))
    core.io.d.rsp << StreamDelay(io.d.rsp.m2sPipe()).continueWhen(io.dRspDrive)

  }

  def main(args: Array[String]) {
    SpinalVhdl({

      new TopLevel().setDefinitionName("CoreWrapper")
    }
    ,_.setLibrary("riscv"))
  }
}




object QSysAvalonCore{
  import extension._

  class RiscvAvalon extends Component{
    val cached = true
    val cacheParam = InstructionCacheParameters(  cacheSize =4096,
      bytePerLine =32,
      wayCount = 1,
      wrappedMemAccess = true,
      addressWidth = 32,
      cpuDataWidth = 32,
      memDataWidth = 32)

    lazy val p = CoreParm(
      pcWidth = 32,
      addrWidth = 32,
      startAddress = 0x200,
      regFileReadyKind = sync,
      branchPrediction = dynamic,
      bypassExecute0 = true,
      bypassExecute1 = true,
      bypassWriteBack0 = true,
      bypassWriteBack1 = true,
      collapseBubble = true,
      instructionBusKind = cmdStream_rspFlow_oneCycle,
      dataBusKind = cmdStream_rspFlow,
      fastFetchCmdPcCalculation = true,
      dynamicBranchPredictorCacheSizeLog2 = 7
    )

    if(cached) assert(p.instructionBusKind == cmdStream_rspFlow_oneCycle)
    val iConfig = if(cached){
      cacheParam.getAvalonConfig()
    }else{
      CoreInstructionBus.getAvalonConfig(p)
    }
    val io = new Bundle{
      val i = master(AvalonMMBus(iConfig))
      val d = master(AvalonMMBus(CoreDataBus.getAvalonConfig(p)))
      val interrupt = in(Bool)
    }

    p.add(new MulExtension)
    p.add(new DivExtension)
    p.add(new BarrelShifterFullExtension)
    p.add(new SimpleInterruptExtension(exceptionVector=0x0).addIrq(id=4,pin=io.interrupt,IrqUsage(isException=false),name="io_interrupt"))
    //      p.add(new BarrelShifterLightExtension)
    val core = new Core()(p)

    val cache = new InstructionCache()(cacheParam)
    if(cached){
      core.io.i.cmd.ready := True
      cache.io.cpu.cmd.valid := core.io.i.cmd.valid
      cache.io.cpu.cmd.address := core.io.i.cmd.pc
      core.io.i.rsp.valid := cache.io.cpu.rsp.valid
      core.io.i.rsp.instruction := cache.io.cpu.rsp.data
      io.i <> cache.io.mem.toAvalon()
    }else{
      io.i <>core.io.i.toAvalon()
    }

    io.d <> core.io.d.toAvalon()

  }

  def main(args: Array[String]) {
    //val report = SpinalVhdl(new RiscvAvalon(),_.setLibrary("lib_riscvAvalon"))
    val report = SpinalVhdl(new RiscvAvalon(),_.setLibrary("qsys").onlyStdLogicVectorTopLevelIo)
    //val report = SpinalVhdl(new RiscvAvalon())

    report.topLevel.io.i addTag(ClockDomainTag(report.topLevel.clockDomain))
    report.topLevel.io.d addTag(ClockDomainTag(report.topLevel.clockDomain))
    report.topLevel.io.interrupt addTag(InterruptReceiverTag(report.topLevel.io.i,report.topLevel.clockDomain))
    QSysify(report.topLevel)
  }
}