package spinal.lib.cpu.riscv.impl

import spinal.core._
import spinal.lib.WrapWithReg.Wrapper
import spinal.lib._
import spinal.lib.bus.avalon._
import spinal.lib.cpu.riscv.impl.CoreQSysAvalon.RiscvAvalon
import spinal.lib.tool.{ResetEmitterTag, InterruptReceiverTag, QSysify}


object CoreQSysAvalon{
  import extension._

  class RiscvAvalon extends Component{
    val iCached = false
    val dCached = false
    val debug = true

    val iCacheConfig = InstructionCacheConfig(
      cacheSize =4096,
      bytePerLine =32,
      wayCount = 1,
      wrappedMemAccess = true,
      addressWidth = 32,
      cpuDataWidth = 32,
      memDataWidth = 32)

    val dCacheConfig = DataCacheConfig(
      cacheSize = 4096,
      bytePerLine =32,
      wayCount = 1,
      addressWidth = 32,
      cpuDataWidth = 32,
      memDataWidth = 32
    )

    lazy val p = CoreConfig(
      pcWidth = 32,
      addrWidth = 32,
      startAddress = 0x200,
      regFileReadyKind = sync,
      branchPrediction = disable,
      bypassExecute0 = false,
      bypassExecute1 = false,
      bypassWriteBack = false,
      bypassWriteBackBuffer = false,
      collapseBubble = false,
      dataBusKind = cmdStream_rspFlow,
      fastFetchCmdPcCalculation = false,
      dynamicBranchPredictorCacheSizeLog2 = 7
    )


    val iConfig = if(iCached){
      iCacheConfig.getAvalonConfig()
    }else{
      CoreInstructionBus.getAvalonConfig(p)
    }

    val dConfig = if(dCached){
      dCacheConfig.getAvalonConfig()
    }else{
      CoreDataBus.getAvalonConfig(p)
    }

    val io = new Bundle{
      val i = master(AvalonMMBus(iConfig))
      val d = master(AvalonMMBus(dConfig))
      val interrupt = in(Bits(4 bit))
      val debugResetIn = if(debug) in Bool else null
      val debugResetOut = if(debug) out Bool else null
      val debugBus = if(debug) slave(AvalonMMBus(DebugExtension.getAvalonMMConfig)) else null
    }

    //p.add(new MulExtension)
   // p.add(new DivExtension)
   // p.add(new BarrelShifterFullExtension)
   // p.add(new SimpleInterruptExtension(exceptionVector=0x0).addIrq(id=4,pins=io.interrupt,IrqUsage(isException=false),name="io_interrupt"))
    p.add(new BarrelShifterLightExtension)
    val nativeInstructionBusExtension = if(!iCached)p.add(new NativeInstructionBusExtension)  else null
    val cachedInstructionBusExtension = if(iCached)p.add(new CachedInstructionBusExtension(iCacheConfig,false,true))  else null
    val nativeDataBusExtension = if(!dCached) p.add(new NativeDataBusExtension) else null
    val cachedDataBusExtension = if(dCached) p.add(new CachedDataBusExtension(dCacheConfig,true)) else null



    val debugExtension = if(debug) {
      val clockDomain = ClockDomain.current.clone(reset = io.debugResetIn)
      val extension = new DebugExtension(clockDomain)
      p.add(extension)
      extension
    } else null

    val core = new Core()(p)

    if(debug) {
      DebugExtension.avalonToDebugBus(io.debugBus,debugExtension.io.bus)
      io.debugResetOut := debugExtension.io.resetOut
    }

    if(iCached){
      val memCache = cachedInstructionBusExtension.memBus
      val memI = memCache.clone
      memI.cmd <-< memCache.cmd
      memI.rsp >-> memCache.rsp
      io.i <> memI.toAvalon()
    }else{
      val memCpu = nativeInstructionBusExtension.memBus
      val coreI = memCpu.clone
      coreI.cmd <-< memCpu.cmd
      coreI.rsp >-> memCpu.rsp
      io.i <> coreI.toAvalon()

      if(coreI.branchCachePort != null){
        coreI.branchCachePort <> memCpu.branchCachePort
      }
    }
    if(dCached){
      val memCache = cachedDataBusExtension.memBus
      val memD = memCache.clone
      memD.cmd <-/< memCache.cmd
      memD.rsp >-> memCache.rsp
      io.d <> memD.toAvalon()
    }else{
      val memCpu = nativeDataBusExtension.memBus
      val coreD = memCpu.clone
      coreD.cmd <-/< memCpu.cmd
      coreD.rsp >-> memCpu.rsp
      io.d <> coreD.toAvalon()
    }

  }

  def main(args: Array[String]) {
    //val report = SpinalVhdl(new RiscvAvalon(),_.setLibrary("lib_riscvAvalon"))
    val report = SpinalVhdl(new RiscvAvalon(),_.setLibrary("qsys").onlyStdLogicVectorTopLevelIo)
    //val report = SpinalVhdl(new RiscvAvalon())

    report.toplevel.io.i addTag(ClockDomainTag(report.toplevel.clockDomain))
    report.toplevel.io.d addTag(ClockDomainTag(report.toplevel.clockDomain))
    report.toplevel.io.interrupt addTag(InterruptReceiverTag(report.toplevel.io.i,report.toplevel.clockDomain))
    if(report.toplevel.debug) {
      report.toplevel.io.debugBus addTag(ClockDomainTag(report.toplevel.debugExtension.clockDomain))
      report.toplevel.io.debugResetOut.addTag(ResetEmitterTag(report.toplevel.debugExtension.clockDomain))
    }
    QSysify(report.toplevel)
  }
}


object CoreFMaxBench{
  def main(args: Array[String]) {
    SpinalVhdl(new WrapWithReg.Wrapper(new RiscvAvalon).setDefinitionName("TopLevel"))
  }
}

