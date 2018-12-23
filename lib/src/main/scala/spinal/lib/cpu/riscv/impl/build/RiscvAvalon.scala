package spinal.lib.cpu.riscv.impl.build

import spinal.core._
import spinal.lib._
import spinal.lib.bus.avalon._
import spinal.lib.cpu.riscv.impl._
import spinal.lib.eda.altera.{InterruptReceiverTag, QSysify, ResetEmitterTag}

import cpu.riscv.impl.extension._

class RiscvAvalon(coreConfig : RiscvCoreConfig,iCacheConfig : InstructionCacheConfig, dCacheConfig : DataCacheConfig,debug : Boolean,interruptCount : Int) extends Component{
  val iCached = iCacheConfig != null
  val dCached = dCacheConfig != null

  val iConfig = if(iCached){
    iCacheConfig.getAvalonConfig()
  }else{
    CoreInstructionBus.getAvalonConfig(coreConfig)
  }

  val dConfig = if(dCached){
    dCacheConfig.getAvalonConfig()
  }else{
    CoreDataBus.getAvalonConfig(coreConfig)
  }

  val io = new Bundle{
    val i = master(AvalonMM(iConfig))
    val d = master(AvalonMM(dConfig))
    val interrupt = if(interruptCount != 0) in(Bits(4 bit)) else null
    val debugResetIn = if(debug) in Bool else null
    val debugResetOut = if(debug) out Bool else null
    val debugBus = if(debug) slave(AvalonMM(DebugExtension.getAvalonMMConfig)) else null
  }


  if(interruptCount != 0)
    coreConfig.add(new SimpleInterruptExtension(exceptionVector=0x0).addIrq(id=4,pins=io.interrupt,IrqUsage(isException=false),name="io_interrupt"))
  val nativeInstructionBusExtension = if(!iCached)coreConfig.add(new NativeInstructionBusExtension)  else null
  val cachedInstructionBusExtension = if(iCached)coreConfig.add(new CachedInstructionBusExtension(iCacheConfig,false,true))  else null
  val nativeDataBusExtension = if(!dCached) coreConfig.add(new NativeDataBusExtension) else null
  val cachedDataBusExtension = if(dCached) coreConfig.add(new CachedDataBusExtension(dCacheConfig,true)) else null



  val debugExtension = if(debug) {
    val clockDomain = ClockDomain.current.copy(reset = io.debugResetIn)
    val extension = new DebugExtension(clockDomain)
    coreConfig.add(extension)
    extension
  } else null

  val core = new RiscvCore()(coreConfig)

  if(debug) {
    DebugExtension.avalonToDebugBus(io.debugBus,debugExtension.io.bus)
    io.debugResetOut := debugExtension.io.resetOut
  }

  if(iCached){
    val memCache = cachedInstructionBusExtension.memBus
    val memI = cloneOf(memCache)
    memI.cmd << memCache.cmd.halfPipe()
    memI.rsp >> memCache.rsp
    io.i <> memI.toAvalon()
  }else{
    val memCpu = nativeInstructionBusExtension.memBus
    val coreI = cloneOf(memCpu)
    coreI.cmd <-< memCpu.cmd
    coreI.rsp >-> memCpu.rsp
    io.i <> coreI.toAvalon()

    if(coreI.branchCachePort != null){
      coreI.branchCachePort <> memCpu.branchCachePort
    }
  }
  if(dCached){
    val memCache = cachedDataBusExtension.memBus
    val memD = cloneOf(memCache)
    memD.cmd << memCache.cmd.halfPipe()
    memD.rsp >-> memCache.rsp
    io.d <> memD.toAvalon()
  }else{
    val memCpu = nativeDataBusExtension.memBus
    val coreD = cloneOf(memCpu)
    coreD.cmd <-/< memCpu.cmd
    coreD.rsp >-> memCpu.rsp
    io.d <> coreD.toAvalon(false)
  }

}

object RiscvAvalon{

  def main(args: Array[String]) {
    val debug = true
    val interruptCount = 4

    val report = SpinalConfig(onlyStdLogicVectorAtTopLevelIo=true).generateVerilog({

      //replace wit null to disable instruction cache
      val iCacheConfig = InstructionCacheConfig(
        cacheSize =4096,
        bytePerLine =32,
        wayCount = 1,
        wrappedMemAccess = true,
        addressWidth = 32,
        cpuDataWidth = 32,
        memDataWidth = 32
      )

      //replace wit null to disable data cache
      val dCacheConfig = DataCacheConfig(
        cacheSize = 4096,
        bytePerLine =32,
        wayCount = 1,
        addressWidth = 32,
        cpuDataWidth = 32,
        memDataWidth = 32
      )

      val coreConfig = RiscvCoreConfig(
        pcWidth = 32,
        addrWidth = 32,
        startAddress = 0x200,
        regFileReadyKind = sync,
        branchPrediction = dynamic,
        bypassExecute0 = true,
        bypassExecute1 = true,
        bypassWriteBack = true,
        bypassWriteBackBuffer = true,
        collapseBubble = false,
        fastFetchCmdPcCalculation = true,
        dynamicBranchPredictorCacheSizeLog2 = 7
      )

      coreConfig.add(new MulExtension)
      coreConfig.add(new DivExtension)
      coreConfig.add(new BarrelShifterFullExtension)
      //  p.add(new BarrelShifterLightExtension)


      new RiscvAvalon(coreConfig,iCacheConfig,dCacheConfig,debug,interruptCount)
    })

    report.toplevel.io.i addTag(ClockDomainTag(report.toplevel.clockDomain))
    report.toplevel.io.d addTag(ClockDomainTag(report.toplevel.clockDomain))
    if(interruptCount != 0)
      report.toplevel.io.interrupt addTag(InterruptReceiverTag(report.toplevel.io.i,report.toplevel.clockDomain))
    if(debug) {
      report.toplevel.io.debugBus addTag(ClockDomainTag(report.toplevel.debugExtension.clockDomain))
      report.toplevel.io.debugResetOut.addTag(ResetEmitterTag(report.toplevel.debugExtension.clockDomain))
    }
    QSysify(report.toplevel)
  }
}



