package spinal.lib.cpu.riscv.impl.build

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba3.apb.{Apb3Config, Apb3}
import spinal.lib.bus.amba4.axi.{Axi4ReadOnly, Axi4Shared}
import spinal.lib.bus.avalon._
import spinal.lib.cpu.riscv.impl._
import spinal.lib.eda.altera.{InterruptReceiverTag, QSysify, ResetEmitterTag}

import cpu.riscv.impl.extension._


class RiscvAxi4(coreConfig : RiscvCoreConfig,iCacheConfig : InstructionCacheConfig, dCacheConfig : DataCacheConfig,debug : Boolean,interruptCount : Int) extends Component{
  val iCached = iCacheConfig != null
  val dCached = dCacheConfig != null

  val iConfig = if(iCached){
    iCacheConfig.getAxi4ReadOnlyConfig()
  }else{
    CoreInstructionBus.getAxi4Config(coreConfig)
  }

  val dConfig = if(dCached){
    dCacheConfig.getAxi4SharedConfig()
  }else{
    CoreDataBus.getAxi4Config(coreConfig)
  }

  val io = new Bundle{
    val i = master(Axi4ReadOnly(iConfig))
    val d = master(Axi4Shared(dConfig))
    val interrupt = if(interruptCount != 0) in(Bits(4 bit)) else null
    val debugResetIn = if(debug) in Bool else null
    val debugResetOut = if(debug) out Bool else null
    val debugBus = if(debug) slave(Apb3(RiscvAxi4.getDebugApbConfig())) else null
  }


  if(interruptCount != 0)
    coreConfig.add(new SimpleInterruptExtension(exceptionVector=0x20).addIrq(id=4,pins=io.interrupt,IrqUsage(isException=false),name="io_interrupt"))
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
    DebugExtension.Apb3ToDebugBus(io.debugBus,debugExtension.io.bus)
    io.debugResetOut := debugExtension.io.resetOut
  }

  if(iCached){
    val memCache = cachedInstructionBusExtension.memBus
    val memI = cloneOf(memCache)
    memI.cmd << memCache.cmd.halfPipe()
    memI.rsp >> memCache.rsp
    io.i <> memI.toAxi4ReadOnly()
  }else{
    val memCpu = nativeInstructionBusExtension.memBus
    val coreI = cloneOf(memCpu)
    coreI.cmd << memCpu.cmd
    coreI.rsp >> memCpu.rsp
    io.i <> coreI.toAxi4ReadOnly()

    if(coreI.branchCachePort != null){
      coreI.branchCachePort <> memCpu.branchCachePort
    }
  }
  if(dCached){
    val memCache = cachedDataBusExtension.memBus
    val memD = cloneOf(memCache)
    memD.cmd << memCache.cmd.halfPipe()
    memD.rsp >-> memCache.rsp
    io.d <> memD.toAxi4Shared()
  }else{
    val memCpu = nativeDataBusExtension.memBus
    val coreD = cloneOf(memCpu)
    coreD.cmd </< memCpu.cmd
    coreD.rsp >> memCpu.rsp
    io.d <> coreD.toAxi4Shared()
  }
}

object RiscvAxi4{
  def getDebugApbConfig() = Apb3Config(
    addressWidth = 12,
    dataWidth = 32
  )

  def main(args: Array[String]) {
    val debug = true
    val interruptCount = 4

    val report = SpinalConfig(onlyStdLogicVectorAtTopLevelIo=true).generateVerilog({
      val apb3Config = Apb3Config(16,32)
      //replace wit null to disable instruction cache
      val iCacheConfig = InstructionCacheConfig(
         cacheSize =4096,
         bytePerLine =32,
         wayCount = 1,  //Can only be one for the moment
         wrappedMemAccess = true,
         addressWidth = 32,
         cpuDataWidth = 32,
         memDataWidth = 32
       )

      //replace wit null to disable data cache
      val dCacheConfig = null
      //         DataCacheConfig(
      //         cacheSize = 4096,
      //         bytePerLine =32,
      //         wayCount = 1,
      //         addressWidth = 32,
      //         cpuDataWidth = 32,
      //         memDataWidth = 32
      //       )
      //
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


      new RiscvAxi4(coreConfig,iCacheConfig,dCacheConfig,debug,interruptCount)
    })
  }
}



