package spinal.lib.cpu.riscv.impl.extension

import spinal.core._
import spinal.lib.bus.avalon.{AvalonMMBus, AvalonMMConfig}
import spinal.lib.cpu.riscv.impl._
import spinal.lib._

trait AvalonProvider{
  def getConfig() : AvalonMMConfig
  def getAvalon() : AvalonMMBus
}

class NativeInstructionBusExtension extends CoreExtension with AvalonProvider{
  override def getName: String = "NativeInstructionBus"
  var memBus : CoreInstructionBus = null
  override def applyIt(core: Core): Area = new Area{
    memBus = master(CoreInstructionBus()(core.c)).setName("io_i")
    memBus.cmd << core.iCmd
    memBus.rsp >> core.iRsp
    if(memBus.branchCachePort != null) memBus.branchCachePort <> core.brancheCache.readSyncPort

//    core.iCacheFlush.cmd.ready := True
//    core.iCacheFlush.rsp := core.iCacheFlush.cmd.valid
  }

  override def getAvalon(): AvalonMMBus = memBus.toAvalon()
  override def getConfig(): AvalonMMConfig = CoreInstructionBus.getAvalonConfig(memBus.p)
}



class CachedInstructionBusExtension(c : InstructionCacheConfig,cutCpuCmdReady : Boolean = false,cutCpuRspReady : Boolean = false) extends CoreExtension with AvalonProvider{
  override def getName: String = "CachedInstructionBus"
  var memBus : InstructionCacheMemBus = null
  override def applyIt(core: Core): Area = new Area{
    val coreICmd = if(cutCpuCmdReady) core.iCmd.s2mPipe() else core.iCmd
    val coreIRsp = core.iRsp.clone
    if(cutCpuRspReady) coreIRsp >/> core.iRsp else coreIRsp >> core.iRsp

    val cache = new InstructionCache()(c)
    cache.io.cpu.cmd.valid := coreICmd.valid
    cache.io.cpu.cmd.address := coreICmd.pc
    coreICmd.ready := cache.io.cpu.cmd.ready

    coreIRsp.valid := cache.io.cpu.rsp.valid
    coreIRsp.pc := cache.io.cpu.rsp.address
    coreIRsp.instruction := cache.io.cpu.rsp.data
    cache.io.cpu.rsp.ready := coreIRsp.ready

    if(core.c.branchPrediction == dynamic){
      coreIRsp.branchCacheLine := core.brancheCache.readSync(cache.io.cpu.cmd.address(2,core.c.dynamicBranchPredictorCacheSizeLog2 bits),cache.io.cpu.cmd.fire)
    }

    memBus = master(InstructionCacheMemBus()(c)).setName("io_i")
    memBus <> cache.io.mem

    val iCacheflushEmitter = EventEmitter(on=cache.io.flush.cmd)

    when(core.execute1.inInst.valid && core.execute1.inInst.ctrl.fencei){
      //core.prefetch.halt := True
      when(core.execute1.inInst.ready) {
        //core.execute0.flush := True
        iCacheflushEmitter.emit()
      }
    }
  }

  override def getAvalon(): AvalonMMBus = memBus.toAvalon()
  override def getConfig(): AvalonMMConfig = c.getAvalonConfig()
}
