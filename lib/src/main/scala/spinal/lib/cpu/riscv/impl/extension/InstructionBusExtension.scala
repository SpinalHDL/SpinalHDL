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
    memBus.cmd << core.prefetch.iCmd
    memBus.rsp >> core.fetch.iRsp
    if(memBus.branchCachePort != null) memBus.branchCachePort <> core.brancheCache.readSyncPort

    core.prefetch.iCacheFlush.cmd.ready := True
    core.prefetch.iCacheFlush.rsp := core.prefetch.iCacheFlush.cmd.valid
  }

  override def getAvalon(): AvalonMMBus = memBus.toAvalon()
  override def getConfig(): AvalonMMConfig = CoreInstructionBus.getAvalonConfig(memBus.p)
}



class CachedInstructionBusExtension(c : InstructionCacheConfig) extends CoreExtension with AvalonProvider{
  override def getName: String = "CachedInstructionBus"
  var memBus : InstructionCacheMemBus = null
  override def applyIt(core: Core): Area = new Area{
    val cache = new InstructionCache()(c)
    cache.io.cpu.cmd.valid := core.prefetch.iCmd.valid
    cache.io.cpu.cmd.address := core.prefetch.iCmd.pc
    core.prefetch.iCmd.ready := cache.io.cpu.cmd.ready

    core.fetch.iRsp.valid := cache.io.cpu.rsp.valid
    core.fetch.iRsp.pc := cache.io.cpu.rsp.address
    core.fetch.iRsp.instruction := cache.io.cpu.rsp.data
    cache.io.cpu.rsp.ready := core.fetch.iRsp.ready

    if(core.c.branchPrediction == dynamic){
      core.fetch.iRsp.branchCacheLine := core.brancheCache.readSync(cache.io.cpu.cmd.address(2,core.c.dynamicBranchPredictorCacheSizeLog2 bits),cache.io.cpu.cmd.fire)
    }

    memBus = master(InstructionCacheMemBus()(c)).setName("io_i")
    memBus <> cache.io.mem

    cache.io.flush <> core.prefetch.iCacheFlush
  }

  override def getAvalon(): AvalonMMBus = ??? //iBus.toAvalon()
  override def getConfig(): AvalonMMConfig = ??? //CoreInstructionBus.getAvalonConfig(iBus.p)
}
