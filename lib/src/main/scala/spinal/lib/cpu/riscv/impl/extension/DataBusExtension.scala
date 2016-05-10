package spinal.lib.cpu.riscv.impl.extension


import spinal.core._
import spinal.lib.bus.avalon.{AvalonMMBus, AvalonMMConfig}
import spinal.lib.cpu.riscv.impl._
import spinal.lib._

class NativeDataBusExtension extends CoreExtension with AvalonProvider{
  override def getName: String = "NativeInstructionBus"
  var memBus : CoreDataBus = null
  override def applyIt(core: Core): Area = new Area{
    memBus = master(CoreDataBus()(core.c)).setName("io_d")
    memBus.cmd << core.dCmd
    memBus.rsp >> core.dRsp
  }

  override def getAvalon(): AvalonMMBus = memBus.toAvalon()
  override def getConfig(): AvalonMMConfig = CoreInstructionBus.getAvalonConfig(memBus.p)
}


class CachedDataBusExtension(c : DataCacheConfig,cutCpuCmdReady : Boolean = false,cutCpuRspReady : Boolean = false) extends CoreExtension with AvalonProvider{
  override def getName: String = "CachedDataBus"
  var memBus : DataCacheMemBus = null
  override def applyIt(core: Core): Area = new Area{
    val coreDCmd = if(cutCpuCmdReady) core.dCmd.s2mPipe() else core.dCmd
    val coreDRsp = core.dRsp.clone
    if(cutCpuRspReady) coreDRsp >/> core.dRsp else coreDRsp >> core.dRsp

    val cache = new DataCache()(c)
    cache.io.cpu.cmd.valid := coreDCmd.valid
    cache.io.cpu.cmd.wr := coreDCmd.wr
    cache.io.cpu.cmd.address := coreDCmd.address(coreDCmd.address.high downto 2) @@ U"00"
    cache.io.cpu.cmd.data := coreDCmd.size.map (
      U(0) -> coreDCmd.data(7 downto 0) ## coreDCmd.data(7 downto 0) ## coreDCmd.data(7 downto 0) ## coreDCmd.data(7 downto 0),
      U(1) -> coreDCmd.data(15 downto 0) ## coreDCmd.data(15 downto 0),
      default -> coreDCmd.data(31 downto 0)
    )
    cache.io.cpu.cmd.mask := (coreDCmd.size.map (
      U(0) -> B"0001",
      U(1) -> B"0011",
      default -> B"1111"
    ) << coreDCmd.address(1 downto 0)).resized
    cache.io.cpu.cmd.bypass := coreDCmd.address.msb
    cache.io.cpu.cmd.all := False
    cache.io.cpu.cmd.kind := DataCacheCpuCmdKind.MEMORY
    coreDCmd.ready := cache.io.cpu.cmd.ready

    coreDRsp.valid := cache.io.cpu.rsp.valid
    coreDRsp.payload := cache.io.cpu.rsp.data
    switch(core.writeBack.inInst.dCmdAddress(1 downto 0)){
      is(1){coreDRsp.payload(7 downto 0) := cache.io.cpu.rsp.data(15 downto 8)}
      is(2){coreDRsp.payload(15 downto 0) := cache.io.cpu.rsp.data(31 downto 16)}
      is(3){coreDRsp.payload(7 downto 0) := cache.io.cpu.rsp.data(31 downto 24)}
    }

    memBus = master(DataCacheMemBus()(c)).setName("io_d")
    memBus <> cache.io.mem
  }

  override def getAvalon(): AvalonMMBus = memBus.toAvalon()
  override def getConfig(): AvalonMMConfig = c.getAvalonConfig()
}