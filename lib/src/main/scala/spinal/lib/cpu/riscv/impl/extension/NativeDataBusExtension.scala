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

