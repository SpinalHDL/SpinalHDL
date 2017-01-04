package spinal.lib.cpu.riscv.impl.bench


import spinal.core._
import spinal.lib._
import spinal.lib.cpu.riscv.impl.build.RiscvAvalon
import spinal.lib.cpu.riscv.impl._
import spinal.lib.cpu.riscv.impl.extension._


//Only used to check FMAX
object CoreFMaxBench{
  def main(args: Array[String]) {
    val debug = true
    val interruptCount = 4
    SpinalVhdl({

      val iCacheConfig = InstructionCacheConfig(
        cacheSize =4096,
        bytePerLine =32,
        wayCount = 1,
        wrappedMemAccess = true,
        addressWidth = 32,
        cpuDataWidth = 32,
        memDataWidth = 32
      )

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


      new WrapWithReg.Wrapper(new RiscvAvalon(coreConfig,iCacheConfig,dCacheConfig,debug,interruptCount)).setDefinitionName("TopLevel")
    })
  }
}
