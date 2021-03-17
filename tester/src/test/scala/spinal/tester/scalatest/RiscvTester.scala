package spinal.tester.scalatest

import org.scalatest.FunSuite
import spinal.core._
import spinal.lib.cpu.riscv.impl._
import spinal.lib.cpu.riscv.impl.bench.CoreUut
import spinal.lib.cpu.riscv.impl.bench.CoreUut.TopLevel
import spinal.lib.cpu.riscv.impl.extension.{BarrelShifterFullExtension, BarrelShifterLightExtension, DivExtension, MulExtension}

import language.postfixOps


//class RiscvCompilationTester extends FunSuite {
//  test("riscv_cached"){
//    val iCacheConfig = InstructionCacheConfig(
//      cacheSize = 4096*2,
//      bytePerLine =32,
//      wayCount = 1,
//      wrappedMemAccess = true,
//      addressWidth = 32,
//      cpuDataWidth = 32,
//      memDataWidth = 32
//    )
//
//    val dCacheConfig = DataCacheConfig(
//      cacheSize = 4096*2,
//      bytePerLine =32,
//      wayCount = 1,
//      addressWidth = 32,
//      cpuDataWidth = 32,
//      memDataWidth = 32
//    )
//
//    implicit val p = RiscvCoreConfig(
//      pcWidth = 32,
//      addrWidth = 32,
//      startAddress = 0x200,
//      regFileReadyKind = sync,
//      branchPrediction = dynamic,
//      bypassExecute0 = true,
//      bypassExecute1 = true,
//      bypassWriteBack = true,
//      bypassWriteBackBuffer = true,
//      collapseBubble = true,
//      fastFetchCmdPcCalculation = true,
//      dynamicBranchPredictorCacheSizeLog2 = 16,
//      branchPredictorHistoryWidth = 2
//    )
//
//    p.add(new MulExtension)
//    p.add(new DivExtension)
//    p.add(new BarrelShifterFullExtension)
//    //     p.add(new BarrelShifterLightExtension)
//
//    SpinalVerilog(new TopLevel(p,iCacheConfig,dCacheConfig,true,4))
//    SpinalVhdl(new TopLevel(p,iCacheConfig,dCacheConfig,true,4))
//  }
//
//  test("riscv_uncached"){
//    val iCacheConfig = null
//    val dCacheConfig = null
//
//    implicit val p = RiscvCoreConfig(
//      pcWidth = 32,
//      addrWidth = 32,
//      startAddress = 0x200,
//      regFileReadyKind = sync,
//      branchPrediction = disable,
//      bypassExecute0 = false,
//      bypassExecute1 = false,
//      bypassWriteBack = false,
//      bypassWriteBackBuffer = false,
//      collapseBubble = false,
//      fastFetchCmdPcCalculation = false,
//      dynamicBranchPredictorCacheSizeLog2 = 16,
//      branchPredictorHistoryWidth = 2
//    )
//
//    p.add(new MulExtension)
//    p.add(new DivExtension)
//    //p.add(new BarrelShifterFullExtension)
//    p.add(new BarrelShifterLightExtension)
//
//    SpinalVerilog(new TopLevel(p,iCacheConfig,dCacheConfig,true,4).setDefinitionName("RiscvTesterUncached"))
//    SpinalVhdl(new TopLevel(p,iCacheConfig,dCacheConfig,true,4).setDefinitionName("RiscvTesterUncached"))
//  }
//}


//
//class RiscvTesterCachedCocotbBoot extends SpinalTesterCocotbBase {
//  override def getName: String = "RiscvTesterCached"
//  override def pythonTestLocation: String = "tester/src/test/python/spinal/RiscvTester/cached"
//  override def createToplevel: Component = {
//
//    val iCacheConfig = InstructionCacheConfig(
//      cacheSize = 4096*2,
//      bytePerLine =32,
//      wayCount = 1,
//      wrappedMemAccess = true,
//      addressWidth = 32,
//      cpuDataWidth = 32,
//      memDataWidth = 32
//    )
//
//    val dCacheConfig = DataCacheConfig(
//      cacheSize = 4096*2,
//      bytePerLine =32,
//      wayCount = 1,
//      addressWidth = 32,
//      cpuDataWidth = 32,
//      memDataWidth = 32
//    )
//
//    implicit val p = RiscvCoreConfig(
//      pcWidth = 32,
//      addrWidth = 32,
//      startAddress = 0x200,
//      regFileReadyKind = sync,
//      branchPrediction = dynamic,
//      bypassExecute0 = true,
//      bypassExecute1 = true,
//      bypassWriteBack = true,
//      bypassWriteBackBuffer = true,
//      collapseBubble = true,
//      fastFetchCmdPcCalculation = true,
//      dynamicBranchPredictorCacheSizeLog2 = 16,
//      branchPredictorHistoryWidth = 2
//    )
//
//    p.add(new MulExtension)
//    p.add(new DivExtension)
//    p.add(new BarrelShifterFullExtension)
//    //     p.add(new BarrelShifterLightExtension)
//
//    new CoreUut.TopLevel(p,iCacheConfig,dCacheConfig,true,4).setDefinitionName("RiscvTesterCached")
//  }
//
//  override def backendConfig(config: SpinalConfig) = config.mode match {
//    case `Verilog` => config.copy(mergeAsyncProcess = false) // avoid iverilog bug
//    case _ => config
//  }
//}
//
//
//class RiscvTesterUncachedCocotbBoot extends SpinalTesterCocotbBase {
//  override def getName: String = "RiscvTesterUncached"
//  override def pythonTestLocation: String = "tester/src/test/python/spinal/RiscvTester/uncached"
//  override def createToplevel: Component = {
//
//    val iCacheConfig = null
//    val dCacheConfig = null
//
//    implicit val p = RiscvCoreConfig(
//      pcWidth = 32,
//      addrWidth = 32,
//      startAddress = 0x200,
//      regFileReadyKind = sync,
//      branchPrediction = disable,
//      bypassExecute0 = false,
//      bypassExecute1 = false,
//      bypassWriteBack = false,
//      bypassWriteBackBuffer = false,
//      collapseBubble = false,
//      fastFetchCmdPcCalculation = false,
//      dynamicBranchPredictorCacheSizeLog2 = 16,
//      branchPredictorHistoryWidth = 2
//    )
//
//    p.add(new MulExtension)
//    p.add(new DivExtension)
//    //p.add(new BarrelShifterFullExtension)
//    p.add(new BarrelShifterLightExtension)
//
//    new CoreUut.TopLevel(p,iCacheConfig,dCacheConfig,true,4).setDefinitionName("RiscvTesterUncached")
//  }
//}