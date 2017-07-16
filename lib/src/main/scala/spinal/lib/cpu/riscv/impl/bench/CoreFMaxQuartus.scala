package spinal.lib.cpu.riscv.impl.bench

import java.util.concurrent.ForkJoinPool

import spinal.core.SpinalVhdl
import spinal.lib.WrapWithReg
import spinal.lib.cpu.riscv.impl.build.RiscvAvalon
import spinal.lib.cpu.riscv.impl._
import spinal.lib.cpu.riscv.impl.extension.{BarrelShifterLightExtension, BarrelShifterFullExtension, DivExtension, MulExtension}
import spinal.lib.eda.altera.QuartusFlow
import spinal.lib.eda.bench.Report


import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}

//import scala.concurrent.ExecutionContext.Implicits.global
/**
 * Created by PIC32F_USER on 18/05/2016.
 */

object CoreFMaxQuartusBench {
  def main(args: Array[String]) {
    import scala.concurrent.ExecutionContext
    //import java.util.concurrent.Executors
    //implicit val ec = ExecutionContext.fromExecutorService(Executors.newWorkStealingPool(6))
    implicit val ec = ExecutionContext.fromExecutorService(
      new ForkJoinPool(6, ForkJoinPool.defaultForkJoinWorkerThreadFactory, null, true)
    )

    val quartus15Path = "D:/altera_lite/15.1/quartus/bin64"
    val quartus13Path = "D:/altera/13.0sp1/quartus/bin64"
    val tmpPath = "E:/tmp/"

    trait Core {
      def getName(): String
      def getTopLevelName(): String
    }

    trait Target {
      def doit(core: Core, workspace: String): Future[Report]
      def getFamily: String
    }


    val bigCore = new Core {
      override def getName(): String = "Big core"
      override def getTopLevelName(): String = "bigCore"

      SpinalVhdl({
        val debug = true
        val interruptCount = 4

        val iCacheConfig = InstructionCacheConfig(
          cacheSize = 4096,
          bytePerLine = 32,
          wayCount = 1,
          wrappedMemAccess = true,
          addressWidth = 32,
          cpuDataWidth = 32,
          memDataWidth = 32
        )

        val dCacheConfig = DataCacheConfig(
          cacheSize = 4096,
          bytePerLine = 32,
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
        //  coreConfig.add(new BarrelShifterLightExtension)
        new WrapWithReg.Wrapper(new RiscvAvalon(coreConfig, iCacheConfig, dCacheConfig, debug, interruptCount)).setDefinitionName(getTopLevelName())
      })
    }

    val bigNoCacheCore = new Core {
      override def getName(): String = "Big core without caches"
      override def getTopLevelName(): String = "bigNoCacheCore"

      SpinalVhdl({
        val debug = true
        val interruptCount = 4

        val iCacheConfig = null
        val dCacheConfig = null

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
        //  coreConfig.add(new BarrelShifterLightExtension)
        new WrapWithReg.Wrapper(new RiscvAvalon(coreConfig, iCacheConfig, dCacheConfig, debug, interruptCount)).setDefinitionName(getTopLevelName())
      })
    }

    val smallCore = new Core {
      override def getName(): String = "Small core"
      override def getTopLevelName(): String = "smallCore"

      SpinalVhdl({
        val debug = false
        val interruptCount = 0

        val iCacheConfig = null
        val dCacheConfig = null

        val coreConfig = RiscvCoreConfig(
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
          fastFetchCmdPcCalculation = false,
          dynamicBranchPredictorCacheSizeLog2 = 7
        )

        coreConfig.add(new BarrelShifterLightExtension)
        new WrapWithReg.Wrapper(new RiscvAvalon(coreConfig, iCacheConfig, dCacheConfig, debug, interruptCount)).setDefinitionName(getTopLevelName())
      })
    }

    val fastCore = new Core {
      override def getName(): String = "Fast core"

      override def getTopLevelName(): String = "fastCore"

      SpinalVhdl({
        val debug = false
        val interruptCount = 0

        val iCacheConfig = null
        val dCacheConfig = null

        val coreConfig = RiscvCoreConfig(
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
          fastFetchCmdPcCalculation = true,
          dynamicBranchPredictorCacheSizeLog2 = 7
        )

        coreConfig.add(new BarrelShifterLightExtension)
        new WrapWithReg.Wrapper(new RiscvAvalon(coreConfig, iCacheConfig, dCacheConfig, debug, interruptCount)).setDefinitionName(getTopLevelName())
      })
    }


    val cycloneV = new Target {
      override def getFamily: String = "Cyclone V"

      override def doit(core: Core, workspace: String): Future[Report] = Future {
        QuartusFlow(
          quartusPath = quartus15Path,
          workspacePath = workspace,
          toplevelPath = core.getTopLevelName() + ".vhd",
          family = getFamily,
          device = "5CSEMA5F31C6"
        )
      }
    }
    val cycloneIV = new Target {
      override def getFamily: String = "Cyclone IV"

      override def doit(core: Core, workspace: String): Future[Report] = Future {
        QuartusFlow(
          quartusPath = quartus15Path,
          workspacePath = workspace,
          toplevelPath = core.getTopLevelName() + ".vhd",
          family = getFamily,
          device = "EP4CE30F29C6"
        )
      }
    }
    val cycloneII = new Target {
      override def getFamily: String = "Cyclone II"

      override def doit(core: Core, workspace: String): Future[Report] = Future {
        QuartusFlow(
          quartusPath = quartus13Path,
          workspacePath = workspace,
          toplevelPath = core.getTopLevelName() + ".vhd",
          family = getFamily,
          device = "EP2C35F672C6"
        )
      }
    }
    val cores = Seq(bigCore, smallCore, fastCore,bigNoCacheCore)
    val targets = Seq(cycloneII,cycloneIV, cycloneV)

    val results = (for (core <- cores) yield {
      (core -> (for (target <- targets) yield {
        val ret = (target -> target.doit(core, tmpPath + core.getTopLevelName() + "_" + target.getFamily.replace(" ", "")))
        Thread.sleep(8000)
        ret
      }).toMap)
    }).toMap

    for (core <- cores) {
      for (target <- targets) {
        Await.ready(results(core)(target), Duration.Inf)
      }
    }

    for (core <- cores) {
      println(s"${core.getName()} ->")
      for (target <- targets) {
        val report = results(core)(target).value.get.get
        println(s"${target.getFamily} -> ${(report.getFMax() / 1e6).toInt} Mhz")
      }
    }
  }
}


//Big core ->
//Cyclone II -> 86 Mhz
//Cyclone IV -> 112 Mhz
//Cyclone V -> 126 Mhz
//Small core ->
//Cyclone II -> 143 Mhz
//Cyclone IV -> 169 Mhz
//Cyclone V -> 193 Mhz
//Fast core ->
//Cyclone II -> 150 Mhz
//Cyclone IV -> 171 Mhz
//Cyclone V -> 179 Mhz
//Big core without caches ->
//Cyclone II -> 102 Mhz
//Cyclone IV -> 117 Mhz
//Cyclone V -> 134 Mhz
