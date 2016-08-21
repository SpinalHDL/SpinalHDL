package spinal.lib.soc.pinsec

import java.io.File
import java.nio.file.Files

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba3.apb._
import spinal.lib.bus.amba4.axi._
import spinal.lib.com.jtag.Jtag
import spinal.lib.cpu.riscv.impl.build.RiscvAxi4
import spinal.lib.cpu.riscv.impl.extension.{BarrelShifterFullExtension, DivExtension, MulExtension}
import spinal.lib.cpu.riscv.impl.{disable, dynamic, sync, CoreConfig}
import spinal.lib.io.TriStateArray
import spinal.lib.system.debugger.{JtagAxi4SharedDebugger, SystemDebuggerConfig}

class Pinsec extends Component{
  val debug = true
  val interruptCount = 4
//  val ahbConfig = Axi4Config(addressWidth = 32,dataWidth = 32)
//  val apbConfig = Apb3Config(addressWidth = 16,dataWidth = 32)

  val io = new Bundle{
//    val ahbAccess = slave(Axi4(ahbConfig))
    val asyncReset = in Bool
    val axiClk = in Bool
    val jtag_tck = in Bool
    val jtag = slave(Jtag())
    val gpioA = master(TriStateArray(32 bits))
    val gpioB = master(TriStateArray(32 bits))
    val interrupt = in Bits(interruptCount bits)
  }

  val resetCtrl = new ClockingArea(ClockDomain(io.axiClk,config = ClockDomainConfig(resetKind = BOOT))) {
    val doReset = BufferCC(io.asyncReset)
    val axiResetCounter = Reg(UInt(4 bits)) init(0)
    when(axiResetCounter =/= "1111"){
      axiResetCounter := axiResetCounter + 1
    }
    when(doReset) {
      axiResetCounter := 0
    }
    val axiReset = RegNext(axiResetCounter =/= "1111")
  }

  val axi = new ClockingArea(ClockDomain(io.axiClk,resetCtrl.axiReset)) {

    //replace wit null to disable instruction cache
    val iCacheConfig = null
    //         InstructionCacheConfig(
    //         cacheSize =4096,
    //         bytePerLine =32,
    //         wayCount = 1,
    //         wrappedMemAccess = true,
    //         addressWidth = 32,
    //         cpuDataWidth = 32,
    //         memDataWidth = 32
    //       )

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
    val coreConfig = CoreConfig(
      pcWidth = 32,
      addrWidth = 32,
      startAddress = 0x200,
      regFileReadyKind = sync,
      branchPrediction = disable,
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


    val core = new RiscvAxi4(coreConfig, iCacheConfig, dCacheConfig, debug, interruptCount)
    val ram, rom = Axi4SharedOnChipRam(
      dataWidth = 32,
      byteCount = 16 KB,
      idWidth = 4
    )

    val jtagCtrl = JtagAxi4SharedDebugger(SystemDebuggerConfig(
      memAddressWidth = 32,
      memDataWidth = 32,
      remoteCmdWidth = 1,
      jtagClockDomain = ClockDomain(io.jtag_tck)
    ))


    val apbBridge = Axi4SharedToApb3Bridge(
      addressWidth = 20,
      dataWidth = 32,
      idWidth = 4
    )

    val gpioACtrl = Apb3Gpio(32)
    val gpioBCtrl = Apb3Gpio(32)

    val ahbCrossbar = Axi4CrossbarFactory()
      .addSlaves(
        rom.io.axi ->(0x00000000L, 512 KB),
        ram.io.axi ->(0x04000000L, 512 KB),
        apbBridge.io.axi ->(0xF0000000L, 1 MB)
      ).addConnections(
        core.io.i
          -> List(rom.io.axi, ram.io.axi),
        core.io.d
          -> List(rom.io.axi, ram.io.axi, apbBridge.io.axi),
        jtagCtrl.io.axi
          -> List(rom.io.axi, ram.io.axi, apbBridge.io.axi)
      ).build()


    val apbDecoder = Apb3Crossbar(
      master = apbBridge.io.apb,
      slaves = List(
        gpioACtrl.io.apb ->(0x00000, 4 KB),
        gpioBCtrl.io.apb ->(0x01000, 4 KB),
        core.io.debugBus ->(0xF0000, 4 KB)
      )
    )

    if (interruptCount != 0) core.io.interrupt := io.interrupt
    if (debug) {
      core.io.debugResetIn := resetCtrl.axiReset
      when(core.io.debugResetOut) {
        resetCtrl.doReset := True
      }
    }
  }

  io.gpioA <> axi.gpioACtrl.io.gpio
  io.gpioB <> axi.gpioBCtrl.io.gpio
  io.jtag  <> axi.jtagCtrl.io.jtag
}


object Pinsec{
  def main(args: Array[String]) {
    SpinalConfig().dumpWave().generateVerilog(new Pinsec)
    SpinalConfig().dumpWave().generateVhdl(new Pinsec)
  }
}