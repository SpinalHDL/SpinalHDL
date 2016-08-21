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

class Pinsec extends Component{
  val debug = true
  val interruptCount = 4
//  val ahbConfig = Axi4Config(addressWidth = 32,dataWidth = 32)
//  val apbConfig = Apb3Config(addressWidth = 16,dataWidth = 32)

  val io = new Bundle{
//    val ahbAccess = slave(Axi4(ahbConfig))
//    val jtag = slave(Jtag())
    val gpioA = master(TriStateArray(32 bits))
    val gpioB = master(TriStateArray(32 bits))
    val interrupt = in Bits(interruptCount bits)
//    val debugResetIn  = if(debug) in Bool else null
    val debugResetOut = if(debug) out Bool else null
  }




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


  val core      = new RiscvAxi4(coreConfig,iCacheConfig,dCacheConfig,debug,interruptCount)
////  val rom       = Axi4OnChipRam(ahbConfig,byteCount = 512 KB)
//  val rom       = new Axi4OnChipRom(ahbConfig,{
//    val bytes = Files.readAllBytes(new File("E:/vm/share/pinsec_test.bin").toPath()).map(v => BigInt(if(v < 0) v + 256 else v))
//    val array =  (0 until bytes.length/4).map(i => B(bytes(i*4+0) + (bytes(i*4+1) << 8) + (bytes(i*4+2) << 16) + (bytes(i*4+3) << 24),32 bits))
//    array
//  })
  val ram,rom       = Axi4SharedOnChipRam(
    dataWidth = 32,
    byteCount = 16 KB,
    idWidth = 4
  )

  val apbBridge = Axi4SharedToApb3Bridge(
    addressWidth = 20,
    dataWidth = 32,
    idWidth = 4
  )

//  val axiMaster0 = slave(Axi4ReadOnly(Axi4Config(32,32,2)))
//  val axiMaster1 = slave(Axi4Shared(Axi4Config(32,32,2)))

  val ahbInterconnect = Axi4InterconnectFactory()
    .addSlaves(
      rom.io.axi       -> (0x00000000L, 512 KB),
      ram.io.axi       -> (0x04000000L, 512 KB),
      apbBridge.io.axi -> (0xF0000000L,   1 MB)
    ).addConnections(
//      axiMaster0
//        -> List(rom.io.axi, ram.io.axi),
//      axiMaster1
//        -> List(rom.io.axi, ram.io.axi, apbBridge.io.axi),
      core.io.i
        -> List(rom.io.axi, ram.io.axi),
      core.io.d
        -> List(rom.io.axi, ram.io.axi, apbBridge.io.axi)
//      io.ahbAccess
//        -> List(rom.io.ahb, ram.io.ahb, apbBridge.io.ahb)
    ).build()

  val gpioACtrl  = Apb3Gpio(32)
  val gpioBCtrl  = Apb3Gpio(32)

  val apbDecoder = Apb3Interconnect(
    master = apbBridge.io.apb,
    slaves = List(
      gpioACtrl.io.apb  -> (0x00000, 4 KB),
      gpioBCtrl.io.apb  -> (0x01000, 4 KB),
      core.io.debugBus ->  (0xF0000, 4 KB)
    )
  )
//
  if(interruptCount != 0) core.io.interrupt := io.interrupt
  if(debug){
    core.io.debugResetIn  <> ClockDomain.current.readResetWire//io.debugResetIn
    core.io.debugResetOut <> io.debugResetOut
  }
//  io.debugResetOut := True
  gpioACtrl.io.gpio <> io.gpioA
  gpioBCtrl.io.gpio <> io.gpioB
}


object Pinsec{
  def main(args: Array[String]) {
    SpinalConfig().dumpWave().generateVerilog(new Pinsec)
    SpinalConfig().dumpWave().generateVhdl(new Pinsec)
  }
}