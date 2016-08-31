package spinal.lib.soc.pinsec

import java.io.File
import java.nio.file.Files

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba3.apb._
import spinal.lib.bus.amba4.axi._
import spinal.lib.com.jtag.Jtag
import spinal.lib.com.uart.{Uart, UartCtrlGenerics, UartCtrlMemoryMappedConfig, Apb3UartCtrl}
import spinal.lib.cpu.riscv.impl.build.RiscvAxi4
import spinal.lib.cpu.riscv.impl.extension.{BarrelShifterFullExtension, DivExtension, MulExtension}
import spinal.lib.cpu.riscv.impl._
import spinal.lib.graphic.RgbConfig
import spinal.lib.graphic.vga.{Vga, Axi4VgaCtrlGenerics, Axi4VgaCtrl}
import spinal.lib.io.TriStateArray
import spinal.lib.memory.sdram._
import spinal.lib.system.debugger.{JtagAxi4SharedDebugger, SystemDebuggerConfig}

class Pinsec extends Component{
  val debug = true
  val interruptCount = 4
  def vgaRgbConfig = RgbConfig(5,6,5)

  val io = new Bundle{
    val asyncReset = in Bool
    val axiClk = in Bool
    val vgaClk = in Bool
    val jtag_tck = in Bool
    val jtag = slave(Jtag())
    val gpioA = master(TriStateArray(32 bits))
    val gpioB = master(TriStateArray(32 bits))
    val timerExternal = in(PinsecTimerCtrlExternal())
    val uart  = master(Uart())
    val sdram = master(SdramInterface(IS42x320D.layout))
    val vga   = master(Vga(vgaRgbConfig))
  }


  val resetCtrl = new ClockingArea(ClockDomain(io.axiClk,config = ClockDomainConfig(resetKind = BOOT))) {
    val axiResetOrder = False
    val axiResetCounter = Reg(UInt(6 bits)) init(0)
    when(axiResetCounter =/= U(axiResetCounter.range -> true)){
      axiResetCounter := axiResetCounter + 1
      axiResetOrder := True
    }
    when(BufferCC(io.asyncReset)){
      axiResetCounter := 0
    }

    val coreResetOrder = False setWhen(axiResetOrder)

    val axiReset  = RegNext(axiResetOrder)
    val vgaReset  = BufferCC(axiReset)
    val coreReset = RegNext(coreResetOrder)
  }


  val axi = new ClockingArea(ClockDomain(io.axiClk,resetCtrl.axiReset,frequency = ClockDomain.current.frequency)) {

    val core = ClockDomain(io.axiClk,resetCtrl.coreReset){ //The RISCV Core has a separate reset
      val coreConfig = CoreConfig(
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
      
      val iCacheConfig = InstructionCacheConfig(
        cacheSize =4096,
        bytePerLine =32,
        wayCount = 1,  //Can only be one for the moment
        wrappedMemAccess = true,
        addressWidth = 32,
        cpuDataWidth = 32,
        memDataWidth = 32
      )

      new RiscvAxi4(coreConfig, iCacheConfig, null, debug, interruptCount)
    }

    val rom = Axi4SharedOnChipRam(
      dataWidth = 32,
      byteCount = 128 kB,
      idWidth = 4
    )

    val ram = Axi4SharedOnChipRam(
      dataWidth = 32,
      byteCount = 32 kB,
      idWidth = 4
    )

    val sdramCtrl = Axi4SharedSdramCtrl(
      dataWidth = 32,
      idWidth = 4,
      layout = IS42x320D.layout,
      timing = IS42x320D.timingGrade7,
      CAS = 3
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
    val timerCtrl = PinsecTimerCtrl()
    val uartCtrl = Apb3UartCtrl(UartCtrlMemoryMappedConfig(
      uartCtrlConfig = UartCtrlGenerics(
        dataWidthMax = 8,
        clockDividerWidth = 20,
        preSamplingSize = 1,
        samplingSize = 5,
        postSamplingSize = 2
      ),
      txFifoDepth = 16,
      rxFifoDepth = 16
    ))

    val vgaCtrl = Axi4VgaCtrl(Axi4VgaCtrlGenerics(
      axiAddressWidth = 32,
      axiDataWidth = 32,
      burstLength = 8,
      frameSizeMax = 2048*1512*2,
      fifoSize = 512,
      rgbConfig = vgaRgbConfig,
      vgaClock = ClockDomain(io.vgaClk,resetCtrl.vgaReset)
    ))

    val axiCrossbar = Axi4CrossbarFactory()
      .addSlaves(
        rom.io.axi       ->(0x00000000L, 128 kB),
        ram.io.axi       ->(0x04000000L,  32 kB),
        sdramCtrl.io.axi ->(0x40000000L,  64 MB),
        apbBridge.io.axi ->(0xF0000000L,   1 MB)
      ).addConnections(
        core.io.i       -> List(rom.io.axi, ram.io.axi, sdramCtrl.io.axi),
        core.io.d       -> List(rom.io.axi, ram.io.axi, sdramCtrl.io.axi, apbBridge.io.axi),
        jtagCtrl.io.axi -> List(rom.io.axi, ram.io.axi, sdramCtrl.io.axi, apbBridge.io.axi),
        vgaCtrl.io.axi  -> List(                        sdramCtrl.io.axi)
      ).addPipelining(apbBridge.io.axi,(from,to) => {
        from.sharedCmd.halfPipe() >> to.sharedCmd
        from.writeData.halfPipe() >> to.writeData
        from.writeRsp << to.writeRsp
        from.readRsp << to.readRsp
      }).addPipelining(sdramCtrl.io.axi,(from,to) => {
        from.sharedCmd.halfPipe() >> to.sharedCmd
        from.writeData >/-> to.writeData
        from.writeRsp << to.writeRsp
        from.readRsp << to.readRsp
      }).build()


    val apbDecoder = Apb3Decoder(
      master = apbBridge.io.apb,
      slaves = List(
        gpioACtrl.io.apb -> (0x00000, 4 kB),
        gpioBCtrl.io.apb -> (0x01000, 4 kB),
        uartCtrl.io.apb  -> (0x10000, 4 kB),
        timerCtrl.io.apb -> (0x20000, 4 kB),
        vgaCtrl.io.apb   -> (0x30000, 4 kB),
        core.io.debugBus -> (0xF0000, 4 kB)
      )
    )

    if (interruptCount != 0) {
      core.io.interrupt := (
        (0 -> uartCtrl.io.interrupt),
        (1 -> timerCtrl.io.interrupt),
        (default -> false)
      )
    }

    if (debug) {
      core.io.debugResetIn := resetCtrl.axiReset
      resetCtrl.coreResetOrder setWhen(core.io.debugResetOut)
    }
  }

  io.gpioA         <> axi.gpioACtrl.io.gpio
  io.gpioB         <> axi.gpioBCtrl.io.gpio
  io.timerExternal <> axi.timerCtrl.io.external
  io.jtag          <> axi.jtagCtrl.io.jtag
  io.uart          <> axi.uartCtrl.io.uart
  io.sdram         <> axi.sdramCtrl.io.sdram
  io.vga           <> axi.vgaCtrl.io.vga
}


object Pinsec{
  def main(args: Array[String]) {
    val config = SpinalConfig(defaultClockDomainFrequency = FixedFrequency(50 MHz)).dumpWave()
    config.generateVerilog(new Pinsec)
    config.generateVhdl(new Pinsec)
  }
}