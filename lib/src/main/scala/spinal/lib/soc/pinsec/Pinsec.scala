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

class Pinsec(axiFrequency : BigDecimal) extends Component{
  val debug = true
  val interruptCount = 4
  def vgaRgbConfig = RgbConfig(5,6,5)

  val io = new Bundle{
    val asyncReset = in Bool
    val axiClk = in Bool
    val vgaClk = in Bool
    val jtag  = slave(Jtag(useTck = true))
    val gpioA = master(TriStateArray(32 bits))
    val gpioB = master(TriStateArray(32 bits))
    val timerExternal = in(PinsecTimerCtrlExternal())
    val uart  = master(Uart())
    val sdram = master(SdramInterface(IS42x320D.layout))
    val vga   = master(Vga(vgaRgbConfig))
  }

  val resetClockDomain = ClockDomain(
    clock = io.axiClk,
    config = ClockDomainConfig(
      resetKind = BOOT
    )
  )

  val resetCtrl = new ClockingArea(resetClockDomain) {
    val axiResetOrder  = False
    val coreResetOrder = False setWhen(axiResetOrder)

    val axiResetCounter = Reg(UInt(6 bits)) init(0)
    when(axiResetCounter =/= U(axiResetCounter.range -> true)){
      axiResetCounter := axiResetCounter + 1
      axiResetOrder := True
    }
    when(BufferCC(io.asyncReset)){
      axiResetCounter := 0
    }

    val axiReset  = RegNext(axiResetOrder)
    val coreReset = RegNext(coreResetOrder)
    val vgaReset  = BufferCC(axiReset)
  }

  val axiClockDomain = ClockDomain(
    clock = io.axiClk,
    reset = resetCtrl.axiReset,
    frequency = FixedFrequency(axiFrequency)
  )

  val coreClockDomain = ClockDomain(
    clock = io.axiClk,
    reset = resetCtrl.coreReset
  )

  val vgaClockDomain = ClockDomain(
    clock = io.vgaClk,
    reset = resetCtrl.vgaReset
  )

  val jtagClockDomain = ClockDomain(
    clock = io.jtag.tck
  )

  val axi = new ClockingArea(axiClockDomain) {
    val core = coreClockDomain{
      val coreConfig = CoreConfig(
        pcWidth = 32,
        addrWidth = 32,
        startAddress = 0x00000000,
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

      new RiscvAxi4(
        coreConfig = coreConfig,
        iCacheConfig = iCacheConfig,
        dCacheConfig = null,
        debug = debug,
        interruptCount = interruptCount
      )
    }

    val ram = Axi4SharedOnChipRam(
      dataWidth = 32,
      byteCount = 4 kB,
      idWidth = 4
    )

    val sdramCtrl = Axi4SharedSdramCtrl(
      dataWidth = 32,
      idWidth   = 4,
      layout    = IS42x320D.layout,
      timing    = IS42x320D.timingGrade7,
      CAS       = 3
    )

    val jtagCtrl = JtagAxi4SharedDebugger(SystemDebuggerConfig(
      memAddressWidth = 32,
      memDataWidth    = 32,
      remoteCmdWidth  = 1,
      jtagClockDomain = jtagClockDomain
    ))


    val apbBridge = Axi4SharedToApb3Bridge(
      addressWidth = 20,
      dataWidth    = 32,
      idWidth      = 4
    )

    val gpioACtrl = Apb3Gpio(32)
    val gpioBCtrl = Apb3Gpio(32)
    val timerCtrl = PinsecTimerCtrl()

    val uartCtrlConfig = UartCtrlMemoryMappedConfig(
      uartCtrlConfig = UartCtrlGenerics(
        dataWidthMax      = 8,
        clockDividerWidth = 20,
        preSamplingSize   = 1,
        samplingSize      = 5,
        postSamplingSize  = 2
      ),
      txFifoDepth = 16,
      rxFifoDepth = 16
    )
    val uartCtrl = Apb3UartCtrl(uartCtrlConfig)


    val vgaCtrlConfig = Axi4VgaCtrlGenerics(
      axiAddressWidth = 32,
      axiDataWidth    = 32,
      burstLength     = 8,
      frameSizeMax    = 2048*1512*2,
      fifoSize        = 512,
      rgbConfig       = vgaRgbConfig,
      vgaClock        = vgaClockDomain
    )
    val vgaCtrl = Axi4VgaCtrl(vgaCtrlConfig)

    val axiCrossbar = Axi4CrossbarFactory()

    axiCrossbar.addSlaves(
      ram.io.axi       -> (0x00000000L,   4 kB),
      sdramCtrl.io.axi -> (0x40000000L,  64 MB),
      apbBridge.io.axi -> (0xF0000000L,   1 MB)
    )

    axiCrossbar.addConnections(
      core.io.i       -> List(ram.io.axi, sdramCtrl.io.axi),
      core.io.d       -> List(ram.io.axi, sdramCtrl.io.axi, apbBridge.io.axi),
      jtagCtrl.io.axi -> List(ram.io.axi, sdramCtrl.io.axi, apbBridge.io.axi),
      vgaCtrl.io.axi  -> List(                        sdramCtrl.io.axi)
    )

    axiCrossbar.addPipelining(apbBridge.io.axi,(crossbar,bridge) => {
      crossbar.sharedCmd.halfPipe() >> bridge.sharedCmd
      crossbar.writeData.halfPipe() >> bridge.writeData
      crossbar.writeRsp             << bridge.writeRsp
      crossbar.readRsp              << bridge.readRsp
    })

    axiCrossbar.addPipelining(sdramCtrl.io.axi,(crossbar,ctrl) => {
      crossbar.sharedCmd.halfPipe()  >>  ctrl.sharedCmd
      crossbar.writeData            >/-> ctrl.writeData
      crossbar.writeRsp              <<  ctrl.writeRsp
      crossbar.readRsp               <<  ctrl.readRsp
    })

    axiCrossbar.build()


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

  io.gpioA          <> axi.gpioACtrl.io.gpio
  io.gpioB          <> axi.gpioBCtrl.io.gpio
  io.timerExternal  <> axi.timerCtrl.io.external
  io.jtag.unclocked <> axi.jtagCtrl.io.jtag
  io.uart           <> axi.uartCtrl.io.uart
  io.sdram          <> axi.sdramCtrl.io.sdram
  io.vga            <> axi.vgaCtrl.io.vga
}


object Pinsec{
  def main(args: Array[String]) {
    val config = SpinalConfig().dumpWave()
    config.generateVerilog(new Pinsec(100 MHz))
    config.generateVhdl(new Pinsec(100 MHz))
  }
}