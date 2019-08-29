package spinal.lib.soc.pinsec


import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba3.apb._
import spinal.lib.bus.amba4.axi._
import spinal.lib.com.jtag.Jtag
import spinal.lib.com.uart.{Apb3UartCtrl, Uart, UartCtrlGenerics, UartCtrlMemoryMappedConfig}
import spinal.lib.cpu.riscv.impl.Utils.BR
import spinal.lib.cpu.riscv.impl.build.RiscvAxi4
import spinal.lib.cpu.riscv.impl.extension.{BarrelShifterFullExtension, DivExtension, MulExtension}
import spinal.lib.cpu.riscv.impl._
import spinal.lib.graphic.RgbConfig
import spinal.lib.graphic.vga.{Axi4VgaCtrl, Axi4VgaCtrlGenerics, Vga}
import spinal.lib.io.TriStateArray
import spinal.lib.memory.sdram._
import spinal.lib.memory.sdram.sdr.{Axi4SharedSdramCtrl, IS42x320D, SdramInterface, SdramTimings}
import spinal.lib.system.debugger.{JtagAxi4SharedDebugger, SystemDebuggerConfig}


case class PinsecConfig(axiFrequency : HertzNumber,
                        onChipRamSize : BigInt,
                        sdramLayout: SdramLayout,
                        sdramTimings: SdramTimings,
                        cpu : RiscvCoreConfig,
                        iCache : InstructionCacheConfig)

object PinsecConfig{
  def default = {
    val config = PinsecConfig(
      axiFrequency = 100 MHz,
      onChipRamSize  = 4 KiB,
      sdramLayout = IS42x320D.layout,
      sdramTimings = IS42x320D.timingGrade7,
      cpu = RiscvCoreConfig(
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
      ),
      iCache = InstructionCacheConfig(
        cacheSize =4096,
        bytePerLine =32,
        wayCount = 1,  //Can only be one for the moment
        wrappedMemAccess = true,
        addressWidth = 32,
        cpuDataWidth = 32,
        memDataWidth = 32
      )
    )
    //The CPU has a systems of plugin which allow to add new feature into the core.
    //Those extension are not directly implemented into the core, but are kind of additive logic patch defined in a separated area.
    config.cpu.add(new MulExtension)
    config.cpu.add(new DivExtension)
    config.cpu.add(new BarrelShifterFullExtension)

    config
  }
}



class Pinsec(config: PinsecConfig) extends Component{

  //Legacy constructor
  def this(axiFrequency: HertzNumber) {
    this(PinsecConfig.default.copy(axiFrequency = axiFrequency))
  }

  import config._
  val debug = true
  val interruptCount = 4
  def vgaRgbConfig = RgbConfig(5,6,5)

  val io = new Bundle{
    //Clocks / reset
    val asyncReset = in Bool
    val axiClk     = in Bool
    val vgaClk     = in Bool

    //Main components IO
    val jtag       = slave(Jtag())
    val sdram      = master(SdramInterface(sdramLayout))

    //Peripherals IO
    val gpioA      = master(TriStateArray(32 bits))
    val gpioB      = master(TriStateArray(32 bits))
    val uart       = master(Uart())
    val vga        = master(Vga(vgaRgbConfig))
    val timerExternal = in(PinsecTimerCtrlExternal())
  }

  val resetCtrlClockDomain = ClockDomain(
    clock = io.axiClk,
    config = ClockDomainConfig(
      resetKind = BOOT
    )
  )

  val resetCtrl = new ClockingArea(resetCtrlClockDomain) {
    val axiResetUnbuffered  = False
    val coreResetUnbuffered = False

    //Implement an counter to keep the reset axiResetOrder high 64 cycles
    // Also this counter will automaticly do a reset when the system boot.
    val axiResetCounter = Reg(UInt(6 bits)) init(0)
    when(axiResetCounter =/= U(axiResetCounter.range -> true)){
      axiResetCounter := axiResetCounter + 1
      axiResetUnbuffered := True
    }
    when(BufferCC(io.asyncReset)){
      axiResetCounter := 0
    }

    //When an axiResetOrder happen, the core reset will as well
    when(axiResetUnbuffered){
      coreResetUnbuffered := True
    }

    //Create all reset used later in the design
    val axiReset  = RegNext(axiResetUnbuffered)
    val coreReset = RegNext(coreResetUnbuffered)
    val vgaReset  = BufferCC(axiResetUnbuffered)
  }

  val axiClockDomain = ClockDomain(
    clock = io.axiClk,
    reset = resetCtrl.axiReset,
    frequency = FixedFrequency(axiFrequency) //The frequency information is used by the SDRAM controller
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

      new RiscvAxi4(
        coreConfig = config.cpu,
        iCacheConfig = config.iCache,
        dCacheConfig = null,
        debug = debug,
        interruptCount = interruptCount
      )
    }

    val ram = Axi4SharedOnChipRam(
      dataWidth = 32,
      byteCount = onChipRamSize,
      idWidth = 4
    )

    val sdramCtrl = Axi4SharedSdramCtrl(
      axiDataWidth = 32,
      axiIdWidth   = 4,
      layout       = sdramLayout,
      timing       = sdramTimings,
      CAS          = 3
    )

    val jtagCtrl = JtagAxi4SharedDebugger(SystemDebuggerConfig(
      memAddressWidth = 32,
      memDataWidth    = 32,
      remoteCmdWidth  = 1
    ))


    val apbBridge = Axi4SharedToApb3Bridge(
      addressWidth = 20,
      dataWidth    = 32,
      idWidth      = 4
    )

    val gpioACtrl = Apb3Gpio(
      gpioWidth = 32,
      withReadSync = true
    )
    val gpioBCtrl = Apb3Gpio(
      gpioWidth = 32,
      withReadSync = true
    )
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
      ram.io.axi       -> (0x00000000L,   onChipRamSize),
      sdramCtrl.io.axi -> (0x40000000L,   sdramLayout.capacity),
      apbBridge.io.axi -> (0xF0000000L,   1 MiB)
    )

    axiCrossbar.addConnections(
      core.io.i       -> List(ram.io.axi, sdramCtrl.io.axi),
      core.io.d       -> List(ram.io.axi, sdramCtrl.io.axi, apbBridge.io.axi),
      jtagCtrl.io.axi -> List(ram.io.axi, sdramCtrl.io.axi, apbBridge.io.axi),
      vgaCtrl.io.axi  -> List(                              sdramCtrl.io.axi)
    )

    axiCrossbar.addPipelining(apbBridge.io.axi)((crossbar,bridge) => {
      crossbar.sharedCmd.halfPipe() >> bridge.sharedCmd
      crossbar.writeData.halfPipe() >> bridge.writeData
      crossbar.writeRsp             << bridge.writeRsp
      crossbar.readRsp              << bridge.readRsp
    })

    axiCrossbar.addPipelining(sdramCtrl.io.axi)((crossbar,ctrl) => {
      crossbar.sharedCmd.halfPipe()  >>  ctrl.sharedCmd
      crossbar.writeData            >/-> ctrl.writeData
      crossbar.writeRsp              <<  ctrl.writeRsp
      crossbar.readRsp               <<  ctrl.readRsp
    })

    axiCrossbar.build()


    val apbDecoder = Apb3Decoder(
      master = apbBridge.io.apb,
      slaves = List(
        gpioACtrl.io.apb -> (0x00000, 4 KiB),
        gpioBCtrl.io.apb -> (0x01000, 4 KiB),
        uartCtrl.io.apb  -> (0x10000, 4 KiB),
        timerCtrl.io.apb -> (0x20000, 4 KiB),
        vgaCtrl.io.apb   -> (0x30000, 4 KiB),
        core.io.debugBus -> (0xF0000, 4 KiB)
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
      resetCtrl.coreResetUnbuffered setWhen(core.io.debugResetOut)
    }
  }

  io.gpioA          <> axi.gpioACtrl.io.gpio
  io.gpioB          <> axi.gpioBCtrl.io.gpio
  io.timerExternal  <> axi.timerCtrl.io.external
  io.jtag           <> axi.jtagCtrl.io.jtag
  io.uart           <> axi.uartCtrl.io.uart
  io.sdram          <> axi.sdramCtrl.io.sdram
  io.vga            <> axi.vgaCtrl.io.vga
}


object Pinsec{
  def main(args: Array[String]) {
    val config = SpinalConfig(verbose = true).dumpWave()
    val report = config.generateVerilog(new Pinsec(PinsecConfig.default))
//    report.unusedSignals.toList.sortBy(_.getInstanceCounter).foreach(bt => SpinalWarning(s"Unused wire detected : $bt"))
//    report.prunedSignals.toList.sortBy(_.getInstanceCounter).foreach(bt => SpinalWarning(s"Pruned wire detected : $bt"))
    //    config.generateVerilog(new Pinsec(PinsecConfig.default))
//    config.generateVhdl(new Pinsec(PinsecConfig.default))
  }
}