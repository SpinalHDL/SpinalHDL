package spinal.lib.cpu.riscv.impl.extension

import spinal.core._
import spinal.lib.bus.amba3.apb.{Apb3, Apb3Config}
import spinal.lib.cpu.riscv.impl._
import spinal.lib.cpu.riscv.impl.Utils._
import spinal.lib._
import spinal.lib.bus.avalon._

/**
 * Created by PIC32F_USER on 12/04/2016.
 */


object DebugExtension{
  def getAvalonMMConfig = AvalonMMConfig.fixed(
    addressWidth = 8,
    dataWidth = 32,
    readLatency = 1
  ).copy(
    useByteEnable = false,
    addressUnits = WORDS
  )

  def avalonToDebugBus(avalon: AvalonMM,debug : DebugExtensionBus): Unit ={
    assert(avalon.config == getAvalonMMConfig)
    debug.cmd.valid := avalon.read || avalon.write
    debug.cmd.wr := avalon.write
    debug.cmd.address := avalon.address
    debug.cmd.data := avalon.writeData

    avalon.waitRequestn := True
    avalon.readData := debug.rsp.data
  }

  def Apb3ToDebugBus(apb: Apb3,debug : DebugExtensionBus): Unit ={
    assert(apb.config.addressWidth >= debug.cmd.address.getWidth + 2)
    assert(apb.config.dataWidth == 32)
    assert(apb.config.selWidth == 1)
    debug.cmd.valid := apb.PSEL(0) && !apb.PENABLE
    debug.cmd.wr := apb.PWRITE
    debug.cmd.address := (apb.PADDR >> 2).resized
    debug.cmd.data := apb.PWDATA
    if(apb.config.useSlaveError) apb.PSLVERROR := False

    apb.PREADY := True
    apb.PRDATA := debug.rsp.data
  }
}

case class DebugExtensionCmd() extends Bundle{
  val wr = Bool
  val address = UInt(8 bit)
  val data = Bits(32 bit)
}
case class DebugExtensionRsp() extends Bundle{
  val data = Bits(32 bit)
}

case class DebugExtensionBus() extends Bundle with IMasterSlave{
  val cmd = Stream(DebugExtensionCmd())
  val rsp = DebugExtensionRsp() //One cycle latency

  override def asMaster(): Unit = {
    master(cmd)
    in(rsp)
  }
}

case class DebugExtensionIo() extends Bundle with IMasterSlave{
  val bus = DebugExtensionBus()
  val resetOut = Bool

  override def asMaster(): Unit = {
    master(bus)
    in(resetOut)
  }
}

class DebugExtension(val clockDomain: ClockDomain) extends CoreExtension{
  var io : DebugExtensionIo = null
  override def applyIt(core: RiscvCore): Area = clockDomain(new Area{ //Can't use Clocking area because of scala error
    io = slave(DebugExtensionIo())

    val busReadDataReg = Reg(Bits(32 bit))
    io.bus.cmd.ready := True
    io.bus.rsp.data := busReadDataReg

    val readRegFileReg = RegNext(False)
    val resetIt = RegInit(False)
    val haltIt = RegInit(False)
    val flushIt = RegNext(False)
    val stepIt = RegInit(False)

      val isPipActive = RegNext(RegNext(core.iCmd.valid) || (core.fetch.pendingPrefetch =/= 0) ||  core.decode.inInst.valid ||  core.execute0.inInst.valid ||  core.execute1.inInst.valid || core.writeBack.inInst.valid)
    val isPipBusy = isPipActive || RegNext(isPipActive)
    val isInBreakpoint = core.writeBack.inInst.valid && isMyTag(core.writeBack.inInst.ctrl)


    when(io.bus.cmd.valid) {
      when(io.bus.cmd.address.msb){//access special register else regfile
        switch(io.bus.cmd.address(io.bus.cmd.address.high-1 downto 0)) {
          is(0){
            when(io.bus.cmd.wr){
              flushIt := io.bus.cmd.data(2)
              stepIt := io.bus.cmd.data(4)
              resetIt setWhen(io.bus.cmd.data(16))
              haltIt  setWhen(io.bus.cmd.data(17))
              resetIt clearWhen(io.bus.cmd.data(24))
              haltIt  clearWhen(io.bus.cmd.data(25))
            } otherwise{
              busReadDataReg(0) := resetIt
              busReadDataReg(1) := haltIt
              busReadDataReg(2) := isPipBusy
              busReadDataReg(3) := isInBreakpoint
              busReadDataReg(4) := stepIt
              busReadDataReg(5) := core.prefetch.inc
            }
          }
          is(1){
            when(io.bus.cmd.wr){
              core.prefetch.pc := io.bus.cmd.data.asUInt
              core.prefetch.inc := False
            } otherwise{
              when(isInBreakpoint){
                busReadDataReg := core.writeBack.inInst.pc.asBits
              } otherwise{
                busReadDataReg := core.prefetch.pc.asBits
              }
            }
          }
          is(2){
            when(io.bus.cmd.wr){
              val injectedInstructionSent = RegNext(core.decode.inInst.fire) init(False)
              core.decode.inInst.valid.getDrivingReg := !injectedInstructionSent
              core.decode.inInst.instruction.getDrivingReg := io.bus.cmd.data
              io.bus.cmd.ready := injectedInstructionSent
            }
          }
        }
      } otherwise{
        when(io.bus.cmd.wr){
          core.writeBack.regFileWrite.valid := True
          core.writeBack.regFileWrite.address := io.bus.cmd.address(core.writeBack.regFileWrite.address.range)
          core.writeBack.regFileWrite.data := io.bus.cmd.data
        } otherwise {
          core.decode.regFileReadAddress0 := io.bus.cmd.address(core.writeBack.regFileWrite.address.range)
          core.c.regFileReadyKind match{
            case `async` => busReadDataReg := core.decode.src0
            case `sync` => readRegFileReg := True
          }
        }
      }
    }

    //Keep the execution pipeline empty after break instruction
    when(core.execute1.inInst.valid && isMyTag(core.execute1.inInst.ctrl)){
      core.execute0.halt := True
    }

    when(isInBreakpoint){
      core.execute0.halt := True
      core.writeBack.halt := True
    }

    when(flushIt) {
      core.writeBack.flush := True
    }

    when(readRegFileReg){
      io.bus.rsp.data := core.decode.src0
    }

    when(haltIt){
      core.prefetch.halt := True
    }

    when(stepIt && core.iCmd.fire){
      haltIt := True
    }

    io.resetOut := RegNext(resetIt)

    core.writeBack.irq.inhibate setWhen(haltIt || stepIt)
  })

  override def needTag: Boolean = true

  override def getName: String = "DebugExtension"

  override def instructionCtrlExtension(instruction: Bits, ctrl: InstructionCtrl): Unit = {
    when(instruction === 0x00100073){
      applyTag(ctrl)
    }
  }
}