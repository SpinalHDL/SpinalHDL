package spinal.lib.cpu.riscv.impl.extension

import spinal.core._
import spinal.lib.cpu.riscv.impl._
import spinal.lib.cpu.riscv.impl.Utils._
import spinal.lib._
import spinal.lib.bus.avalon.mm._

/**
 * Created by PIC32F_USER on 12/04/2016.
 */


object DebugExtension{
  def getAvalonMMConfig = AvalonMMConfig.fixed(
    addressWidth = 8,
    dataWidth = 32).copy(
      useByteEnable = false,
      addressUnits = words
    )

  def avalonToDebugBus(avalon: AvalonMMBus,debug : DebugExtensionBus): Unit ={
    assert(avalon.c == getAvalonMMConfig)
    debug.cmd.valid := avalon.read || avalon.write
    debug.cmd.wr := avalon.write
    debug.cmd.address := avalon.address
    debug.cmd.data := avalon.writeData
    avalon.waitRequestn := True

    avalon.readData := debug.rsp.data
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

  override def asMaster(): DebugExtensionBus.this.type = {
    master(cmd)
    in(rsp)
    this
  }
}

case class DebugExtensionIo() extends Bundle with IMasterSlave{
  val bus = DebugExtensionBus()
  val reset = Bool
  
  override def asMaster(): this.type = {
    master(bus)
    in(reset)
    this
  }
}

class DebugExtension extends CoreExtension{
  var io : DebugExtensionIo = null
  override def applyIt(core: Core): Area = new Area{
    io = slave(DebugExtensionIo())
    
    val busReadDataReg = Reg(Bits(32 bit))
    io.bus.cmd.ready := True
    io.bus.rsp.data := busReadDataReg
    
//    io.bus.rsp.valid := RegNext(io.bus.cmd.valid) init(False)
//    io.bus.rsp.data := RegNext(core.fetchCmd.pc.asBits)
    val readRegFileReg = RegNext(False)
    val resetIt = RegInit(False)
    val haltIt = RegInit(False)
    val isRunning = True
    when(io.bus.cmd.valid) {
      when(io.bus.cmd.address.msb){//access special register else regfile
        switch(io.bus.cmd.address(io.bus.cmd.address.high-1 downto 0)) {
          is(0){
            when(io.bus.cmd.wr){
              resetIt := io.bus.cmd.data(0)
              haltIt := io.bus.cmd.data(1)
            } otherwise{
              busReadDataReg(0) := resetIt
              busReadDataReg(1) := haltIt
              busReadDataReg(2) := isRunning
            }
          }
          is(1){
            when(io.bus.cmd.wr){
              core.fetchCmd.pc := io.bus.cmd.data.asUInt
              core.fetchCmd.inc := False
            } otherwise{
              busReadDataReg := core.fetchCmd.pc.asBits
            }
          }
        }
      } otherwise{
        when(io.bus.cmd.wr){
          core.writeBack0.regFileWrite.valid := True
          core.writeBack0.regFileWrite.address := io.bus.cmd.address(core.writeBack0.regFileWrite.address.range)
          core.writeBack0.regFileWrite.data := io.bus.cmd.data
        } otherwise {
          core.decode.regFileReadAddress0 := io.bus.cmd.address(core.writeBack0.regFileWrite.address.range)
          core.c.regFileReadyKind match{
            case `async` => busReadDataReg := core.decode.src0
            case `sync` => readRegFileReg := True
          }
        }
      }
      when(readRegFileReg){
        io.bus.rsp.data := core.decode.src0
      }
      io.reset := RegNext(resetIt) init(False)
      when(haltIt){
        core.fetchCmd.halt := True
      }
    }


    //Keep the execution pipeline empty after break instruction
    when(isMyTag(core.writeBack0.inInst.ctrl) || isMyTag(core.execute1.inInst.ctrl)){
      core.execute0.halt := True
    }
  }

  override def needTag: Boolean = true

  override def getName: String = "DebugExtension"

  override def instructionCtrlExtension(instruction: Bits, ctrl: InstructionCtrl): Unit = {
    when(instruction === 0x00100073){
      applyTag(ctrl)
    }
  }
}