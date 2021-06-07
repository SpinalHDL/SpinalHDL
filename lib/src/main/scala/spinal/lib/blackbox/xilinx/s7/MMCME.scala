package spinal.lib.blackbox.xilinx.s7

import spinal.core._
import spinal.core.fiber.{Handle, Unset}
import spinal.lib._
import spinal.lib.bus.bmb._
import spinal.lib.bus.misc.SizeMapping

//      input      [15:0] DO,
//      input             DRDY,
//      input             LOCKED,
//      output reg        DWE,
//      output reg        DEN,
//      output reg [6:0]  DADDR,
//      output reg [15:0] DI,
//      output            DCLK,

//class MMCME {
//
//}
//
//class MMCME2_BASE extends  BlackBox{
//  val DO = out Bits(16 bits)
//  val DRDY = out Bool()
//  val DWE = in Bool()
//  val DEN = in Bool()
//  val DADDR = in Bits(7 bits)
//  val DI = in Bits(16 bits)
//  val DCLK = in Bool()
//}
//
//
//class


case class Mmcme2Dbus() extends Bundle with IMasterSlave {
  val DO = out Bits(16 bits)
  val DRDY = out Bool()
  val DWE = in Bool()
  val DEN = in Bool()
  val DADDR = in UInt(7 bits)
  val DI = in Bits(16 bits)
//  val DCLK = in Bool()

  override def asMaster() = {
    out(DWE, DEN, DADDR, DI)
    in(DO, DRDY)
  }
}

object Mmcme2Ctrl{
  def getBmbCapabilities(accessSource : BmbAccessCapabilities) = BmbSlaveFactory.getBmbCapabilities(
    accessSource,
    addressWidth = addressWidth,
    dataWidth = 32
  )
  def addressWidth = 12
}

case class Mmcme2Ctrl(p : BmbParameter) extends Component{
  val io = new Bundle {
    val ctrl = slave(Bmb(p))
    val dbus = master(Mmcme2Dbus())
  }

  assert(p.access.dataWidth == 32)
  val state = RegInit(U"00")
  val context = Reg(io.ctrl.cmd.context)
  val source = Reg(io.ctrl.cmd.source)
  io.dbus.DEN := io.ctrl.cmd.valid && state === 0
  io.dbus.DWE := io.ctrl.cmd.isWrite
  io.dbus.DADDR := (io.ctrl.cmd.address >> 2).resized
  io.dbus.DI := io.ctrl.cmd.data(15 downto 0)
  io.ctrl.cmd.ready := io.dbus.DEN

  io.ctrl.rsp.valid := state === 2
  io.ctrl.rsp.data := io.dbus.DO.resized
  io.ctrl.rsp.context := context
  io.ctrl.rsp.source := source
  io.ctrl.rsp.last := True
  io.ctrl.rsp.setSuccess()

  switch(state){
    is(0){
      context := io.ctrl.cmd.context
      source := io.ctrl.cmd.source
      when(io.ctrl.cmd.fire){ state := 1}
    }
    is(1){
      when(io.dbus.DRDY){ state := 2}
    }
    is(2){
      when(io.ctrl.rsp.ready){ state := 0}
    }
  }
}


class Mmcme2CtrlGenerator(CtrlOffset : Handle[BigInt] = Unset)
                              (implicit interconnect: BmbInterconnectGenerator, decoder : BmbImplicitPeripheralDecoder = null) extends Area {

  val accessSource       = Handle[BmbAccessCapabilities]
  val accessCapabilities = Handle(Mmcme2Ctrl.getBmbCapabilities(accessSource))
  val accessRequirements = Handle[BmbAccessParameter]

  val logic     = Handle(Mmcme2Ctrl(accessRequirements.toBmbParameter()))
  val ctrl      = Handle(logic.io.ctrl)
  val dbus      = Handle(logic.io.dbus)

  interconnect.addSlave(
    accessSource       = accessSource,
    accessCapabilities = accessCapabilities,
    accessRequirements = accessRequirements,
    bus                = ctrl,
    mapping            = Handle(SizeMapping(CtrlOffset, 1 << Mmcme2Ctrl.addressWidth))
  )

  if(decoder != null) interconnect.addConnection(decoder.bus, ctrl)
}



