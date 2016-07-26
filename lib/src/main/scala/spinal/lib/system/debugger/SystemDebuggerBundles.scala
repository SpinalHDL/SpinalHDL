package spinal.lib.system.debugger

import spinal.core._
import spinal.lib.bus.avalon.AvalonMM
import spinal.lib.bus.avalon._
import spinal.lib._


case class SystemDebuggerRsp(c : SystemDebuggerConfig) extends Bundle{
  val error = Bool
  val data = Bits(c.memDataWidth bit)
}

case class SystemDebuggerRemoteBus(c : SystemDebuggerConfig) extends Bundle with IMasterSlave{
  val cmd = Stream Fragment(Bits(c.remoteCmdWidth bit))
  val rsp = Stream (SystemDebuggerRsp(c))

  override def asMaster(): SystemDebuggerRemoteBus.this.type = {
    master(cmd)
    slave(rsp)
    this
  }
}

case class SystemDebuggerMemCmd(c : SystemDebuggerConfig) extends Bundle{
  val address = UInt(c.memAddressWidth bit)
  val data = Bits(c.memDataWidth bit)
  val wr = Bool
  val size = UInt(log2Up(log2Up(c.memDataWidth/8)+1) bit)
}


case class SystemDebuggerMemBus(c: SystemDebuggerConfig) extends Bundle with IMasterSlave{
  val cmd = Stream (SystemDebuggerMemCmd(c))
  val rsp = Flow (Bits(c.memDataWidth bit))

  override def asMaster(): this.type = {
    master(cmd)
    slave(rsp)
    this
  }

  override def asSlave(): this.type = asMaster.flip()

  def toAvalon(): AvalonMM = {
    assert(c.memDataWidth == 32)
    val avalonConfig = c.getMemAvalonConfig
    val mm = AvalonMM(avalonConfig)
    mm.read := cmd.valid && !cmd.wr
    mm.write := cmd.valid && cmd.wr
    mm.address := cmd.address(cmd.address.high downto 2) @@ U"00"
    mm.writeData := cmd.data //No allignment needed, done by remote
    mm.byteEnable := (cmd.size.mux (
      U(0) -> B"0001",
      U(1) -> B"0011",
      default -> B"1111"
    ) << mm.address(1 downto 0)).resized

    val contextIn = Stream(UInt(2 bit))
    contextIn.valid := cmd.fire && !cmd.wr
    contextIn.payload := cmd.address(1 downto 0)

    val contextOut = contextIn.m2sPipe()
    contextOut.ready := rsp.fire

    cmd.ready := mm.waitRequestn
    rsp.valid := mm.readDataValid
    rsp.payload := mm.readData >> (contextOut.payload*8)
    mm
  }
}

