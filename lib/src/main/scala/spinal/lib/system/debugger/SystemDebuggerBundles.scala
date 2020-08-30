package spinal.lib.system.debugger

import spinal.core._
import spinal.lib.bus.amba4.axi.Axi4Shared
import spinal.lib.bus.avalon.AvalonMM
import spinal.lib.bus.avalon._
import spinal.lib._
import spinal.lib.bus.bmb.Bmb
import spinal.lib.bus.simple.PipelinedMemoryBus


case class SystemDebuggerRsp(c : SystemDebuggerConfig) extends Bundle{
  val error = Bool
  val data = Bits(c.memDataWidth bit)
}

case class SystemDebuggerRemoteBus(c : SystemDebuggerConfig) extends Bundle with IMasterSlave{
  val cmd = Stream Fragment(Bits(c.remoteCmdWidth bit))
  val rsp = Stream (SystemDebuggerRsp(c))

  override def asMaster(): Unit = {
    master(cmd)
    slave(rsp)
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

  override def asMaster(): Unit = {
    master(cmd)
    slave(rsp)
  }


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
    ) << cmd.address(1 downto 0)).resized

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

  def toPipelinedMemoryBus(): PipelinedMemoryBus = {
    assert(c.memAddressWidth == 32)
    assert(c.memDataWidth == 32)
    val avalonConfig = c.getMemAvalonConfig
    val mm = PipelinedMemoryBus(32,32)
    mm.cmd.arbitrationFrom(cmd)
    mm.cmd.write := cmd.wr
    mm.cmd.address := cmd.address(cmd.address.high downto 2) @@ U"00"
    mm.cmd.data := cmd.data //No allignment needed, done by remote
    mm.cmd.mask := (cmd.size.mux (
      U(0) -> B"0001",
      U(1) -> B"0011",
      default -> B"1111"
    ) << cmd.address(1 downto 0)).resized

    rsp.valid := mm.rsp.valid
    rsp.payload := mm.rsp.data
    mm
  }


  def toAxi4Shared(): Axi4Shared = {
    assert(c.memDataWidth == 32)
    val busConfig = c.getMemAxi4SharedConfig
    val mm = Axi4Shared(busConfig)
    val (cmdFork,dataForkTmp) = StreamFork2(cmd)
    val dataFork = dataForkTmp.throwWhen(!dataForkTmp.wr)

    mm.sharedCmd.valid := cmdFork.valid
    mm.sharedCmd.write := cmdFork.wr
    mm.sharedCmd.addr  := cmdFork.address
    mm.sharedCmd.size  := cmdFork.size.resized
    mm.writeData.valid := dataFork.valid
    mm.writeData.data  := dataFork.data //No allignment needed, done by remote
    mm.writeData.strb  := (dataFork.size.mux (
      U(0) -> B"0001",
      U(1) -> B"0011",
      default -> B"1111"
    ) << dataFork.address(1 downto 0)).resized
    mm.writeData.last  := True

    val contextIn = Stream(UInt(2 bit))
    contextIn.valid := cmdFork.fire && !cmdFork.wr
    contextIn.payload := cmdFork.address(1 downto 0)

    val contextOut = contextIn.m2sPipe()
    contextOut.ready := rsp.fire

    cmdFork.ready := mm.sharedCmd.ready
    dataFork.ready := mm.writeData.ready
    rsp.valid := mm.readRsp.valid
    rsp.payload := mm.readRsp.data >> (contextOut.payload*8)

    mm.readRsp.ready := True
    mm.writeRsp.ready := True
    mm
  }

  def toBmb(): Bmb = {
    assert(c.memDataWidth == 32)
    val p = c.getBmbParameter
    val bmb = Bmb(p)
    bmb.cmd.arbitrationFrom(cmd)
    bmb.cmd.last := True
    bmb.cmd.length := 3
    bmb.cmd.opcode := (cmd.wr ? B(Bmb.Cmd.Opcode.WRITE) | B(Bmb.Cmd.Opcode.READ))
    bmb.cmd.address := (cmd.address >> 2) @@ U"00"
    bmb.cmd.data := cmd.data
    bmb.cmd.mask := (cmd.size.mux (
      U(0) -> B"0001",
      U(1) -> B"0011",
      default -> B"1111"
    ) << cmd.address(1 downto 0)).resized

    rsp.valid := bmb.rsp.valid
    rsp.payload := bmb.rsp.data
    bmb.rsp.ready := True

    bmb
  }
}

