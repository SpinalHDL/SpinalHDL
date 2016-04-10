package spinal.lib.com.jtag.debugger

import spinal.core._
import spinal.lib.bus.avalon.mm._
import spinal.lib.com.jtag._
import spinal.lib._
import spinal.lib.tool.QSysify

/**
 * Created by PIC32F_USER on 09/04/2016.
 */

class JtagDebugger(c: SystemDebuggerConfig,jtagClockDomain : ClockDomain,systemClockDomain : ClockDomain) extends Component {
  val io = new Bundle {
    val jtag = slave(Jtag())
    val mem = master(SystemDebuggerMemBus(c))
  }

  val system = new ClockingArea(systemClockDomain){
    val cmd = Flow(Fragment(Bits))
    val rsp = Reg(Flow(SystemDebuggerRsp(c)))
    rsp.valid init(False)

    when(cmd.valid){
      rsp.valid := False
    }

    val debugger = new SystemDebugger(c)
    debugger.io.remote.cmd << cmd.toStream
    debugger.io.mem <> io.mem

    when(debugger.io.remote.rsp.fire){
      rsp.valid := True
      rsp.payload := debugger.io.remote.rsp.payload
    }
    debugger.io.remote.rsp.ready := True //TODO
  }

  val jtag = new ClockingArea(jtagClockDomain){
    implicit val tap = new JtagTap(io.jtag, 4)
    val idcodeArea = new JtagInstructionIdcode(B"x10001FFF", 1)
    val writeArea = new JtagInstructionFlowFragmentPush(system.cmd,systemClockDomain, 2)
    val readArea = new JtagInstructionRead(system.rsp, 3)
    readArea.shifter.addTag(crossClockDomain)
  }
}

case class SystemDebuggerConfig(cpuDataWidth : Int = 32,
                                memAddressWidth : Int = 32,
                                memDataWidth : Int = 32,
                                remoteCmdWidth : Int){
  def getMemAvalonConfig = AvalonMMConfig.pipelined(addressWidth = memAddressWidth,
                                                    dataWidth = memDataWidth)
}

case class SystemDebuggerRsp(c : SystemDebuggerConfig) extends Bundle{
  val error = Bool
  val data = Bits(c.memDataWidth bit)
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

  def toAvalon(): AvalonMMBus = {
    assert(c.memDataWidth == 32)
    val avalonConfig = c.getMemAvalonConfig
    val mm = AvalonMMBus(avalonConfig)
    mm.read := cmd.valid && !cmd.wr
    mm.write := cmd.valid && cmd.wr
    mm.address := cmd.address(cmd.address.high downto 2) @@ U"00"
    mm.writeData := cmd.data //No allignment needed, done by remote
    mm.byteEnable := (cmd.size.map (
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

case class SystemDebuggerMemCmd(c : SystemDebuggerConfig) extends Bundle{
  val address = UInt(c.memAddressWidth bit)
  val data = Bits(c.memDataWidth bit)
  val wr = Bool
  val size = UInt(log2Up(log2Up(c.memDataWidth/8)+1) bit)
}

class SystemDebugger(c : SystemDebuggerConfig) extends Component{
  val io = new Bundle{
    val remote = new Bundle {
      val cmd = slave Stream Fragment(Bits(c.remoteCmdWidth bit))
      val rsp = master Stream (SystemDebuggerRsp(c))
    }
    val mem = master(SystemDebuggerMemBus(c))
  }

  val dispatcher = new StreamFragmentBitsDispatcher(io.remote.cmd,8)
    .add(io.mem.cmd,0)
    .build()
  io.remote.rsp.valid := io.mem.rsp.valid
  io.remote.rsp.error := False
  io.remote.rsp.data := io.mem.rsp.payload
}



class JtagAvalonDebugger(jtagClock : ClockDomain,val systemClock : ClockDomain) extends Component{
  val c = SystemDebuggerConfig(
    cpuDataWidth = 32,
    memAddressWidth = 32,
    memDataWidth = 32,
    remoteCmdWidth = 1
  )

  val io = new Bundle{
    val jtag = slave(Jtag())
    val mem = master(AvalonMMBus(c.getMemAvalonConfig))
  }

  val debugger = new JtagDebugger(jtagClockDomain = jtagClock,systemClockDomain = systemClock,c=c)
  debugger.io.jtag <> io.jtag
  val bridge = new ClockingArea(systemClock) {
    debugger.io.mem.toAvalon() <> io.mem
  }
}
object JtagAvalonDebuggerMain{
  def main(args: Array[String]) {
    val toplevel = SpinalVhdl({
      val tck = Bool.setName("tck")
      val jtagClock = ClockDomain(tck)
      val systemClock = ClockDomain.external("system")
      new JtagAvalonDebugger(jtagClock = jtagClock, systemClock = systemClock)
    },_.onlyStdLogicVectorTopLevelIo.setLibrary("lib_JtagAvalonDebugger")).topLevel

    toplevel.io.mem addTag(ClockDomainTag(toplevel.systemClock))
    QSysify(toplevel)
  }
}