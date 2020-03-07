package spinal.demo.mandelbrot

import net.liftweb.json.DefaultFormats
import net.liftweb.json.Extraction._
import net.liftweb.json.JsonAST._
import net.liftweb.json.Printer._
import spinal.core._
import spinal.lib._

case class MandelbrotCoreConfig(p: MandelbrotCoreParameters) extends Bundle {
  val frameTaskFilterEnable = Bool
}

class CmdInterface(p: MandelbrotCoreParameters) extends Component {
  val io = new Bundle {
    val cmdPort = slave Flow Fragment(Bits(8 bit))
    val retPort = master Stream Fragment(Bits(8 bit))
    val frameTask = master Flow FrameTask(p)
    val coreConfig = out(MandelbrotCoreConfig(p))
  }

  io.frameTask << io.cmdPort.filterHeader(0x01).toFlowOf(FrameTask(p))
  io.coreConfig := io.cmdPort.filterHeader(0x02).toRegOf(MandelbrotCoreConfig(p))
  io.coreConfig.frameTaskFilterEnable init (False)

  val passport = io.cmdPort.eventOn(0xFF).translateWith(S(p.uid, 32 bit)).fragmentTransaction(8)
  io.retPort << passport
}

class FrameTaskFilter(p: MandelbrotCoreParameters) extends Component {
  val io = new Bundle {
    val input = slave Flow FrameTask(p)
    val output = master Flow FrameTask(p)
  }

  val filterIn = io.input.toReg
  filterIn.start.x init (-1.0)
  filterIn.start.y init (-1.0)
  filterIn.inc.x init (2.0 / p.screenResX)
  filterIn.inc.y init (2.0 / p.screenResY)

  def rcFilter(in: SFix, tao: Double, enable: Bool, enableHz: Double): SFix = {
    val shiftRight = log2Up(BigDecimal(enableHz * tao).toBigInt)
    val out = Reg(in) init (0)
    when(enable) {
      out := RegNext(((in - out) >>| shiftRight)) + out
    }
    out
  }
  def rcChainFilter(in: SFix, taos: Seq[Double], enable: Bool, enableHz: Double): SFix = {
    var ptr = in
    for (tao <- taos) {
      ptr = rcFilter(ptr, tao, enable, enableHz)
    }
    ptr
  }

  val clkHz = ClockDomain.current.frequency.getValue
  val filterTaos = Seq(1.0, 2.0)
  val filterHz = 120.0

  val filterEnable = RegNext(CounterFreeRun((clkHz / filterHz).toInt).willOverflow) //TODO periodic pulse lib

  io.output.valid := True
  io.output.start.x := rcChainFilter(filterIn.start.x, filterTaos, filterEnable, filterHz)
  io.output.start.y := rcChainFilter(filterIn.start.y, filterTaos, filterEnable, filterHz)
  io.output.inc.x := rcChainFilter(filterIn.inc.x, filterTaos, filterEnable, filterHz)
  io.output.inc.y := rcChainFilter(filterIn.inc.y, filterTaos, filterEnable, filterHz)
}


class   MandelbrotCore(p: MandelbrotCoreParameters) extends Component {
  val io = new Bundle {
    val cmdPort = slave Flow Fragment(Bits(8 bit))
    val retPort = master Stream Fragment(Bits(8 bit))

    val pixelResult = master Stream Fragment(PixelResult(p))
  }

  val cmdInterface = new CmdInterface(p)
  cmdInterface.io.cmdPort << io.cmdPort
  cmdInterface.io.retPort >> io.retPort

  val frameTaskFilter = new FrameTaskFilter(p)
  frameTaskFilter.io.input << cmdInterface.io.frameTask

  val frameTaskFilterBypass = Mux(
    cmdInterface.io.coreConfig.frameTaskFilterEnable,
    frameTaskFilter.io.output,
    cmdInterface.io.frameTask
  )

  val frameTaskSolver = new FrameTaskSolver(p)
  frameTaskSolver.io.frameTask <-< frameTaskFilterBypass.toStream

  // Force a initial frametask into the system.
  // By this way, when the system is reseted, it draw directly something
  frameTaskSolver.io.frameTask.valid init (True)
  frameTaskSolver.io.frameTask.start.x init (-1.0)
  frameTaskSolver.io.frameTask.start.y init (-1.0)
  frameTaskSolver.io.frameTask.inc.x init (2.0 / p.screenResX)
  frameTaskSolver.io.frameTask.inc.y init (2.0 / p.screenResY)

  io.pixelResult << frameTaskSolver.io.pixelResult


  implicit val formats = DefaultFormats
  //++ LogicAnalyser.jsonSerDes
  val json = decompose(MandelbrotJsonReport(p, p.uid.toString))
  GlobalData.get.addJsonReport(prettyRender(json))
}
case class MandelbrotJsonReport(p : MandelbrotCoreParameters,
                                uid : String,
                                clazz : String = "uidPeripheral",
                                kind : String = "mandelbrotCore")
