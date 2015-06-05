package spinal.demo.mandelbrot

import net.liftweb.json.DefaultFormats
import net.liftweb.json.Extraction._
import net.liftweb.json.JsonAST._
import net.liftweb.json.Printer._
import spinal.core._
import spinal.lib._

import scala.collection.mutable

class CmdInterface(p: MandelbrotCoreParameters) extends Component {
  val io = new Bundle {
    val cmdPort = slave Flow Fragment(Bits(8 bit))
    val frameTask = master Stream FrameTask(p)
  }

  io.frameTask << io.cmdPort.toFlowOf(FrameTask(p)).toStream

}

class FrameTaskFilter(p: MandelbrotCoreParameters) extends Component {
  val io = new Bundle {
    val input = slave Stream FrameTask(p)
    val output = master Stream FrameTask(p)
  }

  io.output <-< io.input

  //TODO better initial value
  io.output.valid init(True)
  io.output.data.start.x init (-1.0)
  io.output.data.start.y init (-1.0)
  io.output.data.inc.x init (2.0 / p.screenResX)
  io.output.data.inc.y init (2.0 / p.screenResY)
}


class MandelbrotCore(p: MandelbrotCoreParameters) extends Component {
  val io = new Bundle {
    val cmdPort = slave Flow Fragment(Bits(8 bit))
    val retPort = master Stream Fragment(Bits(8 bit))

    val pixelResult = master Stream Fragment(PixelResult(p))
  }

  val cmdInterface = new CmdInterface(p)
  cmdInterface.io.cmdPort << io.cmdPort.filterHeader(0x01)

  val frameTaskFilter = new FrameTaskFilter(p)
  frameTaskFilter.io.input << cmdInterface.io.frameTask

  val frameTaskSolver = new FrameTaskSolver(p)
  frameTaskSolver.io.frameTask << frameTaskFilter.io.output

  io.pixelResult << frameTaskSolver.io.pixelResult



  val passportEvent = io.cmdPort eventOn (0xFF)
  val passport = passportEvent.translateWith(S(p.uid, 32 bit)).fragmentTransaction(8)

  io.retPort << passport

  implicit val formats = DefaultFormats //++ LogicAnalyser.jsonSerDes
  val json = decompose(MandelbrotJsonReport(p,p.uid.toString))
  GlobalData.get.addJsonReport(pretty(render(json)))
}
