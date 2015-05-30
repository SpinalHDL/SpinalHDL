package spinal.demo.mandelbrot

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
    val pixelResult = master Stream Fragment(PixelResult(p))
  }

  val cmdInterface = new CmdInterface(p)
  cmdInterface.io.cmdPort << io.cmdPort

  val frameTaskFilter = new FrameTaskFilter(p)
  frameTaskFilter.io.input << cmdInterface.io.frameTask

  val frameTaskSolver = new FrameTaskSolver(p)
  frameTaskSolver.io.frameTask << frameTaskFilter.io.output

  io.pixelResult << frameTaskSolver.io.pixelResult
}
