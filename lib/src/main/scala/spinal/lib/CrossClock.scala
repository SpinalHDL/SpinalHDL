package spinal.lib

import spinal.core._


object BufferCC {
  def apply[T <: Data](input: T, init: T = null, bufferDepth: Int = 2): T = {
    val c = new BufferCC(input, init != null, bufferDepth)
    c.io.dataIn := input
    if(init != null) c.io.initial := init

    val ret = cloneOf(c.io.dataOut)
    ret := c.io.dataOut
    return ret
  }
}

class BufferCC[T <: Data](dataType: T, withInit : Boolean, bufferDepth: Int) extends Component {
  assert(bufferDepth >= 1)

  val io = new Bundle {
    val initial = if(!withInit) null.asInstanceOf[T] else in(cloneOf(dataType))
    val dataIn = in(cloneOf(dataType))
    val dataOut = out(cloneOf(dataType))
  }

  val buffers = Vec(Reg(dataType, io.initial),bufferDepth)

  buffers(0) := io.dataIn
  buffers(0).addTag(crossClockDomain)
  for (i <- 1 until bufferDepth) {
    buffers(i) := buffers(i - 1)
    buffers(i).addTag(crossClockBuffer)

  }

  io.dataOut := buffers.last
}


object PulseCCByToggle {
  def apply(input: Bool, clockIn: ClockDomain, clockOut: ClockDomain): Bool = {
    val c = new PulseCCByToggle(clockIn,clockOut)
    c.io.pulseIn := input
    return c.io.pulseOut
  }
}


class PulseCCByToggle(clockIn: ClockDomain, clockOut: ClockDomain) extends Component{
  val io = new Bundle{
    val pulseIn = in Bool
    val pulseOut = out Bool
  }

  val inArea = new ClockingArea(clockIn) {
    val target = RegInit(False)
    when(io.pulseIn) {
      target := !target
    }
  }

  val outArea = new ClockingArea(clockOut) {
    val target = BufferCC(inArea.target, False)
    val hit    = RegInit(False);

    when(target =/= hit) {
      hit := !hit
    }

    io.pulseOut := (target =/= hit)
  }
}