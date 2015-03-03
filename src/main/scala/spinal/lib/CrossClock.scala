package spinal.lib

import spinal._



object CCBuffer {
  def apply[T <: Data](input: T): T = apply(input, null.asInstanceOf[T])
  def apply[T <: Data](input: T, init: T): T = apply(input,init, ClockDomain.current, 2)
  def apply[T <: Data](input: T, init: T = null, clockOut: ClockDomain, bufferDepth: Int): T = {
    assert(bufferDepth >= 1)

    val buffers = Vec(bufferDepth, Reg(input, init))

    buffers(0) := input
    buffers(0).addTag(crossClockDomain)
    for (i <- 1 until bufferDepth) {
      buffers(i) := buffers(i - 1)
      buffers(i).addTag(crossClockBuffer)
    }
    return buffers.last

  }
}

object CCHandshakeByToggle {
  def apply[T <: Data](input: Handshake[T], clockIn: ClockDomain, clockOut: ClockDomain): Handshake[T] = {
    val c = new CCHandshakeByToggle[T](input.data,clockIn,clockOut)
    c.io.input connectFrom input
    return c.io.output
  }
}


class CCHandshakeByToggle[T <: Data](dataType: T, clockIn: ClockDomain, clockOut: ClockDomain) extends Component {
  val io = new Bundle {
    val input = slave Handshake (dataType)
    val output = master Handshake (dataType)
  }

  val outHitSignal = Bool()

  val inputArea = new ClockingArea(clockIn) {
    val hit = CCBuffer(outHitSignal, Bool(false))
    val target = RegInit(Bool(false))
    val data = Reg(io.input.data)
    io.input.ready := Bool(false)
    when(io.input.valid && hit === target) {
      target := !target
      data := io.input.data
      io.input.ready := Bool(true)
    }
  }


  val outputArea = new ClockingArea(clockOut) {
    val target = CCBuffer(inputArea.target, Bool(false))
    val hit = RegInit(Bool(false))
    outHitSignal := hit

    val handshake = io.input.clone
    handshake.valid := (target !== hit)
    handshake.data := inputArea.data
    when(handshake.fire) {
      hit := !hit
    }

    io.output << handshake.m2sPipe(true)
  }
}

object CCPulseByToggle {
  def apply(input: Bool, clockIn: ClockDomain, clockOut: ClockDomain): Bool = {
    clockIn.push
    val inToogle = RegInit(Bool(false))
    when(input) {
      inToogle := !inToogle
    }

    clockIn.pop
    clockOut.push

    val outTarget = CCBuffer(inToogle, Bool(false))
    val outHit = RegInit(Bool(false));

    when(outTarget !== outHit) {
      outHit := !outHit
    }

    clockOut.pop

    return outTarget !== outHit
  }

}

