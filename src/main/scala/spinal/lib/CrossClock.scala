package spinal.lib

import spinal._


object CCBuffer {
  def apply[T <: Data](input: T): T = apply(input, null.asInstanceOf[T])
  def apply[T <: Data](input: T, init: T): T = apply(input, init, 2)
  def apply[T <: Data](input: T, init: T, bufferDepth: Int): T = {
    val c = new CCBuffer(input, init != null, bufferDepth)
    c.io.input := input
    if(init != null) c.io.init := init
    return c.io.output
  }
}

class CCBuffer[T <: Data](dataType: T, withInit : Boolean, bufferDepth: Int) extends Component {
  assert(bufferDepth >= 1)

  val io = new Bundle {
    val input = in(dataType.clone)
    val init = if(!withInit) null.asInstanceOf[T] else in(dataType.clone)
    val output = out(dataType.clone)
  }

  val buffers = Vec(bufferDepth, Reg(dataType, io.init))

  buffers(0) := io.input
  buffers(0).addTag(crossClockDomain)
  for (i <- 1 until bufferDepth) {
    buffers(i) := buffers(i - 1)
    buffers(i).addTag(crossClockBuffer)

  }

  io.output := buffers.last


}

object CCHandshakeByToggle {
  def apply[T <: Data](input: Handshake[T], clockIn: ClockDomain, clockOut: ClockDomain): Handshake[T] = {
    val c = new CCHandshakeByToggle[T](input.data, clockIn, clockOut)
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
    val c = new CCPulseByToggle(clockIn,clockOut)
    c.io.input := input
    return c.io.output

  }

}


class CCPulseByToggle(clockIn: ClockDomain, clockOut: ClockDomain) extends Component{
  val io = new Bundle{
    val input = in Bool()
    val output = in Bool()
  }
  val inputArea = new ClockingArea(clockIn) {
    val target = RegInit(Bool(false))
    when(io.input) {
      target := !target
    }
  }

  val outputArea = new ClockingArea(clockOut) {
    val target = CCBuffer(inputArea.target, Bool(false))
    val hit = RegInit(Bool(false));

    when(target !== hit) {
      hit := !hit
    }

    io.output := (target !== hit)
  }
}