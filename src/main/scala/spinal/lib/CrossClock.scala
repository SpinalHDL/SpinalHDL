package spinal.lib

import spinal._


object CrossClockBuffer {
  def apply[T <: Data](input: T): T = apply(input, null.asInstanceOf[T])
  def apply[T <: Data](input: T, init: T): T = apply(input, ClockDomain.current, 2, init)
  def apply[T <: Data](input: T, clockOut: ClockDomain, bufferDepth: Int, init: T = null): T = {
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

object CrossClockStream_HandShake {
  def apply[T <: Data](input: Handshake[T], clockIn: ClockDomain, clockOut: ClockDomain): Handshake[T] = {
    val c = new CrossClockStream_HandShake(input.data,clockIn,clockOut) //TODO create intelij scala issus coode code marked red
    c.io.input connectFrom input
    return c.io.output
  }
}


class CrossClockStream_HandShake[T <: Data](dataType: T, clockIn: ClockDomain, clockOut: ClockDomain) extends Component {
  val io = new Bundle {
    val input = slave Handshake (dataType)
    val output = master Handshake (dataType)
  }

  val outHitSignal = Bool()

  val clockInDomain = new ComponentPartClockDomain(clockIn) {
    val hit = CrossClockBuffer(outHitSignal, Bool(false))
    val target = RegInit(Bool(false))
    val data = Reg(io.input.data)
    io.input.ready := Bool(false)
    when(io.input.valid && hit === target) {
      target := !target
      data := io.input.data
      io.input.ready := Bool(true)
    }
  }




  val clockOutDomain = new ComponentPartClockDomain(clockOut) {
    val target = CrossClockBuffer(clockInDomain.target, Bool(false))
    val hit = RegInit(Bool(false))
    outHitSignal := hit

    val handshake = io.input.clone
    handshake.valid := (target !== hit)
    handshake.data := clockInDomain.data
    when(handshake.fire) {
      hit := !hit
    }

    val output = handshake.m2sPipe(true)
    output.data.addTag(crossClockDomain)
    io.output << output
  }
}

object CrossClockEvent_HandShake {
  def apply(input: Bool, clockIn: ClockDomain, clockOut: ClockDomain): Bool = {
    clockIn.push
    val inToogle = RegInit(Bool(false))
    when(input) {
      inToogle := !inToogle
    }

    clockIn.pop
    clockOut.push

    val outTarget = CrossClockBuffer(inToogle, Bool(false))
    val outHit = RegInit(Bool(false));

    when(outTarget !== outHit) {
      outHit := !outHit
    }

    clockOut.pop

    return outTarget !== outHit
  }

}

