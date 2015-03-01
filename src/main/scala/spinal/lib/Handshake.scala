package spinal.lib

import spinal._

object Handshake {
  def apply[T <: Data](gen: T) = new Handshake(gen)



}

class Handshake[T <: Data](gen: T) extends Bundle with Interface {
  val valid = Bool()
  val ready = Bool()
  val data = gen.clone()

  override def clone: this.type = Handshake(gen).asInstanceOf[this.type]

  override def asMaster: Unit = {
    valid.asOutput
    ready.asInput
    data.asOutput
  }

  override def asSlave: Unit = {
    valid.asInput
    ready.asOutput
    data.asInput
  }

  def <<(that: Handshake[T]): Handshake[T] = connectFrom(that)
  def <-<(that: Handshake[T]): Handshake[T] = this m2sPipeFrom that
  def </<(that: Handshake[T]): Handshake[T] = this s2mPipeFrom that
  def <-/<(that: Handshake[T]): Handshake[T] = {
    this m2sPipeFrom (clone s2mPipeFrom that)
  }
  def ~[T2 <: Data](that: T2): Handshake[T2] = translateWith(that)
  def &(cond: Bool): Handshake[T] = continueIf(cond)


  def fire: Bool = valid & ready

  def connectFrom(that: Handshake[T]): Handshake[T] = {
    this.valid := that.valid
    that.ready := this.ready
    this.data := that.data
    that
  }

  def m2sPipeFrom(that: Handshake[T]): Handshake[T] = {
    val rValid = RegInit(Bool(false))
    val rData = Reg(gen)

    that.ready := (!this.valid) || this.ready

    when(that.ready) {
      rValid := that.valid
      rData := that.data
    }

    this.valid := rValid
    this.data := rData
    that
  }

  def s2mPipeFrom(that: Handshake[T]): Handshake[T] = {
    val rValid = RegInit(Bool(false))
    val rBits = Reg(gen)

    this.valid := that.valid || rValid
    that.ready := !rValid
    this.data := Mux(rValid, rBits, that.data)

    when(this.ready) {
      rValid := Bool(false)
    }

    when(that.ready && (!this.ready)) {
      rValid := that.valid
      rBits := that.data
    }
    that
  }

  def translateWith[T2 <: Data](that: T2): Handshake[T2] = {
    val next = new Handshake(that)
    next.valid := this.valid
    this.ready := next.ready
    next.data := that
    next
  }

  def continueIf(cond: Bool): Handshake[T] = {
    val next = new Handshake(gen)
    next.valid := this.valid && cond
    this.ready := next.ready && cond
    next.data := this.data
    return next
  }

  def throwIf(cond: Bool): Handshake[T] = {
    val next = this.clone

    next connectFrom this
    when(cond) {
      next.valid := Bool(false)
      this.ready := Bool(true)
    }
    next
  }

  def haltIf(cond: Bool): Handshake[T] = continueIf(!cond)
  def takeIf(cond: Bool): Handshake[T] = throwIf(!cond)
}






class HandshakeArbiterCoreIO[T <: Data](dataType: T, portCount: Int) extends Bundle {
  val inputs = Vec(portCount, slave Handshake (dataType))
  val output = master Handshake (dataType)
  val chosen = out UInt (log2Up(portCount) bit)
}

abstract class HandshakeArbiterCore[T <: Data](dataType: T, portCount: Int, allowSwitchWithoutConsumption: Boolean) extends Component {
  val io = new HandshakeArbiterCoreIO(dataType, portCount)

  val locked = RegInit(Bool(false))

  val maskProposal = Vec(portCount, Bool())
  val maskLocked = Reg(Vec(portCount, Bool()))
  val maskRouted = if (allowSwitchWithoutConsumption) maskProposal else Mux(locked, maskLocked, maskProposal)


  when(io.output.valid) {
    //Lock
    locked := Bool(true)
    maskLocked := maskRouted
  }
  when(io.output.ready) {
    //unlock
    locked := Bool(false)
  }


  //Route
  var outputValid = Bool(false)
  var outputData = Bits(0)
  for ((input, mask) <- (io.inputs, maskRouted).zipped) {
    outputValid = outputValid | input.valid
    outputData = outputData | Mux(mask, input.data.toBits, Bits(0))
    input.ready := mask & io.output.ready
  }
  io.output.valid := outputValid
  io.output.data.fromBits(outputData)

  io.chosen := OHToUInt(maskRouted)



  //  val zips = (io.inputs, maskRouted).zipped
  //  io.chosen := OHToUInt(maskRouted)
  //  io.output.valid := io.inputs.map(_.valid).reduceLeft(_ | _)
  //  io.output.data.fromBits(zips.map((d, b) => Mux(b, d.data.toBits, Bits(0))).reduceLeft(_ | _))
  //  zips.foreach(_.ready := _ && io.output.ready)
}

//TODOTEST
class HandshakeArbiterPriorityImpl[T <: Data](dataType: T, portCount: Int, allowSwitchWithoutConsumption: Boolean = false) extends HandshakeArbiterCore(dataType, portCount, allowSwitchWithoutConsumption) {
  var search = Bool(true)
  for (i <- 0 to portCount - 2) {
    maskProposal(i) := search & io.inputs(i).valid
    search = search & !io.inputs(i).valid
  }
  maskProposal(portCount - 1) := search
}


//TODOTEST
class HandshakeFork[T <: Data](dataType: T, portCount: Int) extends Component {
  val io = new Bundle {
    val in = slave Handshake (dataType)
    val out = Vec(portCount, master Handshake (dataType))
  }
  val linkEnable = Vec(portCount, RegInit(Bool(true)))

  io.in.ready := Bool(true)
  for (i <- (0 to portCount - 1)) {
    when(!io.out(i).ready && linkEnable(i)) {
      io.in.ready := Bool(false)
    }
  }

  for (i <- (0 to portCount - 1)) {
    io.out(i).valid := io.in.valid && linkEnable(i)
    io.out(i).data := io.in.data
    when(io.out(i).fire) {
      linkEnable(i) := Bool(false)
    }
  }

  when(io.in.ready) {
    linkEnable.map(_ := Bool(true))
  }
}

//TODOTEST
class HandshakeDemux[T <: Data](dataType: T, portCount: Int) extends Component {
  val io = new Bundle {
    val sel = in UInt (log2Up(portCount) bit)
    val input = slave Handshake (dataType)
    val output = Vec(portCount, master Handshake (dataType))
  }
  io.input.ready := Bool(false)

  for (i <- 0 to portCount - 1) {
    io.output(i).data := io.input.data
    when(UInt(i) !== io.sel) {
      io.output(i).valid := Bool(false)
    } otherwise {
      io.output(i).valid := io.input.valid
      io.input.ready := io.output(i).ready
    }
  }
}