package spinal.lib

import spinal._

object Handshake {
 // implicit def autoCast[T2 <: Data, T <: T2](that: Handshake[T]): Handshake[T2] = that.asInstanceOf[Handshake[T2]]




  def apply[T <: Data](dataType: T) = new Handshake(dataType)


}

class Handshake[T <: Data](dataType: T) extends Bundle with Interface {
  val valid = Bool()
  val ready = Bool()
  val data = dataType.clone()

  override def clone: this.type = Handshake(dataType).asInstanceOf[this.type]

  override def asMaster: this.type = {
    valid.asOutput
    ready.asInput
    data.asOutput
    this
  }

  override def asSlave: this.type = asMaster.flip

  def <<(that: Handshake[T]): Handshake[T] = connectFrom(that)

  def <-<(that: Handshake[T]): Handshake[T] = {
    this << that.m2sPipe
    that
  }

  def </<(that: Handshake[T]): Handshake[T] = {
    this << that.s2mPipe
    that
  }

  def <-/<(that: Handshake[T]): Handshake[T] = {
    this << that.s2mPipe.m2sPipe
    that
  }

  def ~[T2 <: Data](that: T2): Handshake[T2] = translateWith(that)
  def &(cond: Bool): Handshake[T] = continueIf(cond)


  def fire: Bool = valid & ready
  def isFree: Bool = !valid || ready
  def connectFrom(that: Handshake[T]): Handshake[T] = {
    this.valid := that.valid
    that.ready := this.ready
    this.data := that.data
    that
  }


  def m2sPipe: Handshake[T] = m2sPipe(false)


  def m2sPipe(crossClockData : Boolean) : Handshake[T] = {
    val ret = Handshake(dataType)

    val rValid = RegInit(Bool(false))
    val rData = Reg(dataType)
    if(crossClockData) rData.addTag(crossClockDomain)

    this.ready := (!ret.valid) || ret.ready

    when(this.ready) {
      rValid := this.valid
      rData := this.data
    }

    ret.valid := rValid
    ret.data := rData


    ret
  }

  def s2mPipe: Handshake[T] = {
    val ret = Handshake(dataType)

    val rValid = RegInit(Bool(false))
    val rBits = Reg(dataType)

    ret.valid := this.valid || rValid
    this.ready := !rValid
    ret.data := Mux(rValid, rBits, this.data)

    when(ret.ready) {
      rValid := Bool(false)
    }

    when(this.ready && (!ret.ready)) {
      rValid := this.valid
      rBits := this.data
    }
    ret
  }

  def translateWith[T2 <: Data](that: T2): Handshake[T2] = {
    val next = new Handshake(that)
    next.valid := this.valid
    this.ready := next.ready
    next.data := that
    next
  }

  def continueIf(cond: Bool): Handshake[T] = {
    val next = new Handshake(dataType)
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