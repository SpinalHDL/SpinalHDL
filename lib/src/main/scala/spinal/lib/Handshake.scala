package spinal.lib

import spinal.core
import spinal.core._

object Handshake {
  // implicit def autoCast[T2 <: Data, T <: T2](that: Handshake[T]): Handshake[T2] = that.asInstanceOf[Handshake[T2]]


  def apply[T <: Data](dataType: T) = new Handshake(dataType)


}

class Handshake[T <: Data](dataType: T) extends Bundle with Interface {
  val valid = Bool()
  val ready = core.Bool()
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
  def >>(into: Handshake[T]): Handshake[T] = {
    into << this;
    into
  }

  def <-<(that: Handshake[T]): Handshake[T] = {
    this << that.m2sPipe
    that
  }
  def >->(into: Handshake[T]): Handshake[T] = {
    into <-< this;
    into
  }

  def </<(that: Handshake[T]): Handshake[T] = {
    this << that.s2mPipe
    that
  }

  def <-/<(that: Handshake[T]): Handshake[T] = {
    this << that.s2mPipe.m2sPipe
    that
  }
  def >/->(into: Handshake[T]): Handshake[T] = {
    into <-/< this;
    into
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


  def m2sPipe(crossClockData: Boolean): Handshake[T] = {
    val ret = Handshake(dataType)

    val rValid = core.RegInit(core.Bool(false))
    val rData = Reg(dataType)
    if (crossClockData) rData.addTag(core.crossClockDomain)

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

    val rValid = core.RegInit(core.Bool(false))
    val rBits = core.Reg(dataType)

    ret.valid := this.valid || rValid
    this.ready := !rValid
    ret.data := core.Mux(rValid, rBits, this.data)

    core.when(ret.ready) {
      rValid := core.Bool(false)
    }

    core.when(this.ready && (!ret.ready)) {
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
    core.when(cond) {
      next.valid := core.Bool(false)
      this.ready := core.Bool(true)
    }
    next
  }

  def haltIf(cond: Bool): Handshake[T] = continueIf(!cond)
  def takeIf(cond: Bool): Handshake[T] = throwIf(!cond)
}


class HandshakeArbiterCoreIO[T <: Data](dataType: T, portCount: Int) extends Bundle {
  val inputs = Vec(portCount, slave Handshake (dataType))
  val output = master Handshake (dataType)
  val chosen = core.out UInt (core.log2Up(portCount) bit)
}

abstract class HandshakeArbiterCore[T <: Data](dataType: T, portCount: Int, allowSwitchWithoutConsumption: Boolean) extends Component {
  val io = new HandshakeArbiterCoreIO(dataType, portCount)

  val locked = core.RegInit(core.Bool(false))

  val maskProposal = core.Vec(portCount, core.Bool())
  val maskLocked = core.Reg(core.Vec(portCount, core.Bool()))
  val maskRouted = if (allowSwitchWithoutConsumption) maskProposal else core.Mux(locked, maskLocked, maskProposal)


  core.when(io.output.valid) {
    //Lock
    locked := core.Bool(true)
    maskLocked := maskRouted
  }
  core.when(io.output.ready) {
    //unlock
    locked := core.Bool(false)
  }


  //Route
  var outputValid = core.Bool(false)
  var outputData = core.Bits(0)
  for ((input, mask) <- (io.inputs, maskRouted).zipped) {
    outputValid = outputValid | input.valid
    outputData = outputData | core.Mux(mask, input.data.toBits, core.Bits(0))
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
  var search = core.Bool(true)
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
    val out = core.Vec(portCount, master Handshake (dataType))
  }
  val linkEnable = core.Vec(portCount, core.RegInit(core.Bool(true)))

  io.in.ready := core.Bool(true)
  for (i <- (0 to portCount - 1)) {
    core.when(!io.out(i).ready && linkEnable(i)) {
      io.in.ready := core.Bool(false)
    }
  }

  for (i <- (0 to portCount - 1)) {
    io.out(i).valid := io.in.valid && linkEnable(i)
    io.out(i).data := io.in.data
    core.when(io.out(i).fire) {
      linkEnable(i) := core.Bool(false)
    }
  }

  core.when(io.in.ready) {
    linkEnable.map(_ := core.Bool(true))
  }
}

//TODOTEST
class HandshakeDemux[T <: Data](dataType: T, portCount: Int) extends Component {
  val io = new Bundle {
    val sel = core.in UInt (core.log2Up(portCount) bit)
    val input = slave Handshake (dataType)
    val output = core.Vec(portCount, master Handshake (dataType))
  }
  io.input.ready := core.Bool(false)

  for (i <- 0 to portCount - 1) {
    io.output(i).data := io.input.data
    core.when(core.UInt(i) !== io.sel) {
      io.output(i).valid := core.Bool(false)
    } otherwise {
      io.output(i).valid := io.input.valid
      io.input.ready := io.output(i).ready
    }
  }
}

class HandshakeFifoIo[T <: Data](dataType: T, depth: Int) extends Bundle {
  val push = slave Handshake (dataType)
  val pop = master Handshake (dataType)
  val occupancy = core.out UInt (core.log2Up(depth + 1) bit)
}

class HandshakeFifo[T <: Data](dataType: T, depth: Int) extends Component {
  val io = new HandshakeFifoIo(dataType, depth)

  val ram = core.Mem(dataType, depth)

  val pushPtr = Counter(depth)
  val popPtr = Counter(depth)
  val ptrMatch = pushPtr === popPtr
  val risingOccupancy = core.RegInit(core.Bool(false))
  val pushing = io.push.fire
  val popping = io.pop.fire
  val empty = ptrMatch & !risingOccupancy
  val full = ptrMatch & risingOccupancy

  io.push.ready := !full
  io.pop.valid := !empty & !(RegNext(popPtr.valueNext === pushPtr, core.Bool(false)) & !full) //mem write to read propagation
  io.pop.data := ram.readSync(popPtr.valueNext)

  core.when(pushing !== popping) {
    risingOccupancy := pushing
  }
  core.when(pushing) {
    ram(pushPtr.value) := io.push.data
    pushPtr ++
  }
  core.when(popping) {
    popPtr ++
  }

  val ptrDif = pushPtr - popPtr
  if (core.isPow2(depth))
    io.occupancy := ((risingOccupancy && ptrMatch) ## ptrDif).toUInt
  else {
    core.when(ptrMatch) {
      io.occupancy := core.Mux(risingOccupancy, core.UInt(depth), core.UInt(0))
    } otherwise {
      io.occupancy := core.Mux(pushPtr > popPtr, ptrDif, core.UInt(depth) + ptrDif)
    }
  }
}


class HandshakeFifoCCIo[T <: Data](dataType: T, depth: Int) extends Bundle {
  val push = slave Handshake (dataType)
  val pop = master Handshake (dataType)
  val pushOccupancy = core.out UInt (core.log2Up(depth + 1) bit)
  val popOccupancy = core.out UInt (core.log2Up(depth + 1) bit)
}

class HandshakeFifoCC[T <: Data](dataType: T, depth: Int, pushClockDomain: ClockDomain, popClockDomain: ClockDomain) extends Component {
  assert(core.isPow2(depth))
  assert(depth >= 2)

  val io = new HandshakeFifoCCIo(dataType, depth)

  val ptrWidth = core.log2Up(depth)+1
  def isFull(a: Bits, b: Bits) = a(ptrWidth - 1, ptrWidth - 2) === ~b(ptrWidth - 1, ptrWidth - 2) && a(ptrWidth - 3, 0) === b(ptrWidth - 3, 0)
  def isEmpty(a: Bits, b: Bits) = a === b

  val ram = core.Mem(dataType, depth)

  val popToPushGray = core.Bits(ptrWidth bit)
  val pushToPopGray = core.Bits(ptrWidth bit)

  val pushCC = new ClockingArea(pushClockDomain) {
    val pushPtr = Counter(depth << 1)
    val pushPtrGray = core.RegNext(toGray(pushPtr.valueNext))
    val popPtrGray = BufferCC(popToPushGray,core.Bits(0))
    val full = isFull(pushPtrGray, popPtrGray)

    io.push.ready := !full
    core.when(io.push.fire) {
      ram(pushPtr) := io.push.data
      pushPtr ++
    }

    io.pushOccupancy := pushPtr - fromGray(popPtrGray)
  }

  val popCC = new ClockingArea(popClockDomain) {
    val popPtr = Counter(depth << 1)
    val popPtrGray = core.RegNext(toGray(popPtr.valueNext))
    val pushPtrGray = BufferCC(pushToPopGray,core.Bits(0))
    val empty = isEmpty(popPtrGray, pushPtrGray)

    io.pop.valid := !empty
    io.pop.data := ram.readSyncCC(popPtr.valueNext)
    core.when(io.pop.fire) {
      popPtr ++
    }

    io.popOccupancy := fromGray(pushPtrGray) - popPtr
  }

  pushToPopGray := pushCC.pushPtrGray
  popToPushGray := popCC.popPtrGray
}

object HandshakeCCByToggle {
  def apply[T <: Data](input: Handshake[T], clockIn: ClockDomain, clockOut: ClockDomain): Handshake[T] = {
    val c = new HandshakeCCByToggle[T](input.data, clockIn, clockOut)
    c.io.input connectFrom input
    return c.io.output
  }
}


class HandshakeCCByToggle[T <: Data](dataType: T, clockIn: ClockDomain, clockOut: ClockDomain) extends Component {
  val io = new Bundle {
    val input = slave Handshake (dataType)
    val output = master Handshake (dataType)
  }

  val outHitSignal = core.Bool()

  val inputArea = new ClockingArea(clockIn) {
    val hit = BufferCC(outHitSignal, core.Bool(false))
    val target = core.RegInit(core.Bool(false))
    val data = core.Reg(io.input.data)
    io.input.ready := core.Bool(false)
    core.when(io.input.valid && hit === target) {
      target := !target
      data := io.input.data
      io.input.ready := core.Bool(true)
    }
  }


  val outputArea = new ClockingArea(clockOut) {
    val target = BufferCC(inputArea.target, core.Bool(false))
    val hit = core.RegInit(core.Bool(false))
    outHitSignal := hit

    val handshake = io.input.clone
    handshake.valid := (target !== hit)
    handshake.data := inputArea.data
    core.when(handshake.fire) {
      hit := !hit
    }

    io.output << handshake.m2sPipe(true)
  }
}


