package spinal.lib

import spinal.core._


class HandshakeFactory extends MSFactory {
  object Fragment extends HandshakeFragmentFactory

  def apply[T <: Data](dataType: T) = {
    val ret = new Handshake(dataType)
    postApply(ret)
    ret
  }
}
object Handshake extends HandshakeFactory


class EventFactory extends MSFactory {
  def apply = {
    val ret = new Handshake(new NoData)
    postApply(ret)
    ret
  }
}



class Handshake[T <: Data](_dataType: T) extends Bundle with Interface with DataCarrier[T] {
  val valid = Bool
  val ready = Bool
  val data: T = _dataType.clone()


  def dataType = _dataType
  override def clone: this.type = Handshake(_dataType).asInstanceOf[this.type]

  override def asMaster: this.type = {
    valid.asOutput
    ready.asInput
    data.asOutput
    this
  }

  override def asSlave: this.type = asMaster.flip

  override def freeRun: Unit = ready := True

  def toFlow: Flow[T] = {
    freeRun
    val ret = Flow(_dataType)
    ret.valid := this.valid
    ret.data := this.data
    ret
  }

  def <<(that: Handshake[T]): Handshake[T] = connectFrom(that)
  //def <<[T2 <: Data](that : Handshake[T2])(dataAssignement : (T,T2) => Unit): Handshake[T2]  = connectFrom(that)(dataAssignement)

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

  def &(cond: Bool): Handshake[T] = continueWhen(cond)
  def ~[T2 <: Data](that: T2): Handshake[T2] = translateWith(that)
  def ~~[T2 <: Data](translate: (T) => T2): Handshake[T2] = {
    (this ~ translate(this.data))
  }


  def queue(size : Int) : Handshake[T] ={
    val fifo = new HandshakeFifo(dataType,size)
    fifo.io.push << this
    return fifo.io.pop
  }

  override def fire: Bool = valid & ready
  def isFree: Bool = !valid || ready
  def connectFrom(that: Handshake[T]): Handshake[T] = {
    this.valid := that.valid
    that.ready := this.ready
    this.data := that.data
    that
  }

  //TODO better name
  def translateFrom[T2 <: Data](that: Handshake[T2])(dataAssignement: (T, that.data.type) => Unit): Handshake[T2] = {
    this.valid := that.valid
    that.ready := this.ready
    dataAssignement(this.data, that.data)
    that
  }


  def m2sPipe: Handshake[T] = m2sPipe(false)


  def m2sPipe(crossClockData: Boolean): Handshake[T] = {
    val ret = Handshake(_dataType)

    val rValid = RegInit(False)
    val rData = Reg(_dataType)
    if (crossClockData) rData.addTag(crossClockDomain)

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
    val ret = Handshake(_dataType)

    val rValid = RegInit(False)
    val rBits = Reg(_dataType)

    ret.valid := this.valid || rValid
    this.ready := !rValid
    ret.data := Mux(rValid, rBits, this.data)

    when(ret.ready) {
      rValid := False
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

  def continueWhen(cond: Bool): Handshake[T] = {
    val next = new Handshake(_dataType)
    next.valid := this.valid && cond
    this.ready := next.ready && cond
    next.data := this.data
    return next
  }

  def throwWhen(cond: Bool): Handshake[T] = {
    val next = this.clone

    next connectFrom this
    when(cond) {
      next.valid := False
      this.ready := True
    }
    next
  }

  def haltWhen(cond: Bool): Handshake[T] = continueWhen(!cond)
  def takeWhen(cond: Bool): Handshake[T] = throwWhen(!cond)


  def fragmentTransaction(bitsWidth: Int): Handshake[Fragment[Bits]] = {
    val converter = new HandshakeToHandshakeFragmentBits(data, bitsWidth)
    converter.io.input << this
    return converter.io.output
  }
}



class HandshakeArbiterCoreIO[T <: Data](dataType: T, portCount: Int) extends Bundle {
  val inputs = Vec(portCount, slave Handshake (dataType))
  val output = master Handshake (dataType)
  val chosen = out UInt (log2Up(portCount) bit)
}

class HandshakeArbiterCore[T <: Data](dataType: T,val portCount: Int)(arbitrationLogic : (HandshakeArbiterCore[T]) => Area, lockLogic : (HandshakeArbiterCore[T]) => Area) extends Component {
  val io = new HandshakeArbiterCoreIO(dataType, portCount)

  val locked = RegInit(False)

  val maskProposal = Vec(portCount, Bool)
  val maskLocked = Reg(Vec(portCount, Bool))
  val maskRouted = Mux(locked, maskLocked, maskProposal)


  when(io.output.valid) {
    maskLocked := maskRouted
  }

  val arbitration = arbitrationLogic(this)
  val lock = lockLogic(this)

  //Route
  var outputValid = False
  var outputData = B(0)
  for ((input, mask) <- (io.inputs, maskRouted).zipped) {
    outputValid = outputValid | input.valid
    outputData = outputData | Mux(mask, input.data.toBits, B(0))
    input.ready := mask & io.output.ready
  }
  io.output.valid := outputValid
  io.output.data.assignFromBits(outputData)

  io.chosen := OHToUInt(maskRouted)
  
}

object HandshakeArbiterCore{
  def arbitration_lowIdPortFirst[T <: Data](core : HandshakeArbiterCore[T]) = new Area{
    import core._
    var search = True
    for (i <- 0 to portCount - 2) {
      maskProposal(i) := search & io.inputs(i).valid
      search = search & !io.inputs(i).valid
    }
    maskProposal(portCount - 1) := search
  }

  def lock_none[T <: Data](core : HandshakeArbiterCore[T]) = new Area {
    import core._

  }

  def lock_transactionLock[T <: Data](core : HandshakeArbiterCore[T]) = new Area {
    import core._
    when(io.output.valid) {
      locked := True
    }
    when(io.output.ready) {
      locked := False
    }
  }

  def lock_fragmentLock[T <: Data](core : HandshakeArbiterCore[Fragment[T]]) = new Area {
    import core._

    when(io.output.valid) {
      locked := True
    }
    when(io.output.ready && io.output.last) {
      locked := False
    }
  }
}

//TODOTEST
//class HandshakeArbiterPriorityImpl[T <: Data](dataType: T, portCount: Int, allowSwitchWithoutConsumption: Boolean = false) extends HandshakeArbiterCore(dataType, portCount, allowSwitchWithoutConsumption) {
//  var search = True
//  for (i <- 0 to portCount - 2) {
//    maskProposal(i) := search & io.inputs(i).valid
//    search = search & !io.inputs(i).valid
//  }
//  maskProposal(portCount - 1) := search
//}


//TODOTEST
class HandshakeFork[T <: Data](dataType: T, portCount: Int) extends Component {
  val io = new Bundle {
    val in = slave Handshake (dataType)
    val out = Vec(portCount, master Handshake (dataType))
  }
  val linkEnable = Vec(portCount, RegInit(True))

  io.in.ready := True
  for (i <- (0 to portCount - 1)) {
    when(!io.out(i).ready && linkEnable(i)) {
      io.in.ready := False
    }
  }

  for (i <- (0 to portCount - 1)) {
    io.out(i).valid := io.in.valid && linkEnable(i)
    io.out(i).data := io.in.data
    when(io.out(i).fire) {
      linkEnable(i) := False
    }
  }

  when(io.in.ready) {
    linkEnable.map(_ := True)
  }
}

//TODOTEST
class HandshakeDemux[T <: Data](dataType: T, portCount: Int) extends Component {
  val io = new Bundle {
    val sel = in UInt (log2Up(portCount) bit)
    val input = slave Handshake (dataType)
    val output = Vec(portCount, master Handshake (dataType))
  }
  io.input.ready := False

  for (i <- 0 to portCount - 1) {
    io.output(i).data := io.input.data
    when(U(i) !== io.sel) {
      io.output(i).valid := False
    } otherwise {
      io.output(i).valid := io.input.valid
      io.input.ready := io.output(i).ready
    }
  }
}

class HandshakeFifoIo[T <: Data](dataType: T, depth: Int) extends Bundle {
  val push = slave Handshake (dataType)
  val pop = master Handshake (dataType)
  val occupancy = out UInt (log2Up(depth + 1) bit)
}

class HandshakeFifo[T <: Data](dataType: T, depth: Int) extends Component {
  val io = new HandshakeFifoIo(dataType, depth)

  val ram = Mem(dataType, depth)

  val pushPtr = Counter(depth)
  val popPtr = Counter(depth)
  val ptrMatch = pushPtr === popPtr
  val risingOccupancy = RegInit(False)
  val pushing = io.push.fire
  val popping = io.pop.fire
  val empty = ptrMatch & !risingOccupancy
  val full = ptrMatch & risingOccupancy

  io.push.ready := !full
  io.pop.valid := !empty & !(RegNext(popPtr.valueNext === pushPtr, False) & !full) //mem write to read propagation
  io.pop.data := ram.readSync(popPtr.valueNext)

  when(pushing !== popping) {
    risingOccupancy := pushing
  }
  when(pushing) {
    ram(pushPtr.value) := io.push.data
    pushPtr ++
  }
  when(popping) {
    popPtr ++
  }

  val ptrDif = pushPtr - popPtr
  if (isPow2(depth))
    io.occupancy := ((risingOccupancy && ptrMatch) ## ptrDif).toUInt
  else {
    when(ptrMatch) {
      io.occupancy := Mux(risingOccupancy, U(depth), U(0))
    } otherwise {
      io.occupancy := Mux(pushPtr > popPtr, ptrDif, U(depth) + ptrDif)
    }
  }
}


class HandshakeFifoCCIo[T <: Data](dataType: T, depth: Int) extends Bundle {
  val push = slave Handshake (dataType)
  val pop = master Handshake (dataType)
  val pushOccupancy = out UInt (log2Up(depth) + 1 bit)
  val popOccupancy = out UInt (log2Up(depth) + 1 bit)
}

class HandshakeFifoCC[T <: Data](dataType: T, depth: Int, pushClockDomain: ClockDomain, popClockDomain: ClockDomain) extends Component {
  assert(isPow2(depth))
  assert(depth >= 2)

  val io = new HandshakeFifoCCIo(dataType, depth)

  val ptrWidth = log2Up(depth) + 1
  def isFull(a: Bits, b: Bits) = a(ptrWidth - 1, ptrWidth - 2) === ~b(ptrWidth - 1, ptrWidth - 2) && a(ptrWidth - 3, 0) === b(ptrWidth - 3, 0)
  def isEmpty(a: Bits, b: Bits) = a === b

  val ram = Mem(dataType, depth)

  val popToPushGray = Bits(ptrWidth bit)
  val pushToPopGray = Bits(ptrWidth bit)

  val pushCC = new ClockingArea(pushClockDomain) {
    val pushPtr = Counter(depth << 1)
    val pushPtrGray = RegNext(toGray(pushPtr.valueNext))
    val popPtrGray = BufferCC(popToPushGray, B"0")
    val full = isFull(pushPtrGray, popPtrGray)

    io.push.ready := !full
    when(io.push.fire) {
      ram(pushPtr) := io.push.data
      pushPtr ++
    }

    io.pushOccupancy := pushPtr - fromGray(popPtrGray)
  }

  val popCC = new ClockingArea(popClockDomain) {
    val popPtr = Counter(depth << 1)
    val popPtrGray = RegNext(toGray(popPtr.valueNext))
    val pushPtrGray = BufferCC(pushToPopGray, B"0")
    val empty = isEmpty(popPtrGray, pushPtrGray)

    io.pop.valid := !empty
    io.pop.data := ram.readSyncCC(popPtr.valueNext)
    when(io.pop.fire) {
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

  val outHitSignal = Bool

  val inputArea = new ClockingArea(clockIn) {
    val hit = BufferCC(outHitSignal, False)
    val target = RegInit(False)
    val data = Reg(io.input.data)
    io.input.ready := False
    when(io.input.valid && hit === target) {
      target := !target
      data := io.input.data
      io.input.ready := True
    }
  }


  val outputArea = new ClockingArea(clockOut) {
    val target = BufferCC(inputArea.target, False)
    val hit = RegInit(False)
    outHitSignal := hit

    val handshake = io.input.clone
    handshake.valid := (target !== hit)
    handshake.data := inputArea.data
    handshake.data.addTag(crossClockDomain)

    when(handshake.fire) {
      hit := !hit
    }

    io.output << handshake.m2sPipe
  }
}


