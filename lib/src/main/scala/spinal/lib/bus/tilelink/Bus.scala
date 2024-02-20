package spinal.lib.bus.tilelink

import spinal.core._
import spinal.core.sim.SimDataPimper
import spinal.lib._
import spinal.lib.bus.bmb.WeakConnector
import spinal.lib.bus.tilelink
import spinal.lib.bus.tilelink.coherent.OrderingCmd
import spinal.lib.bus.tilelink.sim._

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object Opcode extends AreaRoot{
  val A = new SpinalEnum{
    //If you extends that list, don't forget to update the tilelink Decoder
    val PUT_FULL_DATA, PUT_PARTIAL_DATA, GET, ACQUIRE_BLOCK, ACQUIRE_PERM = newElement()
    defaultEncoding = SpinalEnumEncoding("enc")(
      GET -> 4,
      PUT_FULL_DATA -> 0,
      PUT_PARTIAL_DATA -> 1,
      ACQUIRE_BLOCK -> 6,
      ACQUIRE_PERM -> 7
    )
    def isGetPut(c : C) = List(GET, PUT_FULL_DATA, PUT_PARTIAL_DATA).map(c === _).orR
    def isPut(c : C) = List(PUT_FULL_DATA, PUT_PARTIAL_DATA).map(c === _).orR
    def isGet(c : C) = List(GET).map(c === _).orR
    def isAcquire(c : C) = List(ACQUIRE_BLOCK, ACQUIRE_PERM).map(c === _).orR
  }

  val B = new SpinalEnum{
    val PROBE_BLOCK, PROBE_PERM = newElement()
    defaultEncoding = SpinalEnumEncoding("enc")(
      PROBE_BLOCK -> 6,
      PROBE_PERM  -> 7
    )
  }

  val C = new SpinalEnum{
    val PROBE_ACK, PROBE_ACK_DATA, RELEASE, RELEASE_DATA = newElement()
    defaultEncoding = SpinalEnumEncoding("enc")(
      PROBE_ACK     -> 4,
      PROBE_ACK_DATA -> 5,
      RELEASE       -> 6,
      RELEASE_DATA  -> 7
    )

    def withoutData(c: C) = List(PROBE_ACK, RELEASE).map(c === _).orR
    def withData(c: C) =  List(PROBE_ACK_DATA, RELEASE_DATA).map(c === _).orR
    def isProbe(c: C) = List(PROBE_ACK, PROBE_ACK_DATA).map(c === _).orR
    def isRelease(c: C) = List(RELEASE, RELEASE_DATA).map(c === _).orR
  }

  val D = new SpinalEnum{
    val ACCESS_ACK, ACCESS_ACK_DATA, GRANT, GRANT_DATA, RELEASE_ACK = newElement()
    defaultEncoding = SpinalEnumEncoding("enc")(
      ACCESS_ACK      -> 0,
      ACCESS_ACK_DATA -> 1,
      GRANT       -> 4,
      GRANT_DATA  -> 5,
      RELEASE_ACK -> 6
    )
    def fromA(opcode : C) : Bool = List(ACCESS_ACK, ACCESS_ACK_DATA, GRANT, GRANT_DATA).map(opcode === _).orR
    def isFinal(opcode : C) : Bool = List(ACCESS_ACK, ACCESS_ACK_DATA, RELEASE_ACK).map(opcode === _).orR
  }
}

object Param{
  val Cap = new Area {
    val toT = 0
    val toB = 1
    val toN = 2
  }
  val Prune = new Area {
    def fromTo(from : Int, to : Int) = (from, to) match {
      case (Cap.toT, Cap.toB) => TtoB
      case (Cap.toT, Cap.toN) => TtoN
      case (Cap.toB, Cap.toN) => BtoN
    }
    val TtoB = 0
    val TtoN = 1
    val BtoN = 2
  }
  val Report = new Area {
    def fromCap(cap : Int) = cap match {
      case Cap.toT => TtoT
      case Cap.toB => BtoB
      case Cap.toN => NtoN
    }
    val TtoT = 3
    val BtoB = 4
    val NtoN = 5
  }
  val Grow = new Area {
    val NtoB = 0
    val NtoT = 1
    val BtoT = 2

    def apply(withData : Bool, toUnique : Bool) = {
      toUnique mux(
        withData mux(
          B(NtoT, 3 bits),
          B(BtoT, 3 bits)
        ),
        B(NtoB, 3 bits)
      )
    }

    def getCap(grow : Int) = grow match {
      case NtoB => Cap.toB
      case NtoT => Cap.toT
      case BtoT => Cap.toT
    }

    def fromTo(grow : Int) = grow match {
      case NtoB => (Cap.toN, Cap.toB)
      case NtoT => (Cap.toN, Cap.toT)
      case BtoT => (Cap.toB, Cap.toT)
    }
  }
  def reportPruneKeepCopy (param : Bits) = param ===  Prune.TtoB || param === Report.BtoB || param === Report.TtoT
  def reportPruneToCap(param : Int) = param match {
    case Prune.TtoB  => Cap.toB
    case Prune.TtoN  => Cap.toN
    case Prune.BtoN  => Cap.toN
    case Report.TtoT => Cap.toT
    case Report.BtoB => Cap.toB
    case Report.NtoN => Cap.toN
  }
  def reportPruneToCap(from : Int, to : Int) = (from, to) match {
    case (Cap.toT, Cap.toB) => Prune.TtoB
    case (Cap.toT, Cap.toN) => Prune.TtoN
    case (Cap.toB, Cap.toN) => Prune.BtoN
    case (Cap.toT, Cap.toT) => Report.TtoT
    case (Cap.toB, Cap.toB) => Report.BtoB
    case (Cap.toN, Cap.toN) => Report.NtoN
  }
  def reportPruneFromTo(param : Int) = param match {
    case Prune.TtoB  => (Cap.toT, Cap.toB)
    case Prune.TtoN  => (Cap.toT, Cap.toN)
    case Prune.BtoN  => (Cap.toB, Cap.toN)
    case Report.TtoT => (Cap.toT, Cap.toT)
    case Report.BtoB => (Cap.toB, Cap.toB)
    case Report.NtoN => (Cap.toN, Cap.toN)
  }
  def report(fromUnique : Bool, fromShared : Bool,
             toUnique : Bool, toShared : Bool) = {
    (fromUnique ## fromShared ## toUnique ## toShared).muxDc(
      B"1001" -> B(Prune.TtoB, 3 bits),
      B"1000" -> B(Prune.TtoN, 3 bits),
      B"0100" -> B(Prune.BtoN, 3 bits),
      B"1010" -> B(Report.TtoT, 3 bits),
      B"0101" -> B(Report.BtoB, 3 bits),
      B"0000" -> B(Report.NtoN, 3 bits)
    )
  }

}



object ChannelA{
  def apply(node : NodeParameters) : ChannelA = ChannelA(node.toBusParameter())
}
object ChannelB{
  def apply(node : NodeParameters) : ChannelB = ChannelB(node.toBusParameter())
}
object ChannelC{
  def apply(node : NodeParameters) : ChannelC = ChannelC(node.toBusParameter())
}
object ChannelD{
  def apply(node : NodeParameters) : ChannelD = ChannelD(node.toBusParameter())
}
object ChannelE{
  def apply(node : NodeParameters) : ChannelE = ChannelE(node.toBusParameter())
}

case class OrderingTag(cmd : Flow[OrderingCmd], cd : ClockDomain = ClockDomain.current) extends SpinalTag{
  cmd.simPublic()
}

object DebugId{
  class Space(val width : Int){
    var reserved = 0
  }
  val space = new ScopeProperty[Space]{
    override def default = new Space(0)
  }
  def apply() = UInt(space.width bits)
  def width = space.width
  def enabled = width != 0
  def withPostfix(post : UInt) : UInt = {
    if(!enabled) return U(0, 0 bits)
    val size = 1 << widthOf(post)
    val mask = (size)-1
    val base = (space.reserved + mask) & ~mask
    space.reserved = base + size
    U(base, width bits) | post.resized
  }

  def setup(width : Int) = {
    assert(width > 8)
    this.space.set(new Space(width))
  }
}

abstract class BusFragment(val p : BusParameter) extends Bundle {
  def size      : UInt
  def withBeats : Bool
  def data : Bits
  def corrupt : Bool
  def maskNull : Bits = null
  def deniedNull : Bool = null
  def sinkNull : UInt = null
  def addressNull : UInt

  def withAddress : Boolean
  def withData : Boolean
  def withMask : Boolean
  def withDenied : Boolean
  def withSink : Boolean = false

  def sizeToBeatMinusOne() = spinal.lib.bus.tilelink.sizeToBeatMinusOne(p, size)

  def asNoData(): this.type
}

case class ChannelA(override val p : BusParameter) extends BusFragment(p) {
  val opcode  = Opcode.A()
  val param   = Bits(3 bits)
  val source  = p.source()
  val address = p.address()
  val size    = p.size()
  val mask    = p.withDataA generate p.mask()
  val data    = p.withDataA generate p.data()
  val corrupt = p.withDataA generate Bool()
  val debugId = DebugId()

  override def withBeats = p.withDataA.mux(List(Opcode.A.PUT_FULL_DATA(), Opcode.A.PUT_PARTIAL_DATA()).sContains(opcode), False)
  def asNoData() : this.type = p.withDataA match {
    case false => CombInit(this)
    case true => {
      val a = ChannelA(p.copy(withDataA = false))
      a.assignSomeByName(this)
      a
    }.asInstanceOf[this.type]
  }
  override def clone = ChannelA(p)
  override def maskNull = mask
  override def addressNull = address

  def withAddress : Boolean = true
  def withData : Boolean = p.withDataA
  def withMask : Boolean = p.withDataA
  def withDenied : Boolean = false

  def weakAssignFrom(m : ChannelA): Unit ={
    def s = this
    s.opcode := m.opcode
    s.param := m.param
    s.source := m.source
    s.address := m.address
    s.debugId := m.debugId
    WeakConnector(m, s, m.size,    s.size   , defaultValue = () => null, allowUpSize = true , allowDownSize = false, allowDrop = false)
    WeakConnector(m, s, m.mask,    s.mask   , defaultValue = () => cloneOf(s.mask).assignDontCare(), allowUpSize = false , allowDownSize = false, allowDrop = true)
    WeakConnector(m, s, m.data,    s.data   , defaultValue = () => cloneOf(s.data).assignDontCare(), allowUpSize = false , allowDownSize = false, allowDrop = true)
    WeakConnector(m, s, m.corrupt, s.corrupt, defaultValue = () => False, allowUpSize = false , allowDownSize = false, allowDrop = false)
  }
}
case class ChannelB(override val p : BusParameter) extends BusFragment(p) {
  val opcode  = Opcode.B()
  val param   = Bits(3 bits)
  val source  = p.source()
  val address = p.address()
  val size    = p.size()
  val mask    = p.withDataB generate p.mask()
  val data    = p.withDataB generate p.data()
  val corrupt = p.withDataB generate Bool()

  assert(!p.withDataB)
  override def withBeats = p.withDataB.mux(False, False) //TODO
  override def clone = ChannelB(p)
  override def maskNull = mask
  override def addressNull = address
  def withAddress : Boolean = true
  def withData : Boolean = p.withDataB
  def withMask : Boolean = p.withDataB
  def withDenied : Boolean = false

  def asNoData(): this.type = p.withDataB match {
    case false => CombInit(this)
    case true => {
      val a = ChannelB(p.copy(withDataB = false))
      a.assignSomeByName(this)
      a
    }.asInstanceOf[this.type]
  }
}
case class ChannelC(override val p : BusParameter) extends BusFragment(p) {
  val opcode  = Opcode.C()
  val param   = Bits(3 bits)
  val source  = p.source()
  val address = p.address()
  val size    = p.size()
  val data    = p.withDataC generate p.data()
  val corrupt = Bool()

  def isProbeKind()   = opcode === Opcode.C.PROBE_ACK || opcode === Opcode.C.PROBE_ACK_DATA
  def isReleaseKind() = opcode === Opcode.C.RELEASE   || opcode === Opcode.C.RELEASE_DATA
  def isDataKind() = opcode === Opcode.C.PROBE_ACK_DATA   || opcode === Opcode.C.RELEASE_DATA
  override def withBeats = List(Opcode.C.PROBE_ACK_DATA(), Opcode.C.RELEASE_DATA()).sContains(opcode)
  override def clone = ChannelC(p)
  override def addressNull = address

  def withAddress : Boolean = true
  def withData : Boolean = p.withDataC
  def withMask : Boolean = false
  def withDenied : Boolean = false

  def asNoData(): this.type = p.withDataC match {
    case false => CombInit(this)
    case true => {
      val a = ChannelC(p.copy(withDataC = false))
      a.assignSomeByName(this)
      a
    }.asInstanceOf[this.type]
  }
}
case class ChannelD(override val p : BusParameter) extends BusFragment(p) {
  val opcode  = Opcode.D()
  val param   = Bits(3 bits)
  val source  = p.source()
  val sink    = p.sink()
  val size    = p.size()
  val denied  = Bool()
  val data    = p.withDataD generate p.data()
  val corrupt = p.withDataD generate Bool()

  override def withBeats = p.withDataD.mux(List(Opcode.D.ACCESS_ACK_DATA(), Opcode.D.GRANT_DATA()).sContains(opcode), False)
  def withDontCareData(): ChannelD ={
    val ret = ChannelD(p.copy(withDataD = true))
    ret.assignSomeByName(this)
    ret.data.assignDontCare()
    ret.corrupt.assignDontCare()
    ret
  }

  override def deniedNull = denied
  override def clone = ChannelD(p)
  override def addressNull = null
  override def sinkNull = sink

  def withAddress : Boolean = true
  def withData : Boolean = p.withDataD
  def withMask : Boolean = false
  def withDenied : Boolean = true
  override def withSink = true




  def weakAssignFrom(m : ChannelD): Unit ={
    def s = this
    s.opcode := m.opcode
    s.param := m.param
    s.source := m.source
    s.sink := m.sink
    s.denied := m.denied
    WeakConnector(m, s, m.size,    s.size   , defaultValue = null, allowUpSize = true , allowDownSize = true, allowDrop = false)
    WeakConnector(m, s, m.data,    s.data   , defaultValue = () => cloneOf(s.data).assignDontCare(), allowUpSize = false , allowDownSize = false, allowDrop = true)
    WeakConnector(m, s, m.corrupt, s.corrupt, defaultValue = () => False, allowUpSize = false , allowDownSize = false, allowDrop = true)
  }

  def asNoData(): this.type = p.withDataD match {
    case false => CombInit(this)
    case true => {
      val a = ChannelD(p.copy(withDataD = false))
      a.assignSomeByName(this)
      a
    }.asInstanceOf[this.type]
  }
}
case class ChannelE(p : BusParameter) extends Bundle {
  val sink    = p.sink()
}

object Bus{
  def  apply(p : NodeParameters) : Bus = Bus(p.toBusParameter())
  def  apply(p : M2sParameters) : Bus = Bus(p.toNodeParameters().toBusParameter())
}

case class Bus(p : BusParameter) extends Bundle with IMasterSlave{
  val a = Stream(ChannelA(p))
  val b = p.withBCE generate Stream(ChannelB(p))
  val c = p.withBCE generate Stream(ChannelC(p))
  val d = Stream(ChannelD(p))
  val e = p.withBCE generate Stream(ChannelE(p))

  override def asMaster() = {
    master(a, c, e)
    slave(b, d)
  }

  def <<(m : Bus): Unit ={
    a << m.a
    d >> m.d
    if(p.withBCE){
      b >> m.b
      c << m.c
      e << m.e
    }
  }
  def >>(s : Bus): Unit = s << this

  def connectFrom(m : Bus)(
    a: StreamPipe = StreamPipe.NONE,
    b: StreamPipe = StreamPipe.NONE,
    c: StreamPipe = StreamPipe.NONE,
    d: StreamPipe = StreamPipe.NONE,
    e: StreamPipe = StreamPipe.NONE
  ) : Unit = {
    val s = this
    s.a << a(m.a)
    m.d << d(s.d)
    if(p.withBCE){
      m.b << b(s.b)
      s.c << c(m.c)
      s.e << e(m.e)
    }
  }

  def combStage() : Bus = {
    val ret = Bus(p)
    ret << this
    ret
  }
  def fromCombStage() : Bus = {
    val ret = Bus(p)
    ret >> this
    ret
  }

  def withSourceOffset(offset : Int, width: Int): Bus ={
    val ret = Bus(p.copy(sourceWidth = width))
    ret << this
    ret.a.source.removeAssignments() := (this.a.source.resize(width) | offset)
    this.d.source.removeAssignments() := ret.d.source.resized
    if(p.withBCE){
      this.b.source.removeAssignments() := ret.b.source.resized
      ret.c.source.removeAssignments() := (this.c.source.resize(width) | offset)
    }
    ret
  }
  def fromSinkOffset(offset : Int, width: Int): Bus ={
    val ret = Bus(p.copy(sinkWidth = width))
    ret >> this
    if(p.withBCE){
      ret.d.sink.removeAssignments() := (this.d.sink.resize(width) | offset)
      this.e.sink.removeAssignments() := ret.e.sink.resized
    } else {
      ret.d.sink.removeAssignments() := 0
    }
    ret
  }
}
