package spinal.lib.bus.tilelink

import spinal.core._
import spinal.lib._
import spinal.lib.bus.misc.{AddressMapping, SizeMapping}
import spinal.lib.system.tag._
import scala.collection.Seq


case class M2sTransfers(acquireT     : SizeRange = SizeRange.none,
                        acquireB     : SizeRange = SizeRange.none,
                        arithmetic   : SizeRange = SizeRange.none,
                        logical      : SizeRange = SizeRange.none,
                        get          : SizeRange = SizeRange.none,
                        putFull      : SizeRange = SizeRange.none,
                        putPartial   : SizeRange = SizeRange.none,
                        hint         : SizeRange = SizeRange.none) extends MemoryTransfers {

  def isOnlyGetPut() = List(acquireT, acquireB, arithmetic, logical, hint).forall(_.none)

  def allowA(opcode : Opcode.A.E)  : Boolean = opcode match {
    case Opcode.A.PUT_FULL_DATA => putFull.some
    case Opcode.A.PUT_PARTIAL_DATA => putPartial.some
    case Opcode.A.GET => get.some
    case Opcode.A.ACQUIRE_BLOCK => withBCE
    case Opcode.A.ACQUIRE_PERM  => withBCE
  }
  def allowC(opcode : Opcode.C.E)  : Boolean = withBCE
  def allow(opcode : Any) : Boolean = opcode match{
    case Opcode.A.PUT_FULL_DATA => putFull.some
    case Opcode.A.PUT_PARTIAL_DATA => putPartial.some
    case Opcode.A.GET => get.some
    case Opcode.A.ACQUIRE_BLOCK => withBCE
    case Opcode.A.ACQUIRE_PERM  => withBCE
    case Opcode.C.PROBE_ACK | Opcode.C.PROBE_ACK_DATA | Opcode.C.RELEASE | Opcode.C.RELEASE_DATA => withBCE
  }
//  def foreachWithOpcodeA(body : (Int, SizeRange) => Unit): Unit ={
//    body(0, putFull   )
//    body(1, putPartial)
//    body(2, arithmetic)
//    body(3, logical   )
//    body(4, get       )
//    body(5, hint      )
//    body(6, acquireT  )
//    body(7, acquireB  )
//  }
  def withBCE = acquireT.some || acquireB.some
  def withDataA = putFull.some || putPartial.some
  def withDataD = get.some || acquireT.some || acquireB.some || logical.some || arithmetic.some
  def withAny = withDataA || withDataD || withBCE || hint.some

  def intersect(rhs: M2sTransfers) = M2sTransfers(
    acquireT     = acquireT    .intersect(rhs.acquireT),
    acquireB     = acquireB    .intersect(rhs.acquireB),
    arithmetic   = arithmetic  .intersect(rhs.arithmetic),
    logical      = logical     .intersect(rhs.logical),
    get          = get         .intersect(rhs.get),
    putFull      = putFull     .intersect(rhs.putFull),
    putPartial   = putPartial  .intersect(rhs.putPartial),
    hint         = hint        .intersect(rhs.hint))
  def mincover(rhs: M2sTransfers) = M2sTransfers(
    acquireT     = acquireT    .mincover(rhs.acquireT),
    acquireB     = acquireB    .mincover(rhs.acquireB),
    arithmetic   = arithmetic  .mincover(rhs.arithmetic),
    logical      = logical     .mincover(rhs.logical),
    get          = get         .mincover(rhs.get),
    putFull      = putFull     .mincover(rhs.putFull),
    putPartial   = putPartial  .mincover(rhs.putPartial),
    hint         = hint        .mincover(rhs.hint))


  override def mincover(rhs: MemoryTransfers) = rhs match {
    case rhs : M2sTransfers => mincover(rhs)
  }

  override def intersect(rhs: MemoryTransfers) = rhs match {
    case rhs : M2sTransfers => intersect(rhs)
  }

  def isEmpty : Boolean = !nonEmpty
  def nonEmpty : Boolean = withAny

  // Reduce rendering to a simple yes/no per field
  override def toString = {
    def str(x: SizeRange, flag: String) = if (x.none) "" else flag
    def flags = Vector(
      str(acquireT,   "T"),
      str(acquireB,   "B"),
      str(arithmetic, "A"),
      str(logical,    "L"),
      str(get,        "G"),
      str(putFull,    "F"),
      str(putPartial, "P"),
      str(hint,       "H"))
    flags.mkString
  }
  // Prints out the actual information in a user readable way
  def infoString = {
    s"""acquireT = ${acquireT}
       |acquireB = ${acquireB}
       |arithmetic = ${arithmetic}
       |logical = ${logical}
       |get = ${get}
       |putFull = ${putFull}
       |putPartial = ${putPartial}
       |hint = ${hint}
       |
       |""".stripMargin
  }
  val sizeBytes = List(
    acquireT.max,
    acquireB.max,
    arithmetic.max,
    logical.max,
    get.max,
    putFull.max,
    putPartial.max
  ).max

  def contains(opcode : Opcode.A.C) : Bool = {
    opcode.mux(
      Opcode.A.GET              -> Bool(get.some),
      Opcode.A.PUT_FULL_DATA    -> Bool(putFull.some),
      Opcode.A.PUT_PARTIAL_DATA -> Bool(putPartial.some),
      Opcode.A.ACQUIRE_BLOCK    -> Bool(acquireB.some || acquireT.some),
      Opcode.A.ACQUIRE_PERM     -> Bool(acquireB.some || acquireT.some)
    )
  }
}

object M2sTransfers {
  def all = M2sTransfers(
    acquireT   = SizeRange(1, 4096),
    acquireB   = SizeRange(1, 4096),
    arithmetic = SizeRange(1, 4096),
    logical    = SizeRange(1, 4096),
    get        = SizeRange(1, 4096),
    putFull    = SizeRange(1, 4096),
    putPartial = SizeRange(1, 4096),
    hint       = SizeRange(1, 4096)
  )

  def singleSize(size : Int) = M2sTransfers(
    acquireT   = SizeRange(size),
    acquireB   = SizeRange(size),
    arithmetic = SizeRange(size),
    logical    = SizeRange(size),
    get        = SizeRange(size),
    putFull    = SizeRange(size),
    putPartial = SizeRange(size),
    hint       = SizeRange(size)
  )

  def allGetPut = M2sTransfers(
    get        = SizeRange(1, 4096),
    putFull    = SizeRange(1, 4096),
    putPartial = SizeRange(1, 4096)
  )

  def intersect(values : Seq[M2sTransfers]) : M2sTransfers = values.reduce(_ intersect _)
  def mincover(values : Seq[M2sTransfers]) : M2sTransfers = values.reduce(_ mincover _)
}



case class M2sSource(id           : AddressMapping,
                     emits        : M2sTransfers){
  def withSourceOffset(offset : Int) = copy(id = id.withOffset(offset))
  def bSourceId = id.lowerBound.toInt
}

object M2sAgent{
  def apply(name    : Nameable,
            mapping : M2sSource) : M2sAgent = M2sAgent(
    name = name,
    mapping = List(mapping)
  )
}

case class M2sAgent(name    : Nameable,
                    mapping : Seq[M2sSource]) extends OverridedEqualsHashCode {
  def withSourceOffset(offset : Int): M2sAgent ={
    copy(mapping = mapping.map(_.withSourceOffset(offset)))
  }
  val emits = M2sTransfers.mincover(mapping.map(_.emits))
  val sourceWidth = mapping.map(_.id.width).max
  def bSourceId = mapping.head.bSourceId
  def sourceHit(source : UInt) = mapping.map(_.id.hit(source)).orR
  def sourceHit(source : Int) = mapping.exists(_.id.hit(source))
  def remapSources(f : M2sSource => M2sSource) = {
    copy(mapping = mapping.map(f))
  }
}


object M2sParameters{
  def apply(support : M2sSupport, sourceCount : Int) : M2sParameters = apply(support, sourceCount, null)
  def apply(support: M2sSupport, sourceCount: Int, name : Nameable): M2sParameters = M2sParameters(
    addressWidth = support.addressWidth,
    dataWidth = support.dataWidth,
    masters = List(M2sAgent(
      name = name,
      mapping = List(M2sSource(
        id = SizeMapping(0, sourceCount),
        emits = support.transfers
      ))
    ))
  )
}

case class M2sParameters(addressWidth : Int,
                         dataWidth : Int,
                         masters   : Seq[M2sAgent]) extends OverridedEqualsHashCode {
  val sizeBytes = masters.map(_.emits.sizeBytes).max
  val sourceWidth = masters.map(_.sourceWidth).max
  val withBCE = masters.map(_.emits.withBCE).reduce(_ || _)
  val emits = M2sTransfers.mincover(masters.map(_.emits))
  def dataBytes = dataWidth / 8
  def withDataA = masters.map(_.emits.withDataA).reduce(_ || _)
  def withDataD = masters.map(_.emits.withDataD).reduce(_ || _)
  def sourceHit(source : UInt) = masters.map(_.sourceHit(source)).orR
  def getMasterFromSource(source : Int) = masters.find(_.mapping.exists(_.id.hit(source))) match {
    case Some(x) => x
    case None => null
  }
  def remapSources(f : M2sSource => M2sSource) = {
    copy(masters = masters.map(_.remapSources(f)))
  }
  def toSupport() = new M2sSupport(
    transfers    = emits,
    addressWidth = addressWidth,
    dataWidth    = dataWidth
  )

  def toNodeParameters() = NodeParameters(this)
}


object M2sSupport{
  def apply(p : M2sParameters) : M2sSupport = M2sSupport(
    transfers    = p.emits,
    addressWidth = p.addressWidth,
    dataWidth    = p.dataWidth
  )
}

case class M2sSupport(transfers : M2sTransfers,
                      addressWidth : Int,
                      dataWidth : Int) {
  def mincover(that : M2sSupport): M2sSupport ={
    M2sSupport(
      transfers = transfers.mincover(that.transfers),
      dataWidth = dataWidth max that.dataWidth,
      addressWidth = addressWidth max that.addressWidth
    )
  }

  def join(p: M2sParameters): M2sParameters ={
    M2sParameters(
      addressWidth = addressWidth,
      dataWidth    = dataWidth,
      masters = p.masters.map(e => e) //TODO
    )
  }

  def intersect(that : M2sTransfers) : M2sSupport = copy(transfers = transfers.intersect(that))

  def withAddressWidth(w: Int): M2sSupport = copy(addressWidth = w)
  def dataBytes = dataWidth/8
}


