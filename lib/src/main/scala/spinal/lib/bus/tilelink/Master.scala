package spinal.lib.bus.tilelink

import spinal.core._
import spinal.lib._
import spinal.lib.bus.misc.AddressMapping


case class M2sTransfers(acquireT     : SizeRange = SizeRange.none,
                        acquireB     : SizeRange = SizeRange.none,
                        arithmetic   : SizeRange = SizeRange.none,
                        logical      : SizeRange = SizeRange.none,
                        get          : SizeRange = SizeRange.none,
                        putFull      : SizeRange = SizeRange.none,
                        putPartial   : SizeRange = SizeRange.none,
                        hint         : SizeRange = SizeRange.none,
                        probeAckData : SizeRange = SizeRange.none){

  def withBCE = acquireT.some || acquireB.some
  def withDataA = putFull.some
  def withDataD = get.some || acquireT.some || acquireB.some || logical.some || arithmetic.some

  def intersect(rhs: M2sTransfers) = M2sTransfers(
    acquireT     = acquireT    .intersect(rhs.acquireT),
    acquireB     = acquireB    .intersect(rhs.acquireB),
    arithmetic   = arithmetic  .intersect(rhs.arithmetic),
    logical      = logical     .intersect(rhs.logical),
    get          = get         .intersect(rhs.get),
    putFull      = putFull     .intersect(rhs.putFull),
    putPartial   = putPartial  .intersect(rhs.putPartial),
    hint         = hint        .intersect(rhs.hint),
    probeAckData = probeAckData.intersect(rhs.probeAckData))
  def mincover(rhs: M2sTransfers) = M2sTransfers(
    acquireT     = acquireT    .mincover(rhs.acquireT),
    acquireB     = acquireB    .mincover(rhs.acquireB),
    arithmetic   = arithmetic  .mincover(rhs.arithmetic),
    logical      = logical     .mincover(rhs.logical),
    get          = get         .mincover(rhs.get),
    putFull      = putFull     .mincover(rhs.putFull),
    putPartial   = putPartial  .mincover(rhs.putPartial),
    hint         = hint        .mincover(rhs.hint),
    probeAckData = probeAckData.mincover(rhs.probeAckData))
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
    putPartial.max,
    probeAckData.max
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
  def unknownEmits = M2sTransfers(
    acquireT   = SizeRange(1, 4096),
    acquireB   = SizeRange(1, 4096),
    arithmetic = SizeRange(1, 4096),
    logical    = SizeRange(1, 4096),
    get        = SizeRange(1, 4096),
    putFull    = SizeRange(1, 4096),
    putPartial = SizeRange(1, 4096),
    hint       = SizeRange(1, 4096))
  def unknownSupports = M2sTransfers()

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

  def intersect(values : Seq[M2sTransfers]) : M2sTransfers = values.reduce(_ intersect _)
  def mincover(values : Seq[M2sTransfers]) : M2sTransfers = values.reduce(_ mincover _)
}



case class M2sSource(id           : AddressMapping,
                     emits        : M2sTransfers,
                     isExecute : Boolean = false){
  def withSourceOffset(offset : Int) = copy(id = id.withOffset(offset))
  def bSourceId = id.lowerBound.toInt
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
  def withExecute = mapping.exists(_.isExecute)
  def remapSources(f : M2sSource => M2sSource) = {
    copy(mapping = mapping.map(f))
  }
}



case class M2sParameters(addressWidth : Int,
                         dataWidth : Int,
                         masters   : Seq[M2sAgent]) extends OverridedEqualsHashCode {
  val sizeBytes = masters.map(_.emits.sizeBytes).max
  val sourceWidth = masters.map(_.sourceWidth).max
  val withBCE = masters.map(_.emits.withBCE).reduce(_ || _)
  val emits = M2sTransfers.mincover(masters.map(_.emits))
  def dataBytes = dataWidth / 8
  def withExecute = masters.exists(_.withExecute)
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
}

