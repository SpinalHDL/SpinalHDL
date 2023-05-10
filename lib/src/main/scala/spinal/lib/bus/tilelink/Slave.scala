package spinal.lib.bus.tilelink

import spinal.core._
import spinal.lib._
import spinal.lib.bus.misc.{AddressMapping, SizeMapping}


case class S2mTransfers(probe:      SizeRange = SizeRange.none,
                        arithmetic: SizeRange = SizeRange.none,
                        logical:    SizeRange = SizeRange.none,
                        get:        SizeRange = SizeRange.none,
                        putFull:    SizeRange = SizeRange.none,
                        putPartial: SizeRange = SizeRange.none,
                        hint:       SizeRange = SizeRange.none
                         )  {
  def withBCE = probe.some
  def withDataB = putFull.some || putPartial.some

  def intersect(rhs: S2mTransfers) = S2mTransfers(
    probe      = probe     .intersect(rhs.probe),
    arithmetic = arithmetic.intersect(rhs.arithmetic),
    logical    = logical   .intersect(rhs.logical),
    get        = get       .intersect(rhs.get),
    putFull    = putFull   .intersect(rhs.putFull),
    putPartial = putPartial.intersect(rhs.putPartial),
    hint       = hint      .intersect(rhs.hint)
  )
  def mincover(rhs: S2mTransfers) = S2mTransfers(
    probe      = probe     .mincover(rhs.probe),
    arithmetic = arithmetic.mincover(rhs.arithmetic),
    logical    = logical   .mincover(rhs.logical),
    get        = get       .mincover(rhs.get),
    putFull    = putFull   .mincover(rhs.putFull),
    putPartial = putPartial.mincover(rhs.putPartial),
    hint       = hint      .mincover(rhs.hint)
  )
  // Reduce rendering to a simple yes/no per field
  override def toString = {
    def str(x: SizeRange, flag: String) = if (x.none) "" else flag
    def flags = Vector(
      str(probe,      "P"),
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
    s"""probe = ${probe}
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
    probe.max,
    arithmetic.max,
    logical.max,
    get.max,
    putFull.max,
    putPartial.max,
    hint.max
  ).max
}

object S2mTransfers {
  def unknownEmits = S2mTransfers(
    arithmetic = SizeRange(1, 4096),
    logical    = SizeRange(1, 4096),
    get        = SizeRange(1, 4096),
    putFull    = SizeRange(1, 4096),
    putPartial = SizeRange(1, 4096),
    hint       = SizeRange(1, 4096),
    probe      = SizeRange(1, 4096))
  def unknownSupports = S2mTransfers()
  def intersect(values : Seq[S2mTransfers]) : S2mTransfers = values.reduce(_ intersect _)
}


object S2mAgent{
  def simple(name    : Nameable) = S2mAgent(
    name   = name,
    sinkId = SizeMapping(0,0),
    emits = S2mTransfers()
  )
}
case class S2mAgent(name    : Nameable,
                    sinkId  : AddressMapping,
                    emits   : S2mTransfers) extends OverridedEqualsHashCode {
  val sinkWidth = sinkId.width
  def withSinkOffset(offset : Int): S2mAgent ={
    copy(sinkId = sinkId.withOffset(offset))
  }
}

object S2mParameters{
  def simple(name : Nameable) : S2mParameters = S2mParameters(
    List(S2mAgent.simple(name))
  )
}
case class S2mParameters(slaves    : Seq[S2mAgent]) extends OverridedEqualsHashCode {
  def defaulted[T](default : T)(body : => T) : T = if(slaves.isEmpty) default else body
  val sizeBytes = defaulted(0)(slaves.map(_.emits.sizeBytes).max)
  val sinkWidth = defaulted(0)(slaves.map(_.sinkWidth).max)
  val withBCE   = defaulted(false)(slaves.map(_.emits.withBCE).reduce(_ || _))
  def withDataB = defaulted(false)(slaves.map(_.emits.withDataB).reduce(_ || _))
}

