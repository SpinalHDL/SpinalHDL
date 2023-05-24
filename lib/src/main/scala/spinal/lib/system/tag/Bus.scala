package spinal.lib.system.tag

import spinal.core._
import spinal.lib.bus.misc.{AddressMapping, DefaultMapping}
import spinal.lib.bus.tilelink.InterconnectNode

import scala.collection.mutable

//trait SupportedAccess

trait MemoryConnection extends SpinalTag {
  def m : Nameable with SpinalTagReady
  def s : Nameable with SpinalTagReady
  def mapping :  AddressMapping
//  def sToM(downs : Seq[SupportedAccess]) : Seq[SupportedAccess]

  def populate(): Unit ={
    m.addTag(this)
    s.addTag(this)
  }
}


object MemoryConnection{
  case class WalkArgs(node : Nameable with SpinalTagReady, address : BigInt, size : BigInt){
    override def toString = f"$node $address%x $size%x"
  }
  def nodes(m : InterconnectNode): Seq[WalkArgs] = nodes(m, 0, BigInt(1) << m.bus.p.addressWidth)
  def nodes(m : Nameable with SpinalTagReady, address : BigInt, size : BigInt): Seq[WalkArgs] ={
    val l = mutable.LinkedHashMap[Nameable with SpinalTagReady, WalkArgs]()
    walk(m, address, size){ args =>
      l.get(args.node) match {
        case Some(e) => assert(e == args)
        case None => l(args.node) = args
      }
    }
    l.values.toList
  }
  def walk(m : InterconnectNode)(body : WalkArgs => Unit): Unit = walk(m, 0, BigInt(1) << m.bus.p.addressWidth)(body)
  def walk(m : Nameable with SpinalTagReady, address : BigInt, size : BigInt)(body : WalkArgs => Unit): Unit ={
    //println(m.getName() + f" at 0x$address%x over 0x$size%x")
    body(WalkArgs(m, address, size))
    m.foreachTag{
      case c : MemoryConnection if c.m == m => {
        val (a,s) = c.mapping match {
          case DefaultMapping => (address, size)
          case m => (m.lowerBound, m.highestBound+1-m.lowerBound)
        }
        walk(c.s, address+a, s)(body)
      }
      case _ =>
    }
  }
}

trait PMA extends SpinalTag

object PMA {
  case object CACHED        extends PMA // an intermediate agent may have cached a copy of the region for you
  case object TRACKED       extends PMA // the region may have been cached by another master, but coherence is being provided
  case object UNCACHED      extends PMA // the region has not been cached yet, but should be cached when possible
  case object IDEMPOTENT    extends PMA // reads return most recently put content, but content should not be cached
  case object VOLATILE      extends PMA // content may change without a write
  case object WRITE_EFFECTS extends PMA // writes produce side effects and so must not be combined/delayed
  case object READ_EFFECTS  extends PMA // reads produce side effects and so must not be issued speculatively
  case object EXECUTABLE    extends PMA // Allows an agent to fetch code from this region
}
