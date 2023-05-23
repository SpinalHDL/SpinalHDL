package spinal.lib.system.tag

import spinal.core._
import spinal.lib.bus.misc.{AddressMapping, DefaultMapping}

trait MemoryConnection extends SpinalTag {
  def m : Nameable with SpinalTagReady
  def s : Nameable with SpinalTagReady
  def mapping :  AddressMapping
}


object MemoryConnection{
  case class WalkArgs(node : Nameable with SpinalTagReady, address : BigInt, size : BigInt)
  def walk(m : Nameable with SpinalTagReady)(body : WalkArgs => Unit): Unit = walk(m, 0, -1)(body)
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
