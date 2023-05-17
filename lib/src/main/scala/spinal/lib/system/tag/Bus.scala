package spinal.lib.system.tag

import spinal.core._
import spinal.lib.bus.misc.{AddressMapping, DefaultMapping}

class MemoryConnection[T <: Nameable with SpinalTagReady, T2 <: Nameable with SpinalTagReady](val m : T, val s : T2, val mapping :  AddressMapping) extends SpinalTag

object MemoryConnection{
  def walk(m : Nameable with SpinalTagReady)(body : (Nameable with SpinalTagReady, BigInt, BigInt) => Unit): Unit = walk(m, 0, -1)(body)
  def walk(m : Nameable with SpinalTagReady, address : BigInt, size : BigInt)(body : (Nameable with SpinalTagReady, BigInt, BigInt) => Unit): Unit ={
    //println(m.getName() + f" at 0x$address%x over 0x$size%x")
    body(m, address, size)
    m.foreachTag{
      case c : MemoryConnection[_,_] if c.m == m => {
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