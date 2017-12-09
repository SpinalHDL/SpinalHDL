package spinal.sim

import spinal.core._
import spinal.core.internals.GraphUtils

import scala.util.continuations._

object SimRaw {
  val threadLocal = new ThreadLocal[SimRaw[_]]
  def current = threadLocal.get()
  def peak(bt : BaseType) : Long = current.peak(bt)
  def poke(bt : BaseType, value : Long)= current.poke(bt, value)
  def eval() = current.eval
  def sleep(cycles : Long) = current.sleep(cycles)
  def end() = current.end()

  implicit class BaseTypePimper(bt : BaseType) {
    def toLong = current.peak(bt)
    def :<< (value : Long) = current.poke(bt, value)
  }

  implicit class ClockDomainPimper(cd : ClockDomain) {
    def fallingEdge = {
      val current = SimRaw.current
      current.poke(current.getClock(cd), 0)
    }
    def risingEdge = {
      val current = SimRaw.current
      current.poke(current.getClock(cd), 1)
    }
  }
}

abstract class SimRaw[T <: Component](val dut : T){
  def peak(bt : BaseType) : Long
  def poke(bt : BaseType, value : Long)
  def eval()
  def sleep(cycles : Long)
  def end()

  implicit class BaseTypePimper2(bt : BaseType) {
    def toLong = peak(bt)
    def :<< (value : Long) = poke(bt, value)
  }

  def getClock(cd : ClockDomain) : Bool
  implicit class ClockDomainPimper2(cd : ClockDomain) {
    def fallingEdge = poke(getClock(cd), 0)
    def risingEdge = poke(getClock(cd), 1)
  }
}