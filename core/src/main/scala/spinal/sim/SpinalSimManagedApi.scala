package spinal.sim

import spinal.core.{BaseType, Bool, ClockDomain, Component}

import scala.collection.mutable.ArrayBuffer
import scala.util.continuations.suspendable


object SpinalSimManagedApi{
  private def btToSignal(manager : SimManager, bt : BaseType) = manager.raw.userData.asInstanceOf[ArrayBuffer[Signal]](bt.algoInt)

  def getLong(bt : BaseType) : Long = {
    val manager = SimManagerContext.current.manager
    val signal = btToSignal(manager, bt)
    manager.getLong(signal)
  }
  def setLong(bt : BaseType, value : Long) = {
    val manager = SimManagerContext.current.manager
    val signal = btToSignal(manager, bt)
    manager.setLong(signal, value)
  }
  def sleep(cycles : Long) : Unit@suspendable = SimManagerContext.current.thread.sleep(cycles)
  def waitUntil(cond : => Boolean) : Unit@suspendable = SimManagerContext.current.thread.waitUntil(cond)
  def fork(body : => Unit@suspendable) : SimThread@suspendable = SimManagerContext.current.manager.newThread(body)

  implicit class BaseTypePimper(bt : BaseType) {
    def toLong = getLong(bt)
    def :<< (value : Long) = setLong(bt, value)
  }

  implicit class ClockDomainPimper(cd : ClockDomain) {
    private def getClockSignal(manager : SimManager): Signal ={
      val manager = SimManagerContext.current.manager
      val bt = manager.userData.asInstanceOf[Component].pulledDataCache(cd.clock).asInstanceOf[Bool]
      btToSignal(manager, bt)
    }
    def fallingEdge = {
      val manager = SimManagerContext.current.manager
      val signal = getClockSignal(manager)
      manager.setLong(signal, 0)
    }
    def risingEdge = {
      val manager = SimManagerContext.current.manager
      val signal = getClockSignal(manager)
      manager.setLong(signal, 1)
    }

    def waitRisingEdge: Unit@suspendable  ={
      val manager = SimManagerContext.current.manager
      val signal = getClockSignal(manager)
      waitUntil(manager.getLong(signal) == 0)
      waitUntil(manager.getLong(signal) == 1)
    }
  }
}
