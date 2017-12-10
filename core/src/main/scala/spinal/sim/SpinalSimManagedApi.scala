package spinal.sim

import spinal.core.{BaseType, BitVector, Bool, Bits, UInt, SInt, ClockDomain, Component}

import scala.collection.mutable.ArrayBuffer
import scala.util.continuations.suspendable


object SpinalSimManagedApi{
  private def btToSignal(manager : SimManager, bt : BaseType) = manager.raw.userData.asInstanceOf[ArrayBuffer[Signal]](bt.algoInt)

  def getInt(bt : BaseType) : Long = {
    val manager = SimManagerContext.current.manager
    val signal = btToSignal(manager, bt)
    manager.getInt(signal)
  }

  def getLong(bt : BaseType) : Long = {
    val manager = SimManagerContext.current.manager
    val signal = btToSignal(manager, bt)
    manager.getLong(signal)
  }

  def getBigInt(bt : BaseType) : BigInt = {
    val manager = SimManagerContext.current.manager
    val signal = btToSignal(manager, bt)
    manager.getBigInt(signal)
  }

  def setLong(bt : BaseType, value : Long) = {
    val manager = SimManagerContext.current.manager
    val signal = btToSignal(manager, bt)
    manager.setLong(signal, value)
  }

  def setBigInt(bt : BaseType, value : BigInt) = {
    val manager = SimManagerContext.current.manager
    val signal = btToSignal(manager, bt)
    manager.setBigInt(signal, value)
  }

  def sleep(cycles : Long) : Unit@suspendable = SimManagerContext.current.thread.sleep(cycles)
  def waitUntil(cond : => Boolean) : Unit@suspendable = SimManagerContext.current.thread.waitUntil(cond)
  def fork(body : => Unit@suspendable) : SimThread@suspendable = SimManagerContext.current.manager.newThread(body)

  implicit class BoolPimper(bt : Bool) {
    def toBoolean = if(getLong(bt) != 0) true else false
    def :=(value : Boolean) = setLong(bt, if(value) 1 else 0)
  }

  implicit class BitVectorPimper(bt : BitVector) {
    def toInt = getInt(bt)
    def toLong = getLong(bt)
    def toBigInt = getBigInt(bt)
    def \=(value : Int) = setLong(bt, value)
    def \=(value : Long) = setLong(bt, value)
    def \=(value : BigInt) = setBigInt(bt, value)
  }

  implicit class ClockDomainPimper(cd : ClockDomain) {
    private def getClockSignal(manager : SimManager): Signal ={
      val manager = SimManagerContext.current.manager
      val bt = manager.userData.asInstanceOf[Component].pulledDataCache(cd.clock).asInstanceOf[Bool]
      btToSignal(manager, bt)
    }
    def fallingEdge : Unit = {
      val manager = SimManagerContext.current.manager
      val signal = getClockSignal(manager)
      manager.setLong(signal, 0)
    }
    def risingEdge : Unit = {
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
