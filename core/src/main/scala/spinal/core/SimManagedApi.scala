package spinal.core

import spinal.sim._

import scala.collection.mutable.ArrayBuffer
import scala.util.continuations.{cps}


object SimManagedApi{
  type suspendable = cps[Unit]
  private def btToSignal(manager : SimManager, bt : BaseType) = {
    if(bt.algoInt == -1){
      SimError(s"UNACCESSIBLE SIGNAL : $bt isn't accessible during the simulation")
    }
    manager.raw.userData.asInstanceOf[ArrayBuffer[Signal]](bt.algoInt)
  }

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
  def fork(body : => Unit@suspendable) : SimThread = SimManagerContext.current.manager.newThread(body)
  def forkJoin(bodys : (()=> Unit@suspendable)*) : Unit@suspendable = {
    val threads = bodys.mapSim(body => fork(body()))
    threads.foreachSim(thread => thread.join())
  }

  implicit class BoolPimper(bt : Bool) {
    def toBoolean = if(getLong(bt) != 0) true else false
    def #=(value : Boolean) = setLong(bt, if(value) 1 else 0)
  }

  implicit class BitVectorPimper(bt : BitVector) {
    def toInt = getInt(bt)
    def toLong = getLong(bt)
    def toBigInt = getBigInt(bt)
    def #=(value : Int) = setLong(bt, value)
    def #=(value : Long) = setLong(bt, value)
    def #=(value : BigInt) = setBigInt(bt, value)
  }

  implicit class ClockDomainPimper(cd : ClockDomain) {
    private def getBool(manager : SimManager, who : Bool): Bool ={
      val manager = SimManagerContext.current.manager
      manager.userData.asInstanceOf[Component].pulledDataCache(cd.clock).asInstanceOf[Bool]
    }

    private def getSignal(manager : SimManager, who : Bool): Signal ={
      val manager = SimManagerContext.current.manager
      val bt = manager.userData.asInstanceOf[Component].pulledDataCache(cd.clock).asInstanceOf[Bool]
      btToSignal(manager, bt)
    }

    def clockSim = getBool(SimManagerContext.current.manager, cd.clock)
    def resetSim = getBool(SimManagerContext.current.manager, cd.reset)
    def clockEnableSim = getBool(SimManagerContext.current.manager, cd.clockEnable)
    def softResetSim = getBool(SimManagerContext.current.manager, cd.softReset)

    def clockToggle(): Unit ={
      val manager = SimManagerContext.current.manager
      val signal = getSignal(manager, cd.clock)
      manager.setLong(signal, 1-manager.getLong(signal))
    }
    def fallingEdge() : Unit = {
      val manager = SimManagerContext.current.manager
      val signal = getSignal(manager, cd.clock)
      manager.setLong(signal, 0)
    }
    def risingEdge() : Unit = {
      val manager = SimManagerContext.current.manager
      val signal = getSignal(manager, cd.clock)
      manager.setLong(signal, 1)
    }

    def waitRisingEdge(): Unit@suspendable  ={
      val manager = SimManagerContext.current.manager
      val signal = getSignal(manager, cd.clock)
      waitUntil(manager.getLong(signal) == 0)
      waitUntil(manager.getLong(signal) == 1)
    }

    def waitFallingEdge(): Unit@suspendable  ={
      val manager = SimManagerContext.current.manager
      val signal = getSignal(manager, cd.clock)
      waitUntil(manager.getLong(signal) == 1)
      waitUntil(manager.getLong(signal) == 0)
    }

    def waitActiveEdge(): Unit@suspendable  = {
      if(cd.config.clockEdge == spinal.core.RISING)
        waitRisingEdge
      else
        waitFallingEdge
    }

    def assertReset() : Unit = resetSim #= cd.config.resetActiveLevel == spinal.core.HIGH
    def disassertReset() : Unit = resetSim #= cd.config.resetActiveLevel != spinal.core.HIGH
    def assertClockEnable() : Unit = clockEnableSim #= cd.config.clockEnableActiveLevel == spinal.core.HIGH
    def disassertClockEnable() : Unit = clockEnableSim #= cd.config.clockEnableActiveLevel != spinal.core.HIGH
    def assertSoftReset() : Unit = softResetSim #= cd.config.softResetActiveLevel == spinal.core.HIGH
    def disassertSoftReset() : Unit = softResetSim #= cd.config.softResetActiveLevel != spinal.core.HIGH
  }
}
