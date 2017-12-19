package spinal.core

import spinal.sim._
import scala.collection.mutable.ArrayBuffer
import scala.util.continuations.cps

package object sim {
  type suspendable = cps[Unit]
  private def btToSignal(manager : SimManager, bt : BaseType) = {
    if(bt.algoIncrementale != -1){
      SimError(s"UNACCESSIBLE SIGNAL : $bt isn't accessible during the simulation")
    }
    manager.raw.userData.asInstanceOf[ArrayBuffer[Signal]](bt.algoInt)
  }

  def getInt(bt : BaseType) : Long = {
    if(bt.getBitsWidth == 0) return 0
    val manager = SimManagerContext.current.manager
    val signal = btToSignal(manager, bt)
    manager.getInt(signal)
  }

  def getLong(bt : BaseType) : Long = {
    if(bt.getBitsWidth == 0) return 0l
    val manager = SimManagerContext.current.manager
    val signal = btToSignal(manager, bt)
    manager.getLong(signal)
  }

  def getBigInt(bt : BaseType) : BigInt = {
    if(bt.getBitsWidth == 0) return BigInt(0)
    val manager = SimManagerContext.current.manager
    val signal = btToSignal(manager, bt)
    manager.getBigInt(signal)
  }

  def setLong(bt : BaseType, value : Long) : Unit = {
    if(bt.getBitsWidth == 0) {
      assert(value == 0)
      return
    }
    val manager = SimManagerContext.current.manager
    val signal = btToSignal(manager, bt)
    manager.setLong(signal, value)
  }

  def setBigInt(bt : BaseType, value : BigInt) : Unit = {
    if(bt.getBitsWidth == 0) {
      assert(value == 0)
      return
    }
    val manager = SimManagerContext.current.manager
    val signal = btToSignal(manager, bt)
    manager.setBigInt(signal, value)
  }

  def simTime() : Long = SimManagerContext.current.manager.time
  def simExit(): Unit@suspendable = SimManagerContext.current.manager.exitSim()
  def sleep(cycles : Long) : Unit@suspendable = SimManagerContext.current.thread.sleep(cycles)
  def waitUntil(cond : => Boolean) : Unit@suspendable = SimManagerContext.current.thread.waitUntil(cond)
  def fork(body : => Unit@suspendable) : SimThread = SimManagerContext.current.manager.newThread(body)
  def forkJoin(bodys : (()=> Unit@suspendable)*) : Unit@suspendable = {
    val threads = bodys.map(body => fork(body()))
    threads.suspendable.foreach(thread => thread.join())
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


  implicit class EnumPimper[T <: SpinalEnum](bt : SpinalEnumCraft[T]) {
    def toEnum = bt.encoding.getElement(getBigInt(bt), bt.spinalEnum)
    def #=(value : SpinalEnumElement[T]) = setBigInt(bt, bt.encoding.getValue(value))
  }


  implicit class ClockDomainPimper(cd : ClockDomain) {
    private def getBool(manager : SimManager, who : Bool): Bool ={
      val manager = SimManagerContext.current.manager
      manager.userData.asInstanceOf[Component].pulledDataCache(who).asInstanceOf[Bool]
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

    def doStimulus(period : Long) : Unit@suspendable ={
      if(cd.hasClockEnableSignal) assertClockEnable()
      if(cd.hasSoftResetSignal) disassertSoftReset()
      cd.config.clockEdge match {
        case RISING => fallingEdge()
        case FALLING => risingEdge()
      }
      val dummy = if(cd.config.resetKind == ASYNC){
          val dummy = if(cd.hasResetSignal){
            cd.resetSim #= (cd.config.resetActiveLevel match {
              case HIGH => false
              case LOW => true
            })
            sleep(0)
            DoReset(resetSim, period*16, cd.config.resetActiveLevel)
          }
          sleep(period)
          DoClock(clockSim, period)
      } else if(cd.config.resetKind == SYNC) {
        val dummy = if(cd.hasResetSignal){
          cd.assertReset()
          val clk = clockSim
          var value = clk.toBoolean
          spinal.sim.repeatSim(32){
            value = !value
            clk #= value
            sleep(period >> 1)
          }
          cd.disassertReset()
        }
        DoClock(clockSim, period)
      } else if(cd.config.resetKind == BOOT){
        sleep(period)
        DoClock(clockSim, period)
      } else {
        val dummy = throw new Exception("???")
      }

    }

    def forkStimulus(period : Long) = fork(doStimulus(period))

    def assertReset() : Unit = resetSim #= cd.config.resetActiveLevel == spinal.core.HIGH
    def disassertReset() : Unit = resetSim #= cd.config.resetActiveLevel != spinal.core.HIGH
    def assertClockEnable() : Unit = clockEnableSim #= cd.config.clockEnableActiveLevel == spinal.core.HIGH
    def disassertClockEnable() : Unit = clockEnableSim #= cd.config.clockEnableActiveLevel != spinal.core.HIGH
    def assertSoftReset() : Unit = softResetSim #= cd.config.softResetActiveLevel == spinal.core.HIGH
    def disassertSoftReset() : Unit = softResetSim #= cd.config.softResetActiveLevel != spinal.core.HIGH

    def isResetAsserted : Boolean = (cd.hasResetSignal && (cd.resetSim.toBoolean ^ cd.config.resetActiveLevel != spinal.core.HIGH)) || (cd.hasSoftResetSignal && (cd.softResetSim.toBoolean ^ cd.config.softResetActiveLevel != spinal.core.HIGH))
    def isResetDisasserted : Boolean = ! isResetAsserted
  }
}
