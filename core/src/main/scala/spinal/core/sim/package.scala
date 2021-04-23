/*                                                                           *\
**        _____ ____  _____   _____    __                                    **
**       / ___// __ \/  _/ | / /   |  / /   HDL Core                         **
**       \__ \/ /_/ // //  |/ / /| | / /    (c) Dolu, All rights reserved    **
**      ___/ / ____// // /|  / ___ |/ /___                                   **
**     /____/_/   /___/_/ |_/_/  |_/_____/                                   **
**                                                                           **
**      This library is free software; you can redistribute it and/or        **
**    modify it under the terms of the GNU Lesser General Public             **
**    License as published by the Free Software Foundation; either           **
**    version 3.0 of the License, or (at your option) any later version.     **
**                                                                           **
**      This library is distributed in the hope that it will be useful,      **
**    but WITHOUT ANY WARRANTY; without even the implied warranty of         **
**    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU      **
**    Lesser General Public License for more details.                        **
**                                                                           **
**      You should have received a copy of the GNU Lesser General Public     **
**    License along with this library.                                       **
\*                                                                           */
package spinal.core

import spinal.core.internals.BaseNode
import spinal.core.sim.{SimBaseTypePimper, SpinalSimConfig}
import spinal.sim._

import scala.collection.generic.Shrinkable
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.Random

/**
  * Simulation package
  */
package object sim {
  def SimConfig: SpinalSimConfig = new SpinalSimConfig()

  @deprecated("Use SimConfig.???.compile(new Dut) instead", "???")
  def SimConfig[T <: Component](rtl: => T): SimConfigLegacy[T] = {
    new SimConfigLegacy[T](_rtlGen = Some(() => rtl))
  }

  @deprecated("Use SimConfig.???.compile(new Dut) instead", "???")
  def SimConfig[T <: Component](rtl: SpinalReport[T]): SimConfigLegacy[T] = {
    new SimConfigLegacy[T](_spinalReport = Some(rtl))
  }


  private def btToSignal(manager: SimManager, bt: BaseNode) = {
    if(bt.algoIncrementale != -1){
      SimError(s"UNACCESSIBLE SIGNAL : $bt isn't accessible during the simulation.\n- To fix it, call simPublic() on it durring the elaboration.")
    }

    manager.raw.userData.asInstanceOf[ArrayBuffer[Signal]](bt.algoInt)
  }


  def setBigInt[T <: Data](mem : Mem[T], address : Long, data : BigInt): Unit = {
    val manager = SimManagerContext.current.manager
    val tag = mem.getTag(classOf[MemSymbolesTag])
    tag match {
      case None => {
        val signal = btToSignal(manager, mem)
        manager.setBigInt(signal, address, data)
      }
      case Some(tag) => {
        for(i <- 0 until tag.mapping.size; mapping = tag.mapping(i)){
          if(mem.algoIncrementale != -1){
            SimError(s"UNACCESSIBLE SIGNAL : $mem isn't accessible during the simulation.\n- To fix it, call simPublic() on it durring the elaboration.")
          }
          val symbol = manager.raw.userData.asInstanceOf[ArrayBuffer[Signal]](mem.algoInt + i)
          val symbolData = (data >> mapping.range.low) & mapping.mask
          manager.setBigInt(symbol, address, symbolData)
        }
      }
    }
  }

  def getBigInt[T <: Data](mem : Mem[T], address : Long): BigInt = {
    val manager = SimManagerContext.current.manager
    val tag = mem.getTag(classOf[MemSymbolesTag])
    tag match {
      case None => {
        val signal = btToSignal(manager, mem)
        manager.getBigInt(signal, address)
      }
      case Some(tag) => {
        var data = BigInt(0)
        for(i <- 0 until tag.mapping.size; mapping = tag.mapping(i)){
          if(mem.algoIncrementale != -1){
            SimError(s"UNACCESSIBLE SIGNAL : $mem isn't accessible during the simulation.\n- To fix it, call simPublic() on it durring the elaboration.")
          }
          val symbol = manager.raw.userData.asInstanceOf[ArrayBuffer[Signal]](mem.algoInt + i)
          val readed = manager.getBigInt(symbol, address)
          data |= (readed << mapping.range.low)
        }
        data
      }
    }
  }

  /** Get a Int value from a BaseType */
  private def getInt(bt: BaseType): Int = {
    if(bt.getBitsWidth == 0) return 0
    val manager = SimManagerContext.current.manager
    val signal = btToSignal(manager, bt)
    manager.getInt(signal)
  }

  /** Get a Long value from a BaseType */
  private def getLong(bt: BaseType): Long = {
    if(bt.getBitsWidth == 0) return 0l
    val manager = SimManagerContext.current.manager
    val signal = btToSignal(manager, bt)
    manager.getLong(signal)
  }

  /** Get a BigInt value from a BaseType */
  private def getBigInt(bt : BaseType) : BigInt = {
    if(bt.getBitsWidth == 0) return BigInt(0)
    val manager = SimManagerContext.current.manager
    val signal  = btToSignal(manager, bt)
    manager.getBigInt(signal)
  }

  /** Set a long value to a BaseType */
  def setLong(bt: BaseType, value: Long): Unit = {
    if(bt.getBitsWidth == 0) {
      assert(value == 0)
      return
    }

    val manager = SimManagerContext.current.manager
    val signal  = btToSignal(manager, bt)
    manager.setLong(signal, value)
  }

  /** Set a BigInt value to a BaseType */
  def setBigInt(bt : BaseType, value : BigInt) : Unit = {
    if(bt.getBitsWidth == 0) {
      assert(value == 0)
      return
    }
    val manager = SimManagerContext.current.manager
    val signal  = btToSignal(manager, bt)
    manager.setBigInt(signal, value)
  }

  /** Return the current simulation time */
  def simTime(): Long = SimManagerContext.current.manager.time
  def simDeltaCycle(): Long = SimManagerContext.current.manager.deltaCycle

  /** Success/Failure simulation */
  def simSuccess(): Nothing = throw new SimSuccess()
  def simFailure(message: String = ""): Nothing = throw new SimFailure(message)
  def onSimEnd(body : => Unit): Unit = SimManagerContext.current.manager.onEnd(body)

  /** Sleep / WaitUntil */
  def sleep(cycles: Long): Unit = SimManagerContext.current.thread.sleep(cycles)
  def sleep(cycles: Double): Unit = SimManagerContext.current.thread.sleep(cycles.toLong)
  def waitUntil(cond: => Boolean): Unit = {
    SimManagerContext.current.thread.waitUntil(cond)
  }

  /** Fork */
  def fork(body: => Unit): SimThread = SimManagerContext.current.manager.newThread(body)
  def forkJoin(bodys: (()=> Unit)*): Unit = {
    val threads = bodys.map(body => fork(body()))
    threads.foreach(thread => thread.join())
  }

  def forkSensitive(block : => Unit): Unit ={
    SimManagerContext.current.manager.sensitivities += new SimManagerSensitive(){
      override def update(): Boolean = {
        block
        true
      }
    }
  }

  def forkSensitiveWhile(block : => Boolean): Unit ={
    SimManagerContext.current.manager.sensitivities += new SimManagerSensitive(){
      override def update(): Boolean = {
        block
      }
    }
  }

  def forkSensitive(triggers: Data*)(block: => Unit): Unit = {
    def value(data: Data) = data.flatten.map(_.toBigInt)
    def currentTriggerValue = triggers.flatMap(value)

    forkSensitive(currentTriggerValue)(block)
  }

  def forkSensitive(trigger: => Any)(block: => Unit): Unit = {
    var lastValue = trigger

    forkSensitive {
      val newValue = trigger

      if (newValue != lastValue) {
        block
      }

      lastValue = newValue
    }
  }

  def delayed(delay : Long)(body : => Unit) = {
    SimManagerContext.current.manager.schedule(delay)(body)
  }

  def periodicaly(delay : Long)(body : => Unit) : Unit = {
    SimManagerContext.current.manager.schedule(delay){
      body
      periodicaly(delay)(body)
    }
  }

  def simThread = SimManagerContext.current.thread

  /**
    * Add implicit function to BaseType for simulation
    */
  implicit class SimBaseTypePimper(bt: BaseType) {

    def randomize(): Unit = bt match{
      case bt: Bool               => bt #= Random.nextBoolean()
      case bt: Bits               => bt.randomize()
      case bt: UInt               => bt.randomize()
      case bt: SInt               => bt.randomize()
      case bt: SpinalEnumCraft[_] => bt.randomize()
    }

    def assignBigInt(value: BigInt): Unit = bt match{
      case bt: Bool               => bt #= (if(value == 0) false else if(value == 1) true else throw new Exception("Value outide the range"))
      case bt: BitVector          => bt #= value
      case bt: SpinalEnumCraft[_] => {
        assert(value < bt.spinalEnum.elements.length)
        bt #= bt.spinalEnum.elements(value.toInt)
      }
    }

    def toBigInt: BigInt = bt match{
      case bt: Bool               => BigInt(if(bt.toBoolean) 1 else 0)
      case bt: Bits               => bt.toBigInt
      case bt: UInt               => bt.toBigInt
      case bt: SInt               => bt.toBigInt
      case bt: SpinalEnumCraft[_] => BigInt(bt.toEnum.position)
    }
  }


  implicit class SimSeqPimper[T](pimped: Seq[T]){
    def randomPick(): T = pimped(Random.nextInt(pimped.length))
    def randomPickWithIndex(): (T, Int) = {
      val index = Random.nextInt(pimped.length)
      (pimped(index), index)
    }
  }

  implicit class SimArrayBufferPimper[T](pimped: ArrayBuffer[T]){
    def randomPop() : T = {
      val index = Random.nextInt(pimped.length)
      val ret = pimped(index)
      pimped(index) = pimped.last
      pimped.reduceToSize(pimped.length-1)
      ret
    }
    def pop() : T = {
      val index = 0
      val ret = pimped(index)
      pimped(index) = pimped.last
      pimped.reduceToSize(pimped.length-1)
      ret
    }
  }
    /**
    * Add implicit function to Data
    */
  implicit class SimDataPimper[T <: Data](bt: T) {

    def randomize(): Unit = bt.flattenForeach(_.randomize())
    def simPublic(): T = bt.addTag(SimPublic)
  }

  implicit class SimpComponentPimper[T <: Component](uut: T) {
    def tracingOff(): T = uut.addTag(TracingOff)
  }

  implicit class SimMemPimper[T <: Data](mem: Mem[T]) {
    def setBigInt(address : Long, data : BigInt): Unit = sim.setBigInt(mem,address,data)
    def getBigInt(address : Long): BigInt = sim.getBigInt(mem,address)
    def simPublic(): Mem[T] = mem.addTag(SimPublic)
  }

  /**
    * Add implicit function to Bool
    */
  implicit class SimBoolPimper(bt: Bool) {

    def toBoolean = if(getLong(bt) != 0) true else false

    def #=(value: Boolean) = setLong(bt, if(value) 1 else 0)

    def randomize(): Unit = {
      bt #= Random.nextBoolean()
    }
  }

  /**
    * Add implicit function to BitVector
    */
  implicit class SimBitVectorPimper(bt: BitVector) {

    def toInt    = getInt(bt)
    def toLong   = getLong(bt)
    def toBigInt = getBigInt(bt)

    def #=(value: Int)    = setLong(bt, value)
    def #=(value: Long)   = setLong(bt, value)
    def #=(value: BigInt) = setBigInt(bt, value)
    def #=(value: Array[Byte]) = { //TODO improve perf
      var acc = BigInt(0)
      for(i <- value.size-1 downto 0){
        acc = acc << 8
        acc |= value(i).toInt & 0xFF
      }
      setBigInt(bt, acc)
    }
  }

  /**
    * Add implicit function to Bits
    */
  implicit class SimBitsPimper(bt: Bits) {

    def randomize(): Unit = {
      val width = bt.getWidth
      if(width < 64){
        bt #= Random.nextLong() & ((1l << width) - 1)
      }else {
        bt #= BigInt(width, Random)
      }
    }

    def randomizedBigInt() = {
      val width = bt.getWidth
      BigInt(width, Random)
    }
    def randomizedLong() = {
      val width = bt.getWidth
      assert(width < 64)
      Random.nextLong() & ((1l << width) - 1)
    }
    def randomizedInt() = {
      val width = bt.getWidth
      assert(width < 32)
      Random.nextInt() & ((1 << width) - 1)
    }
  }

  /**
    * Add implicit function to UInt
    */
  implicit class SimUIntPimper(bt: UInt) {

    def randomize(): Unit = {
      val width = bt.getWidth
      if(width < 64){
        bt #= Random.nextLong() & ((1l << width) - 1)
      }else {
        bt #= BigInt(width, Random)
      }
    }

    def randomizedBigInt() = {
      val width = bt.getWidth
      BigInt(width, Random)
    }
    def randomizedLong() = {
      val width = bt.getWidth
      assert(width < 64)
      Random.nextLong() & ((1l << width) - 1)
    }
    def randomizedInt() = {
      val width = bt.getWidth
      assert(width < 32)
      Random.nextInt() & ((1 << width) - 1)
    }
  }


  /**
    * Add implicit function to SInt
    */
  implicit class SimSIntPimper(bt: SInt) {

    def randomize(): Unit = {
      val width = bt.getWidth
      if(width <= 64){
        val shift = 64 - width
        bt #= (Random.nextLong() << shift) >> shift
      }else {
        bt #= BigInt(width, Random) - (BigInt(1) << width-1)
      }
    }
  }

  /**
    * Add implicit function to Enum
    */
  implicit class SimEnumPimper[T <: SpinalEnum](bt: SpinalEnumCraft[T]) {

    def toEnum = bt.encoding.getElement(getBigInt(bt), bt.spinalEnum)

    def #=(value: SpinalEnumElement[T]) = setBigInt(bt, bt.encoding.getValue(value))

    def randomize(): Unit ={
      bt #= bt.spinalEnum.elements(Random.nextInt(bt.spinalEnum.elements.length))
    }
  }


  /**
    * Add implicit function to ClockDomain
    */
  implicit class SimClockDomainPimper(cd: ClockDomain) {

    private def getBool(manager: SimManager, who: Bool): Bool = {
      val component = who.component
      if((who.isInput || who.isOutput) && component != null && component.parent == null){
        who
      }else {
        manager.userData.asInstanceOf[Component].pulledDataCache(who).asInstanceOf[Bool]
      }
    }

    private def getSignal(manager: SimManager, who: Bool): Signal ={
      val bt = getBool(manager, who)
      btToSignal(manager, bt)
    }

    def clockSim       = getBool(SimManagerContext.current.manager, cd.clock)
    def resetSim       = getBool(SimManagerContext.current.manager, cd.reset)
    def clockEnableSim = getBool(SimManagerContext.current.manager, cd.clockEnable)
    def softResetSim   = getBool(SimManagerContext.current.manager, cd.softReset)

    def clockToggle(): Unit ={
      val manager = SimManagerContext.current.manager
      val signal = getSignal(manager, cd.clock)
      manager.setLong(signal, 1-manager.getLong(signal))
    }

    def fallingEdge(): Unit = {
      val manager = SimManagerContext.current.manager
      val signal = getSignal(manager, cd.clock)
      manager.setLong(signal, 0)
    }

    def risingEdge(): Unit = {
      val manager = SimManagerContext.current.manager
      val signal = getSignal(manager, cd.clock)
      manager.setLong(signal, 1)
    }



    def waitSampling(): Unit = waitSampling(1)
    def waitSampling(count: Int): Unit ={
      val edgeValue = if(cd.config.clockEdge == spinal.core.RISING) 1l else 0l
      val manager = SimManagerContext.current.manager
      val signal  = getSignal(manager, cd.clock)
      var last    = manager.getLong(signal)
      var counter = 0

      waitUntil{
        val current = manager.getLong(signal)
        if(last != edgeValue && current == edgeValue && isSamplingEnable)
          counter += 1
        last = current
        counter == count
      }
    }

    def waitSamplingWhere(condAnd: => Boolean): Unit = {
      val edgeValue = if(cd.config.clockEdge == spinal.core.RISING) 1l else 0l
      val manager = SimManagerContext.current.manager
      val signal  = getSignal(manager, cd.clock)
      var last    = manager.getLong(signal)

      waitUntil{
        val current = manager.getLong(signal)
        val cond = last != edgeValue && current == edgeValue && condAnd
        last = current
        cond
      }
    }

    def waitEdge(): Unit = waitRisingEdge(1)
    def waitEdge(count : Int): Unit = {
      val manager = SimManagerContext.current.manager
      val signal  = getSignal(manager, cd.clock)
      var last    = manager.getLong(signal)
      var counter = 0

      waitUntil{
        val current = manager.getLong(signal)
        if(last != current)
          counter += 1
        last = current
        counter == count
      }
    }

    def waitEdgeWhere(condAnd: => Boolean): Unit = {
      val manager = SimManagerContext.current.manager
      val signal  = getSignal(manager, cd.clock)
      var last    = manager.getLong(signal)

      waitUntil{
        val current = manager.getLong(signal)
        val cond = last != current && condAnd
        last = current
        cond
      }
    }

    def waitRisingEdge(): Unit = waitRisingEdge(1)
    def waitRisingEdge(count: Int): Unit ={
      val manager = SimManagerContext.current.manager
      val signal = getSignal(manager, cd.clock)
      var last = manager.getLong(signal)
      var counter = 0
      waitUntil{
        val current = manager.getLong(signal)
        if(last == 0l && current == 1l)
          counter += 1
        last = current
        counter == count
      }
    }

    def waitRisingEdgeWhere(condAnd: => Boolean): Unit = {
      val manager = SimManagerContext.current.manager
      val signal  = getSignal(manager, cd.clock)
      var last    = manager.getLong(signal)

      waitUntil{
        val current = manager.getLong(signal)
        val cond = last == 0l && current == 1l && condAnd
        last = current
        cond
      }
    }

    def waitFallingEdge(): Unit = waitFallingEdge(1)
    def waitFallingEdge(count: Int = 1): Unit = {
      val manager = SimManagerContext.current.manager
      val signal  = getSignal(manager, cd.clock)
      var last    = manager.getLong(signal)
      var counter = 0

      waitUntil{
        val current = manager.getLong(signal)
        if(last == 1l && current == 0l)
          counter += 1
        last = current
        counter == count
      }
    }

    def waitFallingEdgeWhere(condAnd: => Boolean): Unit = {
      val manager = SimManagerContext.current.manager
      val signal  = getSignal(manager, cd.clock)
      var last    = manager.getLong(signal)

      waitUntil{
        val current = manager.getLong(signal)
        val cond = last == 1l && current == 0l && condAnd
        last = current
        cond
      }
    }

    def waitActiveEdge(): Unit = waitActiveEdge(1)
    def waitActiveEdge(count: Int = 1): Unit = {
      if (cd.config.clockEdge == spinal.core.RISING) {
        waitRisingEdge(count)
      }else{
        waitFallingEdge(count)
      }
    }

    def waitActiveEdgeWhere(condAnd: => Boolean): Unit = {
      if(cd.config.clockEdge == spinal.core.RISING) {
        waitRisingEdgeWhere(condAnd)
      }else {
        waitFallingEdgeWhere(condAnd)
      }
    }

    def doStimulus(period: Long): Unit = {
      assert(period >= 2)

      if(cd.hasClockEnableSignal) assertClockEnable()
      if(cd.hasSoftResetSignal)   deassertSoftReset()

      cd.config.clockEdge match {
        case RISING  => fallingEdge()
        case FALLING => risingEdge()
      }

      if(cd.config.resetKind == ASYNC){
          val dummy = if(cd.hasResetSignal){
            cd.resetSim #= (cd.config.resetActiveLevel match{
              case HIGH => false
              case LOW => true
            })
            sleep(0)
            DoReset(resetSim, period*16, cd.config.resetActiveLevel)
          }
          sleep(period)
          DoClock(clockSim, period)
      } else if(cd.config.resetKind == SYNC){
        if(cd.hasResetSignal){
          cd.assertReset()
          val clk = clockSim
          var value = clk.toBoolean
          for(repeat <- 0 to 31){
            value = !value
            clk #= value
            sleep(period >> 1)
          }
          cd.deassertReset()
        }
        DoClock(clockSim, period)
      } else if(cd.config.resetKind == BOOT){
        sleep(period)
        DoClock(clockSim, period)
      } else {
        throw new Exception("???")
      }

    }

    def forkStimulus(period: Long) : Unit = {
      cd.config.clockEdge match {
        case RISING  => fallingEdge()
        case FALLING => risingEdge()
      }
      if(cd.hasResetSignal) cd.deassertReset()
      if(cd.hasSoftResetSignal) cd.deassertSoftReset()
      if(cd.hasClockEnableSignal) cd.deassertClockEnable()
      fork(doStimulus(period))
    }

    def forkSimSpeedPrinter(printPeriod: Double = 1.0) : Unit = SimSpeedPrinter(cd, printPeriod)

    def onRisingEdges(block : => Unit): Unit ={
      val manager = SimManagerContext.current.manager
      val signal = getSignal(manager, cd.clock)
      var last = manager.getInt(signal)
      forkSensitive{
        val current = manager.getInt(signal)
        if(last == 0 && current == 1) block
        last = current
      }
    }

    def onFallingEdges(block : => Unit): Unit ={
      val manager = SimManagerContext.current.manager
      val signal = getSignal(manager, cd.clock)
      var last = manager.getInt(signal)
      forkSensitive{
        val current = manager.getInt(signal)
        if(last == 1 && current == 0) block
        last = current
      }
    }

    def onActiveEdges(block : => Unit): Unit = {
      if (cd.config.clockEdge == spinal.core.RISING) {
        onRisingEdges(block)
      }else{
        onFallingEdges(block)
      }
    }

    def onEdges(block : => Unit): Unit ={
      val manager = SimManagerContext.current.manager
      val signal = getSignal(manager, cd.clock)
      var last = manager.getInt(signal)
      forkSensitive{
        val current = manager.getInt(signal)
        if(last != current) block
        last = current
      }
    }

    def onSamplings(body: => Unit): Unit = {
      val key = (SimStatics.onSamplings, cd)
      val context = SimManagerContext.current
      if(!context.contains(key)) {
        val edgeValue = if (cd.config.clockEdge == spinal.core.RISING) 1 else 0
        val manager = context.manager
        val signal = getSignal(manager, cd.clock)
        var last = manager.getInt(signal)
        val listeners = ArrayBuffer[() => Unit]()
        context.set(key, listeners)
        forkSensitive {
          val current = manager.getInt(signal)
          if (last != edgeValue && current == edgeValue && isSamplingEnable)
            listeners.foreach(_())
          last = current
        }
      }
      context.get[ArrayBuffer[() => Unit]](key) += (() => body)
    }

    def onNextSampling(body: => Unit): Unit = {
      val edgeValue = if(cd.config.clockEdge == spinal.core.RISING) 1 else 0
      val manager = SimManagerContext.current.manager
      val signal  = getSignal(manager, cd.clock)
      var last    = manager.getInt(signal)

      forkSensitiveWhile {
        val current = manager.getInt(signal)
        if(last != edgeValue && current == edgeValue && isSamplingEnable) {
          body
          false
        }else {
          last = current
          true
        }
      }
    }


    def assertReset(): Unit         = resetSim #= cd.config.resetActiveLevel == spinal.core.HIGH
    def deassertReset(): Unit       = resetSim #= cd.config.resetActiveLevel != spinal.core.HIGH

    def assertClockEnable(): Unit   = clockEnableSim #= cd.config.clockEnableActiveLevel == spinal.core.HIGH
    def deassertClockEnable(): Unit = clockEnableSim #= cd.config.clockEnableActiveLevel != spinal.core.HIGH

    def assertSoftReset(): Unit     = softResetSim #= cd.config.softResetActiveLevel == spinal.core.HIGH
    def deassertSoftReset(): Unit   = softResetSim #= cd.config.softResetActiveLevel != spinal.core.HIGH


    def isResetAsserted: Boolean         = (cd.hasResetSignal && (cd.resetSim.toBoolean ^ cd.config.resetActiveLevel != spinal.core.HIGH)) || (cd.hasSoftResetSignal && (cd.softResetSim.toBoolean ^ cd.config.softResetActiveLevel != spinal.core.HIGH))
    def isResetDeasserted: Boolean       =  ! isResetAsserted

    def isClockEnableAsserted: Boolean   = !cd.hasClockEnableSignal || (cd.clockEnableSim.toBoolean ^ cd.config.clockEnableActiveLevel != spinal.core.HIGH)
    def isClockEnableDeasserted: Boolean = ! isClockEnableAsserted

    def isSamplingEnable: Boolean        = isResetDeasserted && isClockEnableAsserted
    def isSamplingDisable: Boolean       = ! isSamplingEnable
  }
  implicit class SimClockDomainHandlePimper(cd: spinal.core.fiber.Handle[ClockDomain]) extends SimClockDomainPimper(cd.get)

  def enableSimWave() =  SimManagerContext.current.manager.raw.enableWave()
  def disableSimWave() =  SimManagerContext.current.manager.raw.disableWave()

  case class SimMutex(){
    val queue = mutable.Queue[SimThread]()
    var locked = false
    def lock(){
      val t = simThread
      if(locked) {
        queue.enqueue(t)
        t.suspend()
      } else {
        locked = true
      }
    }
    def unlock(){
      assert(locked)
      if(queue.nonEmpty) {
        queue.dequeue().resume()
      } else {
        locked = false
      }
    }
  }


  def forkSimSporadicWave(captures : Seq[(Double, Double)], enableTime : Double = 1e-7, disableTime : Double = 1e-4, timeUnit : Double = 1e12): Unit ={
    fork{
      for((at, until) <- captures) {
        val duration = until-at
        assert(duration >= 0)
        while (simTime() < at * timeUnit) {
          disableSimWave()
          sleep(disableTime * timeUnit)
          enableSimWave()
          sleep(enableTime * timeUnit)
        }
        println("\n\n********************")
        sleep(duration * timeUnit)
        println("********************\n\n")
      }
      while(true) {
        disableSimWave()
        sleep(disableTime * timeUnit)
        enableSimWave()
        sleep(  enableTime * timeUnit)
      }
    }
  }
}
