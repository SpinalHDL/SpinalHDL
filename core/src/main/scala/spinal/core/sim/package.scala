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

import java.math.BigInteger
import scala.collection.generic.Shrinkable
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.collection.Seq

/**
  * Simulation package
  */
package object sim {
  def SimConfig: SpinalSimConfig = new SpinalSimConfig()

  def simRandom(implicit simManager: SimManager = sm) = simManager.random
  def sm = SimManagerContext.current.manager

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
      SimError(s"UNACCESSIBLE SIGNAL : $bt isn't accessible during the simulation.\n- To fix it, call simPublic() on it during the elaboration.")
    }

    manager.raw.userData.asInstanceOf[ArrayBuffer[Signal]](bt.algoInt)
  }


  def setBigInt[T <: Data](mem : Mem[T], address : Long, data : BigInt): Unit = {
    val manager = SimManagerContext.current.manager
    val tag = mem.getTag(classOf[MemSymbolesTag])
    val depth = mem.wordCount
    if(address >= depth){
      SimError(s"Attempting to write to an out of range address: address: $address, memory depth: $depth")
    }
    tag match {
      case None => {
        val signal = btToSignal(manager, mem)
        manager.setBigInt(signal, address, data)
      }
      case Some(tag) => {
        for(i <- 0 until tag.mapping.size; mapping = tag.mapping(i)){
          if(mem.algoIncrementale != -1){
            SimError(s"UNACCESSIBLE SIGNAL : $mem isn't accessible during the simulation.\n- To fix it, call simPublic() on it during the elaboration.")
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
    val depth = mem.wordCount
    if(address >= depth){
      SimError(s"Attempting to read from an out of range address: address: $address, memory depth: $depth")
    }
    tag match {
      case None => {
        val signal = btToSignal(manager, mem)
        manager.getBigInt(signal, address)
      }
      case Some(tag) => {
        var data = BigInt(0)
        for(i <- 0 until tag.mapping.size; mapping = tag.mapping(i)){
          if(mem.algoIncrementale != -1){
            SimError(s"UNACCESSIBLE SIGNAL : $mem isn't accessible during the simulation.\n- To fix it, call simPublic() on it during the elaboration.")
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
  private def getLong(bt: BaseType)(implicit manager: SimManager = SimManagerContext.current.manager): Long = {
    if(bt.getBitsWidth == 0) return 0l
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

  def simCompiled : SimCompiled[_ <: Component] = sm.asInstanceOf[CoreSimManager].compiled
  def currentTestName(): String = sm.testName
  def currentTestPath(): String = simCompiled.simConfig._testPath.replace("$TEST", currentTestName())

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
  def sleep(time: TimeNumber): Unit =
    sleep((time.toBigDecimal / SimManagerContext.current.manager.timePrecision).setScale(0, BigDecimal.RoundingMode.UP).toLong)
  def waitUntil(cond: => Boolean): Unit = {
    SimManagerContext.current.thread.waitUntil(cond)
  }

  def timeToLong(time : TimeNumber) : Long = {
    (time.toBigDecimal / SimManagerContext.current.manager.timePrecision).toLong
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

  def forkSensitive(triggers: Data)(block: => Unit): Unit = {
    forkSensitive2(triggers)(block)
  }

  def forkSensitive2(triggers: Data*)(block: => Unit): Unit = {
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

  def delayed(delay: TimeNumber)(body: => Unit) = {
    SimManagerContext.current.manager.schedule(timeToLong(delay))(body)
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
      case bt: Bool               => bt #= simRandom.nextBoolean()
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
        setBigInt(bt, value)
      }
    }

    def toBigInt: BigInt = bt match{
      case bt: Bool               => BigInt(if(bt.toBoolean) 1 else 0)
      case bt: Bits               => bt.toBigInt
      case bt: UInt               => bt.toBigInt
      case bt: SInt               => bt.toBigInt
      case bt: SpinalEnumCraft[_] => BigInt(bt.toEnum.position)
    }

    def toBytes: Array[Byte] = toBigInt.toBytes(bt.getBitsWidth)
  }


  implicit class SimSeqPimper[T](pimped: Seq[T]){
    def randomPick(): T = pimped(simRandom.nextInt(pimped.length))
    def randomPickWithIndex(): (T, Int) = {
      val index = simRandom.nextInt(pimped.length)
      (pimped(index), index)
    }
  }

  implicit class SimArrayBufferPimper[T](pimped: ArrayBuffer[T]){
    def randomPop() : T = {
      val index = simRandom.nextInt(pimped.length)
      val ret = pimped(index)
      pimped(index) = pimped.last
      pimped.remove(pimped.length-1)
      ret
    }
    def pop() : T = {
      val index = 0
      val ret = pimped(index)
      pimped(index) = pimped.last
      pimped.remove(pimped.length-1)
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
    def simProxy() = new SimProxy(bt)
    class SimProxy(bt : Bool){
      val manager = SimManagerContext.current.manager
      val signal = manager.raw.userData.asInstanceOf[ArrayBuffer[Signal]](bt.algoInt)
      def toBoolean = manager.getLong(signal) != 0

      def #=(value: Boolean) : Unit  = {
        manager.setLong(signal, if(value) 1 else 0)
      }
    }

    def toBoolean = getLong(bt) != 0

    def #=(value: Boolean) = setLong(bt, if(value) 1 else 0)

    def randomize(): Boolean = {
      val b = simRandom.nextBoolean()
      bt #= b
      b
    }
  }

  /**
    * Add implicit function to BitVector
    */
  implicit class SimBitVectorPimper(bt: BitVector) {
    def simProxy() = new SimProxy(bt)
    class SimProxy(bt : BitVector){
      val manager = SimManagerContext.current.manager
      val signal = manager.raw.userData.asInstanceOf[ArrayBuffer[Signal]](bt.algoInt)
      val alwaysZero = bt.getBitsWidth == 0
      def toInt = if(alwaysZero) 0 else manager.getInt(signal)
      def toLong = if(alwaysZero) 0 else manager.getLong(signal)
      def toBigInt = if(alwaysZero) 0 else manager.getBigInt(signal)

      def #=(value: Int) : Unit  = {
        if(alwaysZero) {
          assert(value == 0)
          return
        }
        manager.setLong(signal, value)
      }
      def #=(value: Long) : Unit  = {
        if(alwaysZero) {
          assert(value == 0)
          return
        }
        manager.setLong(signal, value)
      }
      def #=(value: BigInt) : Unit = {
        if(alwaysZero) {
          assert(value == 0)
          return
        }
        manager.setBigInt(signal, value)
      }
    }

    def toInt    = getInt(bt)
    def toLong(implicit manager: SimManager = SimManagerContext.current.manager)   = getLong(bt)(manager)
    def toBigInt = getBigInt(bt)
    def toBytes: Array[Byte] = toBigInt.toBytes(bt.getBitsWidth)
    def toBooleans : Array[Boolean] = {
      val width = bt.getBitsWidth
      val ret = new Array[Boolean](width)
      val bi = toBigInt
      for(i <- 0 until width) ret(i) = bi.testBit(i)
      ret
    }

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
    def #=(value: Array[Boolean]) = { //TODO improve perf
      var acc = BigInt(0)
      for(i <- value.size-1 downto 0){
        if(value(i)) acc = acc.setBit(i)
      }
      setBigInt(bt, acc)
    }
  }

  protected abstract class RandomizableBitVector(bt: BitVector, longLimit: Int) {
    protected val width = bt.getWidth

    def randomize(): BigInt = {
      if (width < longLimit) {
        val l = randomizedLong()
        bt #= l
        l
      } else {
        val bi = randomizedBigInt()
        bt #= bi
        bi
      }
    }

    def randomizedBigInt() = BigInt(width, simRandom)

    def randomizedLong() = {
      assert(width < 64)
      simRandom.nextLong() & ((1l << width) - 1)
    }

    def randomizedInt() = {
      assert(width < 32)
      simRandom.nextInt() & ((1 << width) - 1)
    }
  }

  /**
   * Add implicit function to Bits
   */
  implicit class SimBitsPimper(bt: Bits) extends RandomizableBitVector(bt, 64)

  /**
   * Add implicit function to UInt
   */
  implicit class SimUIntPimper(bt: UInt) extends RandomizableBitVector(bt, 64)


  /**
   * Add implicit function to SInt
   */
  implicit class SimSIntPimper(bt: SInt) extends RandomizableBitVector(bt, 65) {
    override def randomizedLong(): Long = {
      assert(width <= 64)
      val shift = 64 - width
      (simRandom.nextLong << shift) >> shift
    }

    override def randomizedInt(): Int = {
      assert(width <= 32)
      val shift = 32 - width
      (simRandom.nextInt() << shift) >> shift
    }

    override def randomizedBigInt(): BigInt = BigInt(width, simRandom) - (BigInt(1) << width - 1)
  }

  /**
    * Add implicit function to Enum
    */
  implicit class SimEnumPimper[T <: SpinalEnum](bt: SpinalEnumCraft[T]) {

    def toEnum = bt.encoding.getElement(getBigInt(bt), bt.spinalEnum).asInstanceOf[SpinalEnumElement[T]]

    def #=(value: SpinalEnumElement[T]) = setBigInt(bt, bt.encoding.getValue(value))

    def randomize(): SpinalEnumElement[T] = {
      val e = bt.spinalEnum.elements(simRandom.nextInt(bt.spinalEnum.elements.length))
      setBigInt(bt, bt.encoding.getValue(e))
      e.asInstanceOf[SpinalEnumElement[T]]
    }
  }

  /**
   * Add implicit function to UFix/SFix/AFix
   */
  abstract class SimFix[T <: XFix[_, _]](bt: T) {
    val fractionLength = -bt.minExp
    val maxRawIntValue : BigInt
    val minRawIntValue : BigInt
    private def maxValue = maxRawIntValue.doubleValue / scala.math.pow(2, fractionLength)
    private def minValue = minRawIntValue.doubleValue / scala.math.pow(2, fractionLength)

    protected def rawAssign(that: BigInt): Unit
    def #= (that: BigDecimal): Unit = {
      val rhs = (that * scala.math.pow(2, fractionLength)).toBigInt
      require(rhs <= maxRawIntValue, s"$that is overflow. Max value allowed is $maxValue")
      require(rhs >= minRawIntValue, s"$that is underflow.Min value allowed is $minValue")
      rawAssign(rhs)
    }
    def #= (that : Double): Unit = this #= BigDecimal(that)
    def randomize(): BigDecimal = {
      var rhs = simRandom.nextDouble()
      rhs = Math.max(minValue, rhs)
      rhs = Math.min(maxValue, rhs)
      this #= rhs
      discretize(rhs)
    }

    def toBigDecimal: BigDecimal
    protected def discretize(d: Double): BigDecimal
    def toDouble: Double = this.toBigDecimal.doubleValue
  }

  implicit class SimUFixPimper(bt: UFix) extends SimFix(bt) {
    private val factor = scala.math.pow(2, fractionLength)
    override val maxRawIntValue = bt.raw.maxValue
    override val minRawIntValue: BigInt = 0

    override protected def rawAssign(that: BigInt): Unit = bt.raw #= that

    override def toBigDecimal: BigDecimal = BigDecimal(bt.raw.toBigInt) / factor

    override def discretize(d: Double): BigDecimal = BigDecimal((BigDecimal(d) * factor).toBigInt) / factor
  }

  implicit class SimSFixPimper(bt: SFix) extends SimFix(bt) {
    private val factor = scala.math.pow(2, fractionLength)
    override val maxRawIntValue = bt.raw.maxValue
    override val minRawIntValue = bt.raw.minValue

    override protected def rawAssign(that: BigInt): Unit = bt.raw #= that

    override def toBigDecimal: BigDecimal = BigDecimal(bt.raw.toBigInt) / factor

    override def discretize(d: Double): BigDecimal = BigDecimal((BigDecimal(d) * factor).toBigInt) / factor
  }

  // todo
  implicit class SimAFixPimper(bt: AFix) {
    val fractionLength = bt.fracWidth
    val maxRawIntValue = bt.maxRaw
    val minRawIntValue = bt.minRaw
    private val factor = scala.math.pow(2, fractionLength)
    private def exp = bt.exp
    private def maxDecimal = BigDecimal(maxRawIntValue) * BigDecimal(2).pow(exp)
    private def minDecimal = BigDecimal(minRawIntValue) * BigDecimal(2).pow(exp)

    def #= (that: BigDecimal): Unit = {
      var rhs = (that * BigDecimal(2).pow(-exp)).toBigInt
      require(rhs <= maxRawIntValue, s"$that is overflow. Max value allowed is $maxDecimal")
      require(rhs >= minRawIntValue, s"$that is underflow.Min value allowed is $minDecimal")

      if (rhs.signum >= 0) {
        bt.raw #= rhs
      } else {
        rhs = (rhs.abs - 1)
        (0 until bt.bitWidth).foreach { idx =>
          rhs = rhs.flipBit(idx)
        }
        bt.raw #= rhs
      }
    }
    def #= (that : Double): Unit = this #= BigDecimal(that)

    def randomize(inRange: Boolean = true): BigDecimal = {
      if (inRange) {
        var randBigInt: BigInt = null
        do {
          if (!bt.signed || !simRandom.nextBoolean()) {
            randBigInt = BigInt(maxRawIntValue.bitLength, simRandom) * maxRawIntValue.signum
          } else {
            randBigInt = BigInt(minRawIntValue.bitLength, simRandom) * minRawIntValue.signum
          }
        } while (randBigInt > maxRawIntValue || randBigInt < minRawIntValue)

        if (randBigInt.signum >= 0) {
          bt.raw #= randBigInt
        } else {
          randBigInt = (randBigInt.abs - 1)
          (0 until bt.bitWidth).foreach { idx =>
            randBigInt = randBigInt.flipBit(idx)
          }
          bt.raw #= randBigInt
        }
        discretize(randBigInt)
      } else {
        val bi = bt.raw.randomizedBigInt()
        bt.raw #= bi
        discretize(bi)
      }
    }

    def toBigDecimal: BigDecimal = discretize(bt.raw.toBigInt)

    def toDouble: Double = this.toBigDecimal.doubleValue

    protected def discretize(raw: BigInt): BigDecimal = {
      if (bt.signed) {
        if (!raw.testBit(bt.numWidth)) {
          BigDecimal(raw) / factor
        } else {
          var tmp = raw
          (0 until bt.bitWidth).foreach { idx =>
            tmp = tmp.flipBit(idx)
          }
          tmp = -(tmp + 1)
          BigDecimal(tmp) / factor
        }
      } else {
        BigDecimal(raw) / factor
      }
    }
  }
  
  /**
    * Add implicit function to BigInt
    */
  implicit class SimBigIntPimper(x: BigInt) {
    /**
      * Convert value to array of bytes
      *
      * Checks if a value fit into the given number of bits, but does not reserve a bit for
      * a possible sign bit if the BigInt is positive.
      * Raises an error if a specific BigInt value doesn't fit into the given number of bits.
      * If the value is negative, the sign bit is extended to the given number of bits (if passed)
      * otherwise to the least number of bytes necessary.
      */
    def toBytes(bits: Int = -1, endian: Endianness = LITTLE): Array[Byte] = {
      val requiredLen = x.bitLength + (if(x < 0) 1 else 0)
      assert(bits < 0 || bits >= requiredLen, "Original BigInt has more bytes then bits specified.")
      // use full bytes if not instructed otherwise
      val outLen = if(bits < 0) (requiredLen + 7) & (-8) else bits
      val out = for (shift <- 0 until outLen by 8) yield {
        val mask = if(outLen - 8 < shift) 0xff >> (8 - (outLen - shift)) else 0xff
        ((x >> shift) & mask).toByte
      }
      if (endian == BIG) out.reverse.toArray else out.toArray
    }
  }

  /**
    * Add implicit function to ClockDomain
    */
  implicit class SimClockDomainPimper(cd: ClockDomain) {

    private def getBool(manager: SimManager, who: Bool): Bool = {
      val component = who.component
      if((who.isInput || who.isOutput) && component != null && component.parent == null || who.hasTag(SimPublic)){
        who
      }else {
        manager.userData.asInstanceOf[Component].pulledDataCache.getOrElse(who, null).asInstanceOf[Bool]
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

    def simAssignSafe(that : Bool, value : Boolean) = if(that != null) that #= value
    def resetSimAssign(value : Boolean)       = simAssignSafe(getBool(SimManagerContext.current.manager, cd.reset), value)
    def clockEnableSimAssign(value : Boolean) = simAssignSafe(getBool(SimManagerContext.current.manager, cd.clockEnable), value)
    def softResetSimAssign(value : Boolean)   = simAssignSafe(getBool(SimManagerContext.current.manager, cd.softReset), value)

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

    //timeout in cycles
    //Warning use threaded API (slow)
    //return true on timeout
    def waitSamplingWhere(timeout : Int)(condAnd: => Boolean): Boolean = {
      var counter = 0
      while(true){
        waitSampling()
        if(condAnd) return false
        counter += 1
        if(counter == timeout) return true
      }
      return ???
    }


    def waitEdge(): Unit = waitEdge(1)
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

      if(cd.hasClockEnableSignalSim) assertClockEnable()
      if(cd.hasSoftResetSignalSim)   deassertSoftReset()

      cd.config.clockEdge match {
        case RISING  => fallingEdge()
        case FALLING => risingEdge()
      }

      if(cd.config.resetKind == ASYNC){
          val dummy = if(cd.hasResetSignalSim){
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
        if(cd.hasResetSignalSim){
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

    def forkStimulus(period: Long, sleepDuration : Int = 0) : Unit = {
      cd.config.clockEdge match {
        case RISING  => fallingEdge()
        case FALLING => risingEdge()
      }
      if(cd.hasResetSignalSim) cd.deassertReset()
      if(cd.hasSoftResetSignalSim) cd.deassertSoftReset()
      if(cd.hasClockEnableSignalSim) cd.deassertClockEnable()
      fork(doStimulus(period))
      if(sleepDuration >= 0) sleep(sleepDuration) //This allows the doStimulus to give initial value to clock/reset before going further
    }

    def forkStimulus(period: TimeNumber): Unit = {
      forkStimulus(timeToLong(period))
    }

    def forkStimulus(frequency: HertzNumber): Unit = forkStimulus(frequency.toTime)

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

    def onSamplingWhile(body : => Boolean) : Unit = {
      val context = SimManagerContext.current
      val edgeValue = if (cd.config.clockEdge == spinal.core.RISING) 1 else 0
      val manager = context.manager
      val signal = getSignal(manager, cd.clock)
      var last = manager.getInt(signal)
      val listeners = ArrayBuffer[() => Unit]()
      forkSensitiveWhile {
        var continue = true
        val current = manager.getInt(signal)
        if (last != edgeValue && current == edgeValue && isSamplingEnable) {
          continue = body
        }
        last = current
        continue
      }
    }


    def hasClockEnableSignalSim = cd.hasClockEnableSignal && clockEnableSim != null
    def hasResetSignalSim       = cd.hasResetSignal && resetSim != null
    def hasSoftResetSignalSim   = cd.hasSoftResetSignal && softResetSim != null

    def assertReset(): Unit         = resetSim #= cd.config.resetActiveLevel == spinal.core.HIGH
    def deassertReset(): Unit       = resetSim #= cd.config.resetActiveLevel != spinal.core.HIGH

    def assertClockEnable(): Unit   = clockEnableSim #= cd.config.clockEnableActiveLevel == spinal.core.HIGH
    def deassertClockEnable(): Unit = clockEnableSim #= cd.config.clockEnableActiveLevel != spinal.core.HIGH

    def assertSoftReset(): Unit     = softResetSim #= cd.config.softResetActiveLevel == spinal.core.HIGH
    def deassertSoftReset(): Unit   = softResetSim #= cd.config.softResetActiveLevel != spinal.core.HIGH


    def isResetAsserted: Boolean         = (cd.hasResetSignalSim && (cd.resetSim.toBoolean ^ cd.config.resetActiveLevel != spinal.core.HIGH)) || (cd.hasSoftResetSignalSim && (cd.softResetSim.toBoolean ^ cd.config.softResetActiveLevel != spinal.core.HIGH))
    def isResetDeasserted: Boolean       =  ! isResetAsserted

    def isClockEnableAsserted: Boolean   = !cd.hasClockEnableSignalSim || (cd.clockEnableSim.toBoolean ^ cd.config.clockEnableActiveLevel != spinal.core.HIGH)
    def isClockEnableDeasserted: Boolean = ! isClockEnableAsserted

    def isSamplingEnable: Boolean        = isResetDeasserted && isClockEnableAsserted
    def isSamplingDisable: Boolean       = ! isSamplingEnable
  }
  implicit class SimClockDomainHandlePimper(cd: spinal.core.fiber.Handle[ClockDomain]) extends SimClockDomainPimper(cd.get)

  def enableSimWave() =  SimManagerContext.current.manager.raw.enableWave()
  def disableSimWave() =  SimManagerContext.current.manager.raw.disableWave()

  case class SimMutex(randomized : Boolean = false){
    val queue = mutable.Queue[SimThread]()
    val array = mutable.ArrayBuffer[SimThread]()
    var locked = false
    def lock() : this.type = {
      if(locked) {
        val t = simThread
        randomized match {
          case false => queue.enqueue(t)
          case true => array += t
        }
        t.suspend()
      } else {
        locked = true
      }
      this
    }

    def unlock() : this.type = {
      assert(locked)
      randomized match {
        case false => {
          if(queue.nonEmpty) {
            queue.dequeue().resume()
          } else {
            locked = false
          }
        }
        case true =>  {
          if(array.nonEmpty) {
            val (t, i) = array.randomPickWithIndex()
            array.remove(i)
            t.resume()
          } else {
            locked = false
          }
        }
      }
      this
    }

    def await() : Unit = {
      if(locked) {
        val t = simThread
        randomized match {
          case false => queue.enqueue(t)
          case true => array += t
        }
        t.suspend()
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
