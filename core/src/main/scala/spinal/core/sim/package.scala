package spinal.core

import spinal.core.sim.{SimBaseTypePimper, SpinalSimConfig}
import spinal.sim._

import scala.collection.mutable.ArrayBuffer
import scala.util.Random
import scala.util.continuations.cps

package object sim {
  type suspendable = cps[Unit]

  def SimConfig: SpinalSimConfig = new SpinalSimConfig()

  @deprecated("Use SimConfig.???.compile(new Dut) instead")
  def SimConfig[T <: Component](rtl:  => T) : SimConfigLegacy[T] ={
    new SimConfigLegacy[T](_rtlGen = Some(() => rtl))
  }

  @deprecated("Use SimConfig.???.compile(new Dut) instead")
  def SimConfig[T <: Component](rtl: SpinalReport[T]) : SimConfigLegacy[T] ={
    new SimConfigLegacy[T](_spinalReport = Some(rtl))
  }



  private def btToSignal(manager : SimManager, bt : BaseType) = {
    if(bt.algoIncrementale != -1){
      SimError(s"UNACCESSIBLE SIGNAL : $bt isn't accessible during the simulation.\n- To fix it, call simPublic() on it durring the elaboration.")
    }
    manager.raw.userData.asInstanceOf[ArrayBuffer[Signal]](bt.algoInt)
  }

  private def getInt(bt : BaseType) : Int = {
    if(bt.getBitsWidth == 0) return 0
    val manager = SimManagerContext.current.manager
    val signal = btToSignal(manager, bt)
    manager.getInt(signal)
  }

  private def getLong(bt : BaseType) : Long = {
    if(bt.getBitsWidth == 0) return 0l
    val manager = SimManagerContext.current.manager
    val signal = btToSignal(manager, bt)
    manager.getLong(signal)
  }

  private def getBigInt(bt : BaseType) : BigInt = {
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
  def simSuccess() : Unit = throw new SimSuccess()
  def simFailure(message : String = "") : Unit = throw new SimFailure(message)
//  def simExit(): Unit@suspendable = SimManagerContext.current.manager.exitSim()
  def sleep(cycles : Long) : Unit@suspendable = SimManagerContext.current.thread.sleep(cycles)
  def waitUntil(cond : => Boolean) : Unit@suspendable = SimManagerContext.current.thread.waitUntil(cond)
  def fork(body : => Unit@suspendable) : SimThread = SimManagerContext.current.manager.newThread(body)
  def forkJoin(bodys : (()=> Unit@suspendable)*) : Unit@suspendable = {
    val threads = bodys.map(body => fork(body()))
    threads.suspendable.foreach(thread => thread.join())
  }

  implicit class SimBaseTypePimper(bt : BaseType) {
    def randomize() : Unit = bt match{
      case bt : Bool => bt #= Random.nextBoolean()
      case bt : Bits => bt.randomize()
      case bt : UInt => bt.randomize()
      case bt : SInt => bt.randomize()
      case bt : SpinalEnumCraft[_] => bt.randomize()
    }


    def assignBigInt(value  : BigInt) : Unit = bt match{
      case bt : Bool => bt #= (if(value == 0) false else if(value == 1) true else throw new Exception("Value outide the range"))
      case bt : BitVector => bt #= value
      case bt : SpinalEnumCraft[_] => {
        assert(value < bt.spinalEnum.elements.length)
        bt #= bt.spinalEnum.elements(value.toInt)
      }
    }


    def toBigInt : BigInt = bt match{
      case bt : Bool =>  BigInt(if(bt.toBoolean) 1 else 0)
      case bt : Bits => bt.toBigInt
      case bt : UInt => bt.toBigInt
      case bt : SInt => bt.toBigInt
      case bt : SpinalEnumCraft[_] => BigInt(bt.toEnum.position)
    }
  }

  implicit class SimDataPimper(bt : Data) {
    def randomize() : Unit = bt.flattenForeach(_.randomize())
    def simPublic() : Unit = bt.addTag(SimPublic)
  }

  implicit class SimBoolPimper(bt : Bool) {
    def toBoolean = if(getLong(bt) != 0) true else false
    def #=(value : Boolean) = setLong(bt, if(value) 1 else 0)
    def randomize() : Unit = {
      bt #= Random.nextBoolean()
    }
  }

  implicit class SimBitVectorPimper(bt : BitVector) {
    def toInt = getInt(bt)
    def toLong = getLong(bt)
    def toBigInt = getBigInt(bt)
    def #=(value : Int) = setLong(bt, value)
    def #=(value : Long) = setLong(bt, value)
    def #=(value : BigInt) = setBigInt(bt, value)
  }

  implicit class SimBitsPimper(bt : Bits) {
    def randomize() : Unit = {
      val width = bt.getWidth
      if(width < 64){
        bt #= Random.nextLong() & ((1l << width) - 1)
      }else {
        bt #= BigInt(width, Random)
      }
    }
  }

  implicit class SimUIntPimper(bt : UInt) {
    def randomize() : Unit = {
      val width = bt.getWidth
      if(width < 64){
        bt #= Random.nextLong() & ((1l << width) - 1)
      }else {
        bt #= BigInt(width, Random)
      }
    }
  }


  implicit class SimSIntPimper(bt : SInt) {
    def randomize() : Unit = {
      val width = bt.getWidth
      if(width <= 64){
        val shift = 64 - width
        bt #= (Random.nextLong() << shift) >> shift
      }else {
        bt #= BigInt(width, Random) - (BigInt(1) << width-1)
      }
    }
  }

  implicit class SimEnumPimper[T <: SpinalEnum](bt : SpinalEnumCraft[T]) {
    def toEnum = bt.encoding.getElement(getBigInt(bt), bt.spinalEnum)
    def #=(value : SpinalEnumElement[T]) = setBigInt(bt, bt.encoding.getValue(value))
    def randomize(): Unit ={
      bt #= bt.spinalEnum.elements(Random.nextInt(bt.spinalEnum.elements.length))
    }
  }


  implicit class SimClockDomainPimper(cd : ClockDomain) {
    private def getBool(manager : SimManager, who : Bool): Bool ={
      val component = who.component
      if(who.isInput && component != null && component.parent == null){
        who
      }else {
        manager.userData.asInstanceOf[Component].pulledDataCache(who).asInstanceOf[Bool]
      }
    }

    private def getSignal(manager : SimManager, who : Bool): Signal ={
      val bt = getBool(manager, who)
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

    def onSampling(body : => Unit): Unit ={
      val edgeValue = if(cd.config.clockEdge == spinal.core.RISING) 1l else 0l
      val manager = SimManagerContext.current.manager
      val signal = getSignal(manager, cd.clock)
      var last = manager.getLong(signal)
      manager.sensitivities += (new SimManagerSensitive {
        override def update() = {
          val current = manager.getLong(signal)
          if(last != edgeValue && current == edgeValue && isSamplingEnable)
            body
          last = current
          true
        }
      })
    }
//    def onSamplingFork(body : => Unit@suspendable): Unit = onSampling(fork(body))

      def waitSampling() : Unit@suspendable = waitSampling(1)
    def waitSampling(count : Int): Unit@suspendable  ={
      val edgeValue = if(cd.config.clockEdge == spinal.core.RISING) 1l else 0l
      val manager = SimManagerContext.current.manager
      val signal = getSignal(manager, cd.clock)
      var last = manager.getLong(signal)
      var counter = 0
      waitUntil{
        val current = manager.getLong(signal)
        if(last != edgeValue && current == edgeValue && isSamplingEnable)
          counter += 1
        last = current
        counter == count
      }
    }

    def waitSamplingWhere(condAnd : => Boolean): Unit@suspendable  ={
      val edgeValue = if(cd.config.clockEdge == spinal.core.RISING) 1l else 0l
      val manager = SimManagerContext.current.manager
      val signal = getSignal(manager, cd.clock)
      var last = manager.getLong(signal)
      waitUntil{
        val current = manager.getLong(signal)
        val cond = last != edgeValue && current == edgeValue && condAnd
        last = current
        cond
      }
    }


    def waitEdge() : Unit@suspendable = waitRisingEdge(1)
    def waitEdge(count : Int): Unit@suspendable  ={
      val manager = SimManagerContext.current.manager
      val signal = getSignal(manager, cd.clock)
      var last = manager.getLong(signal)
      var counter = 0
      waitUntil{
        val current = manager.getLong(signal)
        if(last != current)
          counter += 1
        last = current
        counter == count
      }
    }


    def waitEdgeWhere(condAnd : => Boolean): Unit@suspendable  ={
      val manager = SimManagerContext.current.manager
      val signal = getSignal(manager, cd.clock)
      var last = manager.getLong(signal)
      waitUntil{
        val current = manager.getLong(signal)
        val cond = last != current && condAnd
        last = current
        cond
      }
    }


    def waitRisingEdge() : Unit@suspendable = waitRisingEdge(1)
    def waitRisingEdge(count : Int): Unit@suspendable  ={
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

    def waitRisingEdgeWhere(condAnd : => Boolean): Unit@suspendable  ={
      val manager = SimManagerContext.current.manager
      val signal = getSignal(manager, cd.clock)
      var last = manager.getLong(signal)
      waitUntil{
        val current = manager.getLong(signal)
        val cond = last == 0l && current == 1l && condAnd
        last = current
        cond
      }
    }

    def waitFallingEdge() : Unit@suspendable = waitFallingEdge(1)
    def waitFallingEdge(count : Int = 1): Unit@suspendable  ={
      val manager = SimManagerContext.current.manager
      val signal = getSignal(manager, cd.clock)
      var last = manager.getLong(signal)
      var counter = 0
      waitUntil{
        val current = manager.getLong(signal)
        if(last == 1l && current == 0l)
          counter += 1
        last = current
        counter == count
      }
    }

    def waitFallingEdgeWhere(condAnd : => Boolean): Unit@suspendable  ={
      val manager = SimManagerContext.current.manager
      val signal = getSignal(manager, cd.clock)
      var last = manager.getLong(signal)
      waitUntil{
        val current = manager.getLong(signal)
        val cond = last == 1l && current == 0l && condAnd
        last = current
        cond
      }
    }

    def waitActiveEdge() : Unit@suspendable = waitActiveEdge(1)
    def waitActiveEdge(count : Int = 1): Unit@suspendable  = {
      if(cd.config.clockEdge == spinal.core.RISING)
        waitRisingEdge(count)
      else
        waitFallingEdge(count)
    }

    def waitActiveEdgeWhere(condAnd : => Boolean): Unit@suspendable  = {
      if(cd.config.clockEdge == spinal.core.RISING)
        waitRisingEdgeWhere(condAnd)
      else
        waitFallingEdgeWhere(condAnd)
    }



    def doStimulus(period : Long) : Unit@suspendable ={
      assert(period >= 2)
      if(cd.hasClockEnableSignal) assertClockEnable()
      if(cd.hasSoftResetSignal) deassertSoftReset()
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
          cd.deassertReset()
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
    def forkSimSpeedPrinter(printPeriod : Double = 1.0) = SimSpeedPrinter(cd, printPeriod)

    def assertReset() : Unit = resetSim #= cd.config.resetActiveLevel == spinal.core.HIGH
    def deassertReset() : Unit = resetSim #= cd.config.resetActiveLevel != spinal.core.HIGH
    def assertClockEnable() : Unit = clockEnableSim #= cd.config.clockEnableActiveLevel == spinal.core.HIGH
    def deassertClockEnable() : Unit = clockEnableSim #= cd.config.clockEnableActiveLevel != spinal.core.HIGH
    def assertSoftReset() : Unit = softResetSim #= cd.config.softResetActiveLevel == spinal.core.HIGH
    def deassertSoftReset() : Unit = softResetSim #= cd.config.softResetActiveLevel != spinal.core.HIGH

    def isResetAsserted : Boolean = (cd.hasResetSignal && (cd.resetSim.toBoolean ^ cd.config.resetActiveLevel != spinal.core.HIGH)) || (cd.hasSoftResetSignal && (cd.softResetSim.toBoolean ^ cd.config.softResetActiveLevel != spinal.core.HIGH))
    def isResetDeasserted : Boolean = ! isResetAsserted
    def isClockEnableAsserted : Boolean = !cd.hasClockEnableSignal || (cd.clockEnable.toBoolean ^ cd.config.clockEnableActiveLevel != spinal.core.HIGH)
    def isClockEnableDeasserted : Boolean = ! isClockEnableAsserted
    def isSamplingEnable: Boolean = isResetDeasserted && isClockEnableAsserted
    def isSamplingDisable: Boolean = ! isSamplingEnable
  }
}
