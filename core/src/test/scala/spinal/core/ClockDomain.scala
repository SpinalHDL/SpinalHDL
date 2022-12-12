package spinal.core

import scala.util.Random

import spinal.core.sim._
import spinal.lib.{BufferCC, Counter, CounterFreeRun, NoData}
import spinal.tester.{SpinalSimFunSuite, SpinalTesterCocotbBase}

object ClockDomainConfigTester {
  class ClockDomainConfigTester() extends Component {
    val clk,syncReset,asyncReset,softReset,enable    = in Bool()
    val clkn,syncResetn,asyncResetn,softResetn,enablen = in Bool()

    class TestArea(cd : ClockDomain,withInit : Boolean = true) extends ClockingArea(cd){
      val regWithoutReset = out(Reg(UInt(8 bits)) randBoot())
      regWithoutReset := regWithoutReset + 1
      val regWithReset = if(withInit) {
        val reg = out(Reg(UInt(8 bits)) init(42) randBoot())
        reg := reg + 1
        reg
      } else null
    }

    val test_clk = new TestArea(ClockDomain(
      clock = clk,
      config = ClockDomainConfig(
        clockEdge = RISING,
        resetKind = ASYNC,
        resetActiveLevel = HIGH,
        softResetActiveLevel = HIGH,
        clockEnableActiveLevel= HIGH
      )
    ),false)

    val test_clkn = new TestArea(ClockDomain(
      clock = clkn,
      config = ClockDomainConfig(
        clockEdge = FALLING,
        resetKind = ASYNC,
        resetActiveLevel = HIGH,
        softResetActiveLevel = HIGH,
        clockEnableActiveLevel= HIGH
      )
    ),false)


    val test_clk_boot = new TestArea(ClockDomain(
      clock = clk,
      config = ClockDomainConfig(
        clockEdge = RISING,
        resetKind = BOOT,
        resetActiveLevel = HIGH,
        softResetActiveLevel = HIGH,
        clockEnableActiveLevel= HIGH
      )
    ))

    val test_async_reset = new TestArea(ClockDomain(
      clock = clk,
      reset = asyncReset,
      config = ClockDomainConfig(
        clockEdge = RISING,
        resetKind = ASYNC,
        resetActiveLevel = HIGH,
        softResetActiveLevel = HIGH,
        clockEnableActiveLevel= HIGH
      )
    ))

    val test_async_resetn = new TestArea(ClockDomain(
      clock = clk,
      reset = asyncResetn,
      config = ClockDomainConfig(
        clockEdge = RISING,
        resetKind = ASYNC,
        resetActiveLevel = LOW,
        softResetActiveLevel = HIGH,
        clockEnableActiveLevel= HIGH
      )
    ))

    val test_sync_reset = new TestArea(ClockDomain(
      clock = clk,
      reset = syncReset,
      config = ClockDomainConfig(
        clockEdge = RISING,
        resetKind = SYNC,
        resetActiveLevel = HIGH,
        softResetActiveLevel = HIGH,
        clockEnableActiveLevel= HIGH
      )
    ))

    val test_sync_resetn = new TestArea(ClockDomain(
      clock = clk,
      reset = syncResetn,
      config = ClockDomainConfig(
        clockEdge = RISING,
        resetKind = SYNC,
        resetActiveLevel = LOW,
        softResetActiveLevel = HIGH,
        clockEnableActiveLevel= HIGH
      )
    ))

    val test_enable = new TestArea(ClockDomain(
      clock = clk,
      clockEnable = enable,
      config = ClockDomainConfig(
        clockEdge = RISING,
        resetKind = ASYNC,
        resetActiveLevel = HIGH,
        softResetActiveLevel = HIGH,
        clockEnableActiveLevel= HIGH
      )
    ),false)

    val test_enablen = new TestArea(ClockDomain(
      clock = clk,
      clockEnable = enablen,
      config = ClockDomainConfig(
        clockEdge = RISING,
        resetKind = ASYNC,
        resetActiveLevel = HIGH,
        softResetActiveLevel = HIGH,
        clockEnableActiveLevel= LOW
      )
    ),false)

    val test_sync_reset_enable = new TestArea(ClockDomain(
      clock = clk,
      reset = syncReset,
      clockEnable = enable,
      config = ClockDomainConfig(
        clockEdge = RISING,
        resetKind = SYNC,
        resetActiveLevel = HIGH,
        softResetActiveLevel = HIGH,
        clockEnableActiveLevel= HIGH
      )
    ))

    val test_softReset = new TestArea(ClockDomain(
      clock = clk,
      softReset = softReset,
      config = ClockDomainConfig(
        clockEdge = RISING,
        resetKind = ASYNC,
        resetActiveLevel = HIGH,
        softResetActiveLevel = HIGH,
        clockEnableActiveLevel= HIGH
      )
    ))

    val test_softResetn = new TestArea(ClockDomain(
      clock = clk,
      softReset = softResetn,
      config = ClockDomainConfig(
        clockEdge = RISING,
        resetKind = ASYNC,
        resetActiveLevel = HIGH,
        softResetActiveLevel = LOW,
        clockEnableActiveLevel= HIGH
      )
    ))

    val test_async_reset_softReset = new TestArea(ClockDomain(
      clock = clk,
      reset = asyncReset,
      softReset = softReset,
      config = ClockDomainConfig(
        clockEdge = RISING,
        resetKind = ASYNC,
        resetActiveLevel = HIGH,
        softResetActiveLevel = HIGH,
        clockEnableActiveLevel= HIGH
      )
    ))

    val test_sync_reset_softReset = new TestArea(ClockDomain(
      clock = clk,
      reset = syncReset,
      softReset = softReset,
      config = ClockDomainConfig(
        clockEdge = RISING,
        resetKind = SYNC,
        resetActiveLevel = HIGH,
        softResetActiveLevel = HIGH,
        clockEnableActiveLevel= HIGH
      )
    ))



  }

}

class ClockDomainConfigTesterCocotbBoot extends SpinalTesterCocotbBase {
  override def getName: String = "ClockDomainConfigTester"
  override def pythonTestLocation: String = "tester/src/test/python/spinal/ClockDomainConfigTester"
  override def createToplevel: Component = new ClockDomainConfigTester.ClockDomainConfigTester()
}

class MultiClockTester extends Component {
  import spinal.lib._
  
  class BundleA extends Bundle{
    val a = UInt(8 bit)
    val b = Bool()
  }

  val io = new Bundle {
    val clkA = in Bool()
    val resetA = in Bool()
    val clkB = in Bool()
    val resetB = in Bool()

    val slave0 = slave Stream(new BundleA)
    val master0 = master Stream(new BundleA)
    val fifo0_pushOccupancy = out UInt()
    val fifo0_popOccupancy = out UInt()
  }

  val clockDomainA = ClockDomain(io.clkA,io.resetA)
  val clockDomainB = ClockDomain(io.clkB,io.resetB)

  val fifo0 = new StreamFifoCC(new BundleA,16,clockDomainA,clockDomainB)
  fifo0.io.push << io.slave0
  fifo0.io.pop >> io.master0
  io.fifo0_pushOccupancy := fifo0.io.pushOccupancy
  io.fifo0_popOccupancy := fifo0.io.popOccupancy

}

class MultiClockTesterCocotbBoot extends SpinalTesterCocotbBase {
  override def getName: String = "MultiClockTester"
  override def pythonTestLocation: String = "tester/src/test/python/spinal/MultiClockTester"
  override def createToplevel: Component = new MultiClockTester
}

object SpinalSimClockDomainTest{
  class SpinalSimClockDomainTest1 extends Component {
    val io = new Bundle {
      val mClk, mReset = in Bool()
      val a, b, c = in UInt (8 bits)
      val result = out UInt (8 bits)
    }

    val tmpClk, tmpReset = Bool()
    tmpClk := io.mClk
    tmpReset := io.mReset
    ClockDomain(tmpClk, tmpReset){
      io.result := RegNext(io.a + io.b - io.c) init(0)
    }
  }

  class SpinalSimClockDomainTest2 extends Component {
    val io = new Bundle {
      val mClk, mReset = in Bool()
      val a, b, c = in UInt (8 bits)
      val result = out UInt (8 bits)
    }

    ClockDomain(io.mClk, io.mReset){
      io.result := RegNext(io.a + io.b - io.c) init(0)
    }
  }
  class SpinalSimClockDomainTest3 extends Component {
    val io = new Bundle {
      val a, b, c = in UInt (8 bits)
      val result = out UInt (8 bits)
    }

    io.result := RegNext(io.a + io.b - io.c) init(0)
  }

  class SpinalSimClockDomainTest4 extends Component {
    val io = new Bundle {
      val enable = in Bool()
      val result = out UInt (8 bits)
    }

    val reg = RegInit(U(42, 8 bits))
    when(io.enable){
      reg := reg + 1
    }
    io.result := reg
  }
}

class SpinalSimClockDomainTest extends SpinalSimFunSuite {
  val resetKinds = List(SYNC,ASYNC)
  test("Test1") {
    for (resetKind <- resetKinds) {
      val compiled = SimConfig
        .withConfig(SpinalConfig(defaultConfigForClockDomains = ClockDomainConfig(resetKind = resetKind)))
        .compile(new SpinalSimClockDomainTest.SpinalSimClockDomainTest1().setDefinitionName("SpinalSimClockDomainTest1" + resetKind.getClass.getSimpleName.toString.take(4)))
        .doSim(resetKind.toString) { dut =>
          //        dut.clockDomain.forkStimulus(period = 10)
          val cd = ClockDomain(dut.io.mClk, dut.io.mReset)
          cd.forkStimulus(period = 10)

          for (repeat2 <- 0 until 10000) {
            val a, b, c = Random.nextInt(256)
            dut.io.a #= a
            dut.io.b #= b
            dut.io.c #= c
            cd.waitActiveEdge(); sleep(0)
            if (cd.isResetDeasserted) assert(dut.io.result.toInt == ((a + b - c) & 0xFF))
          }
        }
    }
  }

  test("TestDeltaCycle wake") {
    for (resetKind <- resetKinds) {
      val compiled = SimConfig
        .withConfig(SpinalConfig(defaultConfigForClockDomains = ClockDomainConfig(resetKind = resetKind)))
        .compile(new SpinalSimClockDomainTest.SpinalSimClockDomainTest1().setDefinitionName("SpinalSimClockDomainTest1" + resetKind.getClass.getSimpleName.toString.take(4)))
        .doSim(resetKind.toString) { dut =>
          //        dut.clockDomain.forkStimulus(period = 10)
          val cd = ClockDomain(dut.io.mClk, dut.io.mReset)
          dut.io.a #= 0
          dut.io.b #= 0
          dut.io.c #= 0
          sleep(10)
          cd.forkStimulus(period = 10)
          cd.waitSampling()
          cd.waitSampling()
          dut.io.a #= 42
          cd.waitSampling()
          assert(dut.io.result.toInt == 0) //Wakeup while rising edge, but before FF got the result out
          sleep(0)
          assert(dut.io.result.toInt == 42)
        }
    }
  }


  test("Test2") {
    for (resetKind <- resetKinds) {
      SimConfig
        .withConfig(SpinalConfig(defaultConfigForClockDomains = ClockDomainConfig(resetKind = resetKind)))
        .compile((new SpinalSimClockDomainTest.SpinalSimClockDomainTest2().setDefinitionName("SpinalSimClockDomainTest2" + resetKind.getClass.getSimpleName.toString.take(4))))
        .doSim(resetKind.toString) { dut =>
          //        dut.clockDomain.forkStimulus(period = 10)
          val cd = ClockDomain(dut.io.mClk, dut.io.mReset)
          cd.forkStimulus(period = 10)

          var counter = 0
          while (true) {
            val a, b, c = Random.nextInt(256)
            dut.io.a #= a
            dut.io.b #= b
            dut.io.c #= c
            cd.waitActiveEdge();
            sleep(0)
            if (cd.isResetDeasserted) assert(dut.io.result.toInt == ((a + b - c) & 0xFF))
            counter += 1
            if (counter == 10000) simSuccess()
          }
        }
    }
  }

  test("Test3") {
    for (resetKind <- resetKinds) {
      SimConfig
        .withConfig(SpinalConfig(defaultConfigForClockDomains = ClockDomainConfig(resetKind = resetKind)))
        .compile((new SpinalSimClockDomainTest.SpinalSimClockDomainTest3().setDefinitionName("SpinalSimClockDomainTest3" + resetKind.getClass.getSimpleName.toString.take(4))))
        .doSim(resetKind.toString) { dut =>
          dut.clockDomain.forkStimulus(period = 10)
          var model = BigInt(0)
          for (repeat <- 0 until 10000) {
            dut.io.a.randomize()
            dut.io.b.randomize()
            dut.io.c.randomize()
            dut.clockDomain.waitActiveEdge()
            if (dut.clockDomain.isResetDeasserted) {
              assert(dut.io.result.toInt == model)
              model = ((dut.io.a.toBigInt + dut.io.b.toLong - dut.io.c.toInt) & 0xFF)
            }
          }
        }
    }
  }


  test("Test4") {
    SimConfig
      .doSim(new SpinalSimClockDomainTest.SpinalSimClockDomainTest4) { dut =>
        dut.clockDomain.forkStimulus(period = 10)
        var model = 42
        for (repeat <- 0 until 10000) {
          dut.io.enable.randomize()
          dut.clockDomain.waitActiveEdge(); sleep(0)
          if (dut.io.enable.toBoolean) model = (model + 1) & 0xFF
          assert(dut.io.result.toInt == model)
        }
      }
  }

  test("Test5") {
    SimConfig
      .doSim(new SpinalSimClockDomainTest.SpinalSimClockDomainTest4) { dut =>
        dut.clockDomain.forkStimulus(period = 10)
        var model = 42
        dut.io.enable #= false
        dut.clockDomain.waitActiveEdge(1)
        for (repeat <- 0 until 10000) {
          dut.io.enable.randomize()
          val waited = Random.nextInt(10)
          dut.clockDomain.waitActiveEdge(waited); sleep(0)
          if (dut.io.enable.toBoolean) model = (model + waited) & 0xFF
          assert(dut.io.result.toInt == model)
        }
      }
  }
}

object InternalClockTester{
  class ClockGeneratorSub extends Component{
    val io = NoData

    val internalClock = CounterFreeRun(16)(0)

    val initCounter = RegInit(U(7))
    val srcReset = initCounter =/= U(0)
    when(srcReset){
      initCounter := initCounter - U(1)
    }
    val internalClk = ClockDomain(internalClock)
    val internalReset = Bool()

    val resetBuffer = new ClockingArea(internalClk){
      val internalReset = BufferCC(srcReset)
    }
    internalReset := resetBuffer.internalReset

    val internalClockDomain = ClockDomain(internalClock,internalReset)
  }

  class ClockGenerator extends Component{
    val io = NoData
    val sub = new ClockGeneratorSub
  }

  class CounterComponentSub extends Component{
    val io = new Bundle{
      val counter = out UInt(8 bit)
    }
    val counter = Counter(256)
    counter.increment()
    io.counter := counter
  }

  class CounterComponent extends Component{
    val io = new Bundle{
      val counter = out UInt(8 bit)
    }
    val sub = new CounterComponentSub
    io <> sub.io
  }
}

class InternalClockTester extends Component {
  val io = new Bundle {
    val internalClkCounter = out UInt(8 bit)
  }
  val clockGenerator = new InternalClockTester.ClockGenerator

  val internClockDomain = new ClockingArea(clockGenerator.sub.internalClockDomain){
    val counterComponent = new InternalClockTester.CounterComponent
    io.internalClkCounter := counterComponent.io.counter
  }
}

class InternalClockTesterCocotbBoot extends SpinalTesterCocotbBase {
  override def getName: String = "InternalClockTester"
  override def pythonTestLocation: String = "tester/src/test/python/spinal/InternalClockTester"
  override def createToplevel: Component = new InternalClockTester
}
