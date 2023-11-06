package spinal.lib.misc.pipeline

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.sim.{StreamDriver, StreamMonitor, StreamReadyRandomizer}
import spinal.tester.SpinalAnyFunSuite

import scala.collection.mutable

class PipelineTester extends SpinalAnyFunSuite{

  // This a simple pipeline composed of
  // - 3 CtrlConnector connected by 2 register stages
  // - up / down streams to feed and read the pipeline
  class SimplePipeline extends Component{
    val c0 = CtrlConnector()
    val c1 = CtrlConnector()
    val c2 = CtrlConnector()

    val s01 = StageConnector(c0.down, c1.up)
    val s12 = StageConnector(c1.down, c2.up)

    val IN = Stageable(UInt(16 bits))
    val OUT = Stageable(UInt(16 bits))

    val up = slave Stream (IN)
    val down = master Stream (OUT)

    c0.up.driveFrom(up)((self, payload) => self(IN) := payload)
    c2.down.toStream(down)((payload, self) => payload := self(OUT))

    val connectors = List(c0, c1, c2, s01, s12)

    override def postInitCallback() = {
      Builder(connectors)
      super.postInitCallback()
    }

    // The user just need to implement that callback to specify what should be expected
    // on the down stream in function of what is pushed
    def simpleTest(onPush : (Int, mutable.Queue[Int]) => Unit): Unit = {
      val dut = this
      val cd = dut.clockDomain
      cd.forkStimulus(10)
      var coverage = 0

      val refQueue = mutable.Queue[Int]()
      StreamDriver(dut.up, cd) { p =>
        p.randomize()
        true
      }.setFactorPeriodically(500)
      StreamMonitor(dut.up, cd) { p =>
        onPush(p.toInt, refQueue)
      }

      StreamReadyRandomizer(dut.down, cd).setFactorPeriodically(500)
      StreamMonitor(dut.down, cd) { p =>
        val got = p.toInt
        assert(got == refQueue.dequeue())
        coverage += 1
        if (coverage == 1000) simSuccess()
      }
    }
  }

  // Remove C1 transactions which have the LSB set
  test("throw") {
    SimConfig.compile(new SimplePipeline {
      c1.throwWhen(c1(IN).lsb)
      c1(OUT) := c1(IN)
    }).doSimUntilVoid { dut =>
      dut.simpleTest { (value, queue) =>
        if ((value & 1) == 0) {
          queue += value
        }
      }
    }
  }

  // Duplicate the c1 transaction IN(1 downto 0) times
  test("duplicateWhen") {
    SimConfig.compile(new SimplePipeline {
      val counter = Reg(UInt(2 bits)) init (0)
      val last = counter === c1(IN)(counter.bitsRange)
      c1.duplicateWhen(!last)
      when(c1.down.isFireing) {
        counter := counter + 1
        when(last) {
          counter := 0
        }
      }
      c1(OUT) := c1(IN)
    }).doSimUntilVoid { dut =>
      dut.simpleTest { (value, queue) =>
        for (i <- 0 to value & 3) {
          queue += value
        }
      }
    }
  }

  // Block c1 transaction 3 cycles
  test("halt") {
    SimConfig.compile(new SimplePipeline {
      val counter = Reg(UInt(16 bits)) init(0)
      val done = counter(1 downto 0) === 3
      c1.haltWhen(!done)
      when(c1.up.isValid){
        when(!done || c1.down.ready) {
          counter := counter + 1
        }
      }
      c1(OUT) := c1(IN) + counter
    }).doSimUntilVoid { dut =>
      var counter = 0
      dut.simpleTest { (value, queue) =>
        queue += (value + counter + 3) & 0xFFFF
        counter += 4
      }
    }
  }

  // Accumulate the value of 4 transaction in C1 and use the sum as result
  test("terminateWhen") {
    SimConfig.compile(new SimplePipeline {
      val accumulator = Reg(UInt(16 bits)) init (0)
      val counter = Reg(UInt(2 bits)) init(0)
      val last = counter.andR
      val adder = accumulator + c1(IN)
      c1.terminateWhen(!last)
      when(c1.up.isFireing) {
        counter := counter + 1
        accumulator := adder
        when(last){
          accumulator := 0
        }
      }
      c1(OUT) := adder
    }).doSimUntilVoid { dut =>
      var counter = 0
      var accumulator = 0

      dut.simpleTest { (value, queue) =>
        accumulator += value
        if(counter == 3)  {
          queue += accumulator & 0xFFFF
          accumulator = 0
        }
        counter += 1
        counter &= 3
      }
    }
  }


  // There is a state register written late in C2
  // But that state register is readed in C0
  // and we want to make that readed value being updated in C0 / C1 by each transaction in C2
  // This is done using the Ctrl.bypass function
  test("bypass") {
    SimConfig.compile(new SimplePipeline {
      val state = Reg(UInt(16 bits)) init(0)
      c0.up(OUT) := state

      def addBypassOn(ctrl : CtrlConnector): Unit = {
        when(c2.down.valid){
          ctrl.bypass(OUT) := c2(IN)
        }
      }

      addBypassOn(c0)
      addBypassOn(c1)

      when(c2.down.isFireing){
        state := c2(IN)
      }
    }).doSimUntilVoid { dut =>
      var state = 0
      dut.simpleTest { (value, queue) =>
        queue += state
        state = value
      }
    }
  }
}
