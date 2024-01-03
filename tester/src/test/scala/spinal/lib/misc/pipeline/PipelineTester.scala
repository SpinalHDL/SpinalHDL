package spinal.lib.misc.pipeline

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.sim.{FlowDriver, FlowMonitor, StreamDriver, StreamMonitor, StreamReadyRandomizer}
import spinal.tester.SpinalAnyFunSuite

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class PipelineTester extends SpinalAnyFunSuite{


  test("node") {
    SimConfig.compile(new Component {
      val node = Node()
      val X = Payload(UInt(8 bits))
      node.apply(X) := 42
    }).doSimUntilVoid { dut =>
      simSuccess()
    }
  }

  // The user just need to implement that callback to specify what should be expected
  // on the down stream in function of what is pushed on the up stream
  def simpleTest(cd : ClockDomain, up : Any, down : Any)(onPush: (Int, mutable.Queue[Int]) => Unit) = new Area{
    cd.forkStimulus(10)
    var coverage = 0

    val refQueue = mutable.Queue[Int]()
    up match {
      case up : Stream[UInt] => {
        StreamDriver(up, cd) { p =>
          p.randomize()
          true
        }.setFactorPeriodically(500)
        StreamMonitor(up, cd) { p =>
          onPush(p.toInt, refQueue)
        }
      }
      case up: Flow[UInt] => {
        FlowDriver(up, cd) { p =>
          p.randomize()
          true
        }.setFactorPeriodically(500)
        FlowMonitor(up, cd) { p =>
          onPush(p.toInt, refQueue)
        }
      }
      case up: UInt => {
        cd.onSamplings {
          onPush(up.toInt, refQueue)
          up.randomize()
        }
      }
    }

    down match {
      case down: Stream[UInt] => {
        StreamReadyRandomizer(down, cd).setFactorPeriodically(500)
        StreamMonitor(down, cd) { p =>
          val got = p.toInt
          assert(got == refQueue.dequeue())
          coverage += 1
          if (coverage == 1000) simSuccess()
        }
      }
      case down: Flow[UInt] => {
        FlowMonitor(down, cd) { p =>
          val got = p.toInt
          assert(got == refQueue.dequeue())
          coverage += 1
          if (coverage == 1000) simSuccess()
        }
      }
      case down: UInt => {
        cd.onSamplings{
          val got = down.toInt
          assert(got == refQueue.dequeue())
          coverage += 1
          if (coverage == 1000) simSuccess()
        }
      }
    }
  }


  // Simple pipeline composed of 4 nodes connected by 3 register stages
  class NodePipeline extends Component {
    val n0, n1, n2, n3 = Node()

    val s01 = StageLink(n0, n1)
    val s12 = StageLink(n1, n2)
    val s23 = StageLink(n2, n3)

    val IN = Payload(UInt(16 bits))
    val OUT = Payload(UInt(16 bits))

    val up = slave Stream (IN)
    val down = master Stream (OUT)

    n0.driveFrom(up)((self, payload) => self(IN) := payload)
    n3.driveTo(down)((payload, self) => payload := self(OUT))

    val connectors = List(s01, s12, s23)
    afterElaboration(Builder(connectors))

    def testIt(onPush: (Int, mutable.Queue[Int]) => Unit): Unit = simpleTest(clockDomain, up, down)(onPush)
  }

  test("nodeSimple") {
    SimConfig.compile(new NodePipeline {
      n1(OUT) := n1(IN)
    }).doSimUntilVoid { dut =>
      dut.testIt { (value, queue) =>
        queue += value
      }
    }
  }

  test("nodeTmp") {
    SimConfig.compile(new NodePipeline {
      val TMP = n1.insert(n1(IN) + 4)
      n2(OUT) := n2(TMP)+45
    }).doSimUntilVoid { dut =>
      dut.testIt { (value, queue) =>
        queue += (value + 4 + 45) & 0xFFFF
      }
    }
  }


  // This a simple pipeline composed of
  // - 3 CtrlConnector connected by 2 register stages
  // - up / down streams to feed and read the pipeline
  class CtrlPipeline extends Component{
    val c0 = CtrlLink()
    val c1 = CtrlLink()
    val c2 = CtrlLink()

    val s01 = StageLink(c0.down, c1.up)
    val s12 = StageLink(c1.down, c2.up)

    val IN = Payload(UInt(16 bits))
    val OUT = Payload(UInt(16 bits))

    val up = slave Stream (IN)
    val down = master Stream (OUT)

    c0.up.driveFrom(up)((self, payload) => self(IN) := payload)
    c2.down.driveTo(down)((payload, self) => payload := self(OUT))

    val connectors = List(c0, c1, c2, s01, s12)
    afterElaboration(Builder(connectors))

    // The user just need to implement that callback to specify what should be expected
    // on the down stream in function of what is pushed
    def testIt(onPush: (Int, mutable.Queue[Int]) => Unit): Unit = simpleTest(clockDomain, up, down)(onPush)
  }

  test("ctrl") {
    SimConfig.compile(new CtrlPipeline {
      c1(OUT) := c1(IN)
    }).doSimUntilVoid { dut =>
      dut.testIt { (value, queue) =>
        queue += value
      }
    }
  }

  test("secondaryKeys") {
    SimConfig.compile(new CtrlPipeline {
      c0.up(OUT, "one") := c0.up(IN) + 1
      c0(OUT, "two") := c0(IN) + 2
      c1(OUT) := c1(IN) + c1(OUT, "one") + c1(OUT, "two")
    }).doSimUntilVoid { dut =>
      dut.testIt { (value, queue) =>
        queue += (value + (value + 1) + (value + 2)) & 0xFFFF
      }
    }
  }

  test("secondaryKeysList") {
    SimConfig.compile(new CtrlPipeline {
      c0.up(OUT, 1) := c0.up(IN) + 1
      c0(OUT, 2) := c0(IN) + 2
      c1(OUT) := c1(IN) + c1(1 to 2)(OUT).reduce(_ + _)
    }).doSimUntilVoid { dut =>
      dut.testIt { (value, queue) =>
        queue += (value + (value + 1) + (value + 2)) & 0xFFFF
      }
    }
  }


  // Remove C1 transactions which have the LSB set
  test("throw") {
    SimConfig.compile(new CtrlPipeline {
      val doThrow = c1(IN).lsb
      spinal.core.assert(c1.up.isCancel === doThrow)
      spinal.core.assert(!(c1.down.valid && doThrow))
      spinal.core.assert(!c1.down.isCancel)

      when(c1.up.isValid && doThrow) {
        spinal.core.assert(c1.up.isMoving)
        spinal.core.assert(!c1.up.isFiring)
        spinal.core.assert(!c1.down.isMoving)
        spinal.core.assert(!c1.down.isFiring)
      }
      when(!c1.up.isValid || doThrow){
        spinal.core.assert(!c1.down.isMoving)
        spinal.core.assert(!c1.down.isFiring)
      }

      when(doThrow){
        spinal.core.assert(!c0.down.isCancel)
        spinal.core.assert(c1.up.isCancel)
        spinal.core.assert(!c1.down.isCancel)
      }

      c1.throwWhen(doThrow)
      c1(OUT) := c1(IN)
    }).doSimUntilVoid { dut =>
      dut.testIt { (value, queue) =>
        if ((value & 1) == 0) {
          queue += value
        }
      }
    }
  }


  // Remove C1 transactions which have the LSB set
  test("throwC0") {
    SimConfig.compile(new CtrlPipeline {
      val doThrow = c0(IN).lsb

      when(doThrow) {
        spinal.core.assert(c0.up.isCancel)
        spinal.core.assert(!c0.down.isCancel)
      }

      c0.throwWhen(doThrow)
      c1(OUT) := c1(IN)
    }).doSimUntilVoid { dut =>
      dut.testIt { (value, queue) =>
        if ((value & 1) == 0) {
          queue += value
        }
      }
    }
  }

  // Duplicate the c1 transaction IN(1 downto 0) times
  test("duplicateWhen") {
    SimConfig.compile(new CtrlPipeline {
      val counter = Reg(UInt(2 bits)) init (0)
      val last = counter === c1(IN)(counter.bitsRange)
      c1.duplicateWhen(!last)
      when(c1.down.isFiring) {
        counter := counter + 1
        when(last) {
          counter := 0
        }
      }
      c1(OUT) := c1(IN)
    }).doSimUntilVoid { dut =>
      dut.testIt { (value, queue) =>
        for (i <- 0 to value & 3) {
          queue += value
        }
      }
    }
  }

  // Block c1 transaction 3 cycles
  test("halt") {
    SimConfig.compile(new CtrlPipeline {
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
      dut.testIt { (value, queue) =>
        queue += (value + counter + 3) & 0xFFFF
        counter += 4
      }
    }
  }

  // Accumulate the value of 4 transaction in C1 and use the sum as result
  test("terminateWhen") {
    SimConfig.compile(new CtrlPipeline {
      val accumulator = Reg(UInt(16 bits)) init (0)
      val counter = Reg(UInt(2 bits)) init(0)
      val last = counter.andR
      val adder = accumulator + c1(IN)
      c1.terminateWhen(!last)
      when(c1.up.isFiring) {
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

      dut.testIt { (value, queue) =>
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
    SimConfig.compile(new CtrlPipeline {
      val state = Reg(UInt(16 bits)) init(0)
      c0.up(OUT) := state

      def addBypassOn(ctrl : CtrlLink): Unit = {
        when(c2.down.valid){
          ctrl.bypass(OUT) := c2(IN)
        }
      }

      addBypassOn(c0)
      addBypassOn(c1)

      when(c2.down.isFiring){
        state := c2(IN)
      }
    }).doSimUntilVoid { dut =>
      var state = 0
      dut.testIt { (value, queue) =>
        queue += state
        state = value
      }
    }
  }

  test("bypassExplicit") {
    SimConfig.compile(new CtrlPipeline {
      val state = Reg(UInt(16 bits)) init (0)
      c0.up(OUT) := state

      def addBypassOn(ctrl: CtrlLink): Unit = {
        when(c2.down.valid) {
          ctrl.bypass(OUT) := c2(IN)
        }
      }
      c0.bypass(OUT) := c0.up(OUT)
      addBypassOn(c0)
      addBypassOn(c1)

      when(c2.down.isFiring) {
        state := c2(IN)
      }
    }).doSimUntilVoid { dut =>
      var state = 0
      dut.testIt { (value, queue) =>
        queue += state
        state = value
      }
    }
  }

  // 1 input stream is forked into to output stream
  test("fork") {
    SimConfig.compile(new Component {
      val n0, n1, n2, n3, n4, n5, n6 = Node()

      val s0 = StageLink(n0, n1)
      val f0 = ForkLink(n1, List(n2, n4))
      val s1 = StageLink(n2, n3)
      val c0 = CtrlLink(n4, n5)
      val s2 = StageLink(n5, n6)

      val IN = Payload(UInt(16 bits))

      val up = slave Stream (IN)
      val downs = Vec.fill(2)(master Stream (IN))

      n0.driveFrom(up)((self, payload) => self(IN) := payload)
      n3.driveTo(downs(0))((payload, self) => payload := self(IN) + 0x1234)
      c0.throwWhen(c0.up(IN)(0))
      n6.driveTo(downs(1))((payload, self) => payload := self(IN) + 0x4836)

      val connectors = List(f0, s0, s1, s2, c0)
      Builder(connectors)
    }).doSimUntilVoid{ dut =>
      val cd = dut.clockDomain
      cd.forkStimulus(10)
      val coverages = ArrayBuffer.fill(2)(0)

      val refQueues = List.fill(2)(mutable.Queue[Int]())
      StreamDriver(dut.up, cd) { p =>
        p.randomize()
        true
      }.setFactorPeriodically(500)
      StreamMonitor(dut.up, cd) { p =>
        val v = p.toInt
        refQueues(0) += (v + 0x1234) & 0xFFFF
        if((v & 1) == 0) refQueues(1) += (v + 0x4836) & 0xFFFF
      }

      for((down, i) <- dut.downs.zipWithIndex){
        StreamReadyRandomizer(down, cd).setFactorPeriodically(500)
        StreamMonitor(down, cd) { p =>
          val got = p.toInt
          assert(got == refQueues(i).dequeue())
          coverages(i) += 1
          if (coverages.forall(_ > 1000)) simSuccess()
        }
      }
    }
  }


  // 2 input stream are joined into 1 output stream which sum their values
  test("join") {
    SimConfig.compile(new Component {
      val n0, n1, n2, n3, n4, n5 = Node()

      val s0 = StageLink(n0, n1)
      val s1 = StageLink(n2, n3)
      val j0 = JoinLink(List(n1, n3), n4)
      val s2 = StageLink(n4, n5)

      val A, B = Payload(UInt(16 bits))
      val OUT = Payload(UInt(16 bits))

      val ups = Vec.fill(2)(slave Stream (UInt(16 bits)))
      val down = master Stream (OUT)

      n0.driveFrom(ups(0))((self, payload) => self(A) := payload)
      n2.driveFrom(ups(1))((self, payload) => self(B) := payload)

      n5.driveTo(down)((payload, self) => payload := self(A) + self(B))

      val connectors = List(j0, s0, s1, s2)
      Builder(connectors)
    }).doSimUntilVoid { dut =>
      val cd = dut.clockDomain
      cd.forkStimulus(10)
      var coverage = 0

      val refQueues = List.fill(2)(mutable.Queue[Int]())
      for(i <- 0 until 2) {
        val up = dut.ups(i)
        StreamDriver(up, cd) { p =>
          p.randomize()
          true
        }.setFactorPeriodically(500)
        StreamMonitor(up, cd) { p =>
          refQueues(i) += p.toInt
        }
      }

      StreamReadyRandomizer(dut.down, cd).setFactorPeriodically(500)
      StreamMonitor(dut.down, cd) { p =>
        val got = p.toInt
        assert(got == ((refQueues(0).dequeue() + refQueues(1).dequeue()) & 0xFFFF))
        coverage += 1
        if (coverage > 1000) simSuccess()
      }
    }
  }


  class S2mPipeline extends Component {
    val c0 = CtrlLink()
    val c1 = CtrlLink()
    val c2 = CtrlLink()
    val c3 = CtrlLink()

    val s01 = StageLink(c0.down, c1.up)
    val s12 = S2MLink(c1.down, c2.up)
    val s23 = StageLink(c2.down, c3.up)

    val IN = Payload(UInt(16 bits))
    val OUT = Payload(UInt(16 bits))

    val up = slave Stream (IN)
    val down = master Stream (OUT)

    c0.up.driveFrom(up)((self, payload) => self(IN) := payload)
    c3.down.driveTo(down)((payload, self) => payload := self(OUT))

    val connectors = List(c0, c1, c2, c3, s01, s12, s23)

    afterElaboration(Builder(connectors))

    // The user just need to implement that callback to specify what should be expected
    // on the down stream in function of what is pushed
    def testIt(onPush: (Int, mutable.Queue[Int]) => Unit): Unit = simpleTest(clockDomain, up, down)(onPush)
  }

  test("s2m"){
    SimConfig.compile(new S2mPipeline {
      c1(OUT) := c1(IN)
    }).doSimUntilVoid { dut =>
      dut.testIt { (value, queue) =>
        queue += value
      }
    }
  }

  test("s2mThrow") {
    SimConfig.compile(new S2mPipeline {
      val halt = in Bool()
      val doThrow = c2(IN).lsb && !halt
      c2.haltWhen(halt) //This is used to get better coverage on the throw logic
      c2.throwWhen(doThrow)

      when(doThrow){
        spinal.core.assert(c2.up.cancel)
      }
      c2(OUT) := c2(IN)
    }).doSimUntilVoid { dut =>
      dut.clockDomain.onSamplings(dut.halt.randomize())
      dut.testIt { (value, queue) =>
        if ((value & 1) == 0) {
          queue += value
        }
      }
    }
  }


  class UnmappedPipeline extends Component {
    val c0 = CtrlLink()
    val c1 = CtrlLink()
    val c2 = CtrlLink()

    val s01 = StageLink(c0.down, c1.up)
    val s12 = StageLink(c1.down, c2.up)

    val IN = Payload(UInt(16 bits))
    val OUT = Payload(UInt(16 bits))

    val connectors = List(c0, c1, c2, s01, s12)
    afterElaboration(Builder(connectors))
  }

  class FlowPipeline extends UnmappedPipeline {
    val up = slave Flow (IN)
    val down = master Flow (OUT)
    c0.up.driveFrom(up)((self, payload) => self(IN) := payload)
    c2.down.driveTo(down)((payload, self) => payload := self(OUT))
    def testIt(onPush: (Int, mutable.Queue[Int]) => Unit) = simpleTest(clockDomain, up, down)(onPush)
  }

  test("FlowPipeline") {
    SimConfig.compile(new FlowPipeline {
      c1(OUT) := c1(IN)
    }).doSimUntilVoid { dut =>
      dut.testIt { (value, queue) =>
        queue += value
      }
    }
  }

  class PayloadPipeline extends UnmappedPipeline {
    val up = in(IN)
    val down = out(OUT)
    c0.up(IN) := up
    down := c2.down(OUT)

    def testIt(onPush: (Int, mutable.Queue[Int]) => Unit) = simpleTest(clockDomain, up, down)(onPush)
  }

  test("PayloadPipeline") {
    SimConfig.compile(new PayloadPipeline {
      c1(OUT) := c1(IN)
      afterElaboration{
        c1.up(IN) init(0)
        c2.up(OUT) init(0)
      }
    }).doSimUntilVoid { dut =>
      val tb = dut.testIt { (value, queue) =>
        queue += value
      }
      tb.refQueue += 0
      tb.refQueue += 0
    }
  }


  class StreamToFlowPipeline extends UnmappedPipeline {
    val up = slave Stream (IN)
    val down = master Flow (OUT)
    c0.up.driveFrom(up)((self, payload) => self(IN) := payload)
    c2.down.driveTo(down)((payload, self) => payload := self(OUT))
    def testIt(onPush: (Int, mutable.Queue[Int]) => Unit): Unit = simpleTest(clockDomain, up, down)(onPush)
  }

  test("StreamToFlowPipeline") {
    SimConfig.compile(new StreamToFlowPipeline {
      c1(OUT) := c1(IN)
      val halt = in Bool()
      c1.haltWhen(halt)
    }).doSimUntilVoid { dut =>
      dut.clockDomain.onSamplings(dut.halt.randomize())
      dut.testIt { (value, queue) =>
        queue += value
      }
    }
  }


  class DataPipeline extends UnmappedPipeline {
    val up = in UInt (16 bits)
    val down = master Flow(UInt(16 bits))

    c0.up(IN) := up
    c0.up.valid := True

    c2.down.driveTo(down)((payload, self) => payload := self(OUT))

    def testIt(onPush: (Int, mutable.Queue[Int]) => Unit): Unit = simpleTest(clockDomain, up, down)(onPush)
  }

  test("DataPipeline") {
    SimConfig.compile(new DataPipeline {
      c1(OUT) := c1(IN)
    }).doSimUntilVoid { dut =>
      dut.testIt { (value, queue) =>
        queue += value
      }
    }
  }
}
