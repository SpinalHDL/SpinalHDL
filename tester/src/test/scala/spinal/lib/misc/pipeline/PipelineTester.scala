package spinal.lib.misc.pipeline

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.sim.{StreamDriver, StreamMonitor, StreamReadyRandomizer}
import spinal.tester.SpinalAnyFunSuite

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class PipelineTester extends SpinalAnyFunSuite{

  // The user just need to implement that callback to specify what should be expected
  // on the down stream in function of what is pushed on the up stream
  def simpleTest(cd : ClockDomain, up : Stream[UInt], down : Stream[UInt])(onPush: (Int, mutable.Queue[Int]) => Unit): Unit = {
    cd.forkStimulus(10)
    var coverage = 0

    val refQueue = mutable.Queue[Int]()
    StreamDriver(up, cd) { p =>
      p.randomize()
      true
    }.setFactorPeriodically(500)
    StreamMonitor(up, cd) { p =>
      onPush(p.toInt, refQueue)
    }

    StreamReadyRandomizer(down, cd).setFactorPeriodically(500)
    StreamMonitor(down, cd) { p =>
      val got = p.toInt
      assert(got == refQueue.dequeue())
      coverage += 1
      if (coverage == 1000) simSuccess()
    }
  }

  // Simple pipeline composed of 4 nodes connected by 3 register stages
  class NodePipeline extends Component {
    val n0, n1, n2, n3 = new Node()

    val s01 = StageConnector(n0, n1)
    val s12 = StageConnector(n1, n2)
    val s23 = StageConnector(n2, n3)

    val IN = Stageable(UInt(16 bits))
    val OUT = Stageable(UInt(16 bits))

    val up = slave Stream (IN)
    val down = master Stream (OUT)

    n0.driveFrom(up)((self, payload) => self(IN) := payload)
    n3.toStream(down)((payload, self) => payload := self(OUT))

    val connectors = List(s01, s12, s23)

    override def postInitCallback() = {
      Builder(connectors)
      super.postInitCallback()
    }


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
        queue += value + 4 + 45
      }
    }
  }


  // This a simple pipeline composed of
  // - 3 CtrlConnector connected by 2 register stages
  // - up / down streams to feed and read the pipeline
  class CtrlPipeline extends Component{
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
      c1.throwWhen(c1(IN).lsb)
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
      when(c1.down.isFireing) {
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
      dut.testIt { (value, queue) =>
        queue += state
        state = value
      }
    }
  }

  // 1 input stream is forked into to output stream
  test("fork") {
    SimConfig.compile(new Component {
      val n0, n1, n2, n3, n4, n5, n6 = new Node()

      val s0 = StageConnector(n0, n1)
      val f0 = new ForkConnector(n1, List(n2, n4))
      val s1 = StageConnector(n2, n3)
      val c0 = CtrlConnector(n4, n5)
      val s2 = StageConnector(n5, n6)

      val IN = Stageable(UInt(16 bits))

      val up = slave Stream (IN)
      val downs = Vec.fill(2)(master Stream (IN))

      n0.driveFrom(up)((self, payload) => self(IN) := payload)
      n3.toStream(downs(0))((payload, self) => payload := self(IN) + 0x1234)
      c0.throwWhen(c0.up(IN)(0))
      n6.toStream(downs(1))((payload, self) => payload := self(IN) + 0x4836)

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
      val n0, n1, n2, n3, n4, n5 = new Node()

      val s0 = StageConnector(n0, n1)
      val s1 = StageConnector(n2, n3)
      val j0 = new JoinConnector(List(n1, n3), n4)
      val s2 = StageConnector(n4, n5)

      val A, B = Stageable(UInt(16 bits))
      val OUT = Stageable(UInt(16 bits))

      val ups = Vec.fill(2)(slave Stream (UInt(16 bits)))
      val down = master Stream (OUT)

      n0.driveFrom(ups(0))((self, payload) => self(A) := payload)
      n2.driveFrom(ups(1))((self, payload) => self(B) := payload)

      n5.toStream(down)((payload, self) => payload := self(A) + self(B))

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
    val c0 = CtrlConnector()
    val c1 = CtrlConnector()
    val c2 = CtrlConnector()
    val c3 = CtrlConnector()

    val s01 = StageConnector(c0.down, c1.up)
    val s12 = S2mConnector(c1.down, c2.up)
    val s23 = StageConnector(c2.down, c3.up)

    val IN = Stageable(UInt(16 bits))
    val OUT = Stageable(UInt(16 bits))

    val up = slave Stream (IN)
    val down = master Stream (OUT)

    c0.up.driveFrom(up)((self, payload) => self(IN) := payload)
    c3.down.toStream(down)((payload, self) => payload := self(OUT))

    val connectors = List(c0, c1, c2, c3, s01, s12, s23)

    override def postInitCallback() = {
      Builder(connectors)
      super.postInitCallback()
    }

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
    SimConfig.withFstWave.compile(new S2mPipeline {
      val halt = in Bool()
      c2.haltWhen(halt) //This is used to get better coverage on the throw logic
      c2.throwWhen(c2(IN).lsb && !halt)
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
}
