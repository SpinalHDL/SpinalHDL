package spinal.lib.bus.regif

import spinal.core._
import spinal.core.fiber.Fiber
import spinal.core.sim._
import spinal.lib._
import spinal.lib.bus.localbus.{MemBus, MemBusConfig, MemBusDriver}
import spinal.lib.bus.misc.SizeMapping
import spinal.lib.bus.tilelink
import spinal.lib.bus.tilelink._
import spinal.lib.bus.tilelink.fabric.MasterBus
import spinal.lib.bus.tilelink.sim.{Checker, MasterAgent, TransactionD}
import spinal.lib.sim.{FlowMonitor, StreamDriver}
import spinal.tester.SpinalAnyFunSuite

import scala.collection.mutable
import scala.util.Random

class RegIfTilelinkDut(sourceWidth: Int = 2) extends Component {
  val io = new Bundle {
    val bus  = slave(tilelink.Bus(TilelinkBusInterface.getParameter(addressWidth = 12, dataWidth = 32, sourceWidth = sourceWidth)))
    val halt = in Bool()
  }

  val busif = TilelinkBusInterface(io.bus, SizeMapping(0, 4 KiB))
  busif.setReservedAddressErrorState(true)
  when (io.halt) {
    busif.readHalt()
  }

  // One RW register per source at 0x00 + 4 * source
  val perSource = (0 until (1 << sourceWidth)).map { i =>
    busif.newRegAt(i * 4, s"Source $i")(SymbolName(s"SRC$i")).field(Bits(32 bit), AccessType.RW, 0, "rw")(SymbolName(s"src$i"))
  }
  val wo = busif.newRegAt(0x40, "Write only")(SymbolName("WO")).field(Bits(32 bit), AccessType.WO, 0, "wo")(SymbolName("wo")).simPublic()
  val rw = busif.newRegAt(0x44, "Read write")(SymbolName("RW")).field(Bits(32 bit), AccessType.RW, 0, "rw")(SymbolName("rw")).simPublic()
}

class RegIfTilelinkFabricDut extends Component {
  // The master proposes bursts, the negotiation has to narrow them down to single beat accesses
  val m0 = new MasterBus(
    M2sParameters(
      addressWidth = 32,
      dataWidth = 32,
      masters = List(M2sAgent(
        name = null,
        mapping = List(M2sSource(
          id = SizeMapping(0, 4),
          emits = M2sTransfers(
            get = SizeRange(1, 64),
            putFull = SizeRange(1, 64),
            putPartial = SizeRange(1, 64)
          )
        ))
      ))
    )
  )

  val regs = new Area {
    val node = fabric.Node.slave()
    val logic = Fiber build new Area {
      node.m2s.supported.load(TilelinkBusInterface.getSupported(addressWidth = 12, dataWidth = 32, node.m2s.proposed))
      node.s2m.none()

      val busif = TilelinkBusInterface(node.bus, SizeMapping(0, 4 KiB))
      val value = busif.newReg("Value")(SymbolName("VALUE")).field(Bits(32 bit), AccessType.RW, 0, "value")(SymbolName("value"))
    }
  }
  regs.node at 0x1000 of m0.node
}

// TileLink version of RegIfTester (StrbTester.scala): every access type checked against the RegIfRef model,
// with random PutPartialData masks as write strobes
class RegIfTilelinkRefDut extends Component with RegIfRef {
  val bus = slave(tilelink.Bus(TilelinkBusInterface.getParameter(addressWidth = 12, dataWidth = 32)))
  val io = new Bundle {
    val o          = out(Vec(Bits(32 bit), ACCESS_LIST.size))
    val i          = in(Vec(Bits(32 bit), ACCESS_LIST.size))
    val t          = in(Bool())
    val hw_set     = in Bool()
    val hw_set_val = in Bits (32 bit)
  }

  val regIf = TilelinkBusInterface(bus, SizeMapping(0, NUM_REGISTERS * 4))

  val rand = new Random(0)

  ACCESS_LIST.zipWithIndex.foreach { case (acc, i) =>
    val REG = regIf.newRegAt(i * 4, s"Register_0x${(i * 4).hexString(16)}_${acc}")
    val reg = acc match {
      case AccessType.HSRW => REG.fieldHSRW(io.hw_set, io.hw_set_val, resetValue = BigInt.probablePrime(32, rand), s"${acc}")
      case AccessType.RWHS => REG.fieldRWHS(io.hw_set, io.hw_set_val, resetValue = BigInt.probablePrime(32, rand), s"${acc}")
      case _               => REG.field(Bits(32 bit), acc, resetValue = BigInt.probablePrime(32, rand), s"${acc}")
    }
    reg.setName(s"fd_0x${(i * 4).hexString(16)}_${acc}".toLowerCase())
    acc match {
      case AccessType.RO  => io.o(i).clearAll(); reg := "abcdedf".asHex
      case AccessType.ROV => io.o(i).clearAll()
      case _ => {
        when (io.t) {
          reg := io.i(i)
        }
        io.o(i) := reg
      }
    }
  }

  lazy val agent = {
    val agent = new MasterAgent(bus, clockDomain)(null)
    Checker(agent.monitor)
    agent
  }

  def failures = FailMsg.toList

  def dowrite(addr: Long): Unit = {
    val idx   = (addr / 4).toInt
    val acc   = maps(idx)
    val wdata = BigInt(32, rand)
    val wstrb = BigInt(4, rand)

    val oldhwdata = io.o(idx).toBigInt
    agent.putPartialData(0, addr, (0 until 4).map(i => (wdata >> (i * 8)).toByte), (0 until 4).map(wstrb.testBit))
    sleep(0)
    val newhwdata = io.o(idx).toBigInt

    val refdata = onWrite(acc, oldhwdata, wdata, wstrb, !writeHistory.contains(addr))
    writeHistory.add(addr)
    putMsg(compare(acc, addr, newhwdata, refdata, s", write '0x${wdata.hexString(32)}' on '0x${oldhwdata.hexString(32)}' strb '0x${wstrb.binString(4)}'"))
  }

  def doread(addr: Long): Unit = {
    val idx = (addr / 4).toInt
    val acc = maps(idx)

    val oldhwdata = io.o(idx).toBigInt
    agent.get(0, addr, 4)
    sleep(0)
    val newhwdata = io.o(idx).toBigInt

    putMsg(compare(acc, addr, newhwdata, onRead(acc, oldhwdata), s", read on '0x${oldhwdata.hexString(32)}'"))
  }

  def sim(): Unit = {
    clockDomain.forkStimulus(10)
    io.hw_set #= false
    io.i.foreach(_ #= BigInt.probablePrime(32, rand))
    io.t #= true
    agent
    clockDomain.waitSampling()
    io.t #= false
    clockDomain.waitSampling(10)

    (0 until NUM_REGISTERS).foreach(i => dowrite(i * 4))
    (0 until NUM_REGISTERS).foreach(i => doread(i * 4))
    (0 until 2000).foreach { _ =>
      val addr = rand.nextInt(NUM_REGISTERS) * 4
      if (rand.nextBoolean()) dowrite(addr) else doread(addr)
    }
  }
}

// TileLink version of RegifFifoMem (RegIfMemFifoTester.scala)
class RegIfTilelinkMemFifoDut extends Component {
  val io = new Bundle {
    val bus    = slave(tilelink.Bus(TilelinkBusInterface.getParameter(addressWidth = 12, dataWidth = 32)))
    val ram    = master(MemBus(MemBusConfig(aw = 9, dw = 32)))
    val fifowr = master(Flow(Bits(32 bit)))
    val fiford = slave(Stream(Bits(32 bit)))
  }

  val busif = TilelinkBusInterface(io.bus, SizeMapping(0, 4 KiB))
  busif.newReg("Version")(SymbolName("Version")).field(Bits(32 bit), AccessType.ROV, 0x12345678, "device version")(SymbolName("version"))

  val WRFIFO = busif.newWrFifoAt(0x0004, "write FIFO")(SymbolName("WRFIFO"))
  WRFIFO.field(32, doc = "data")("data")
  WRFIFO.bus >> io.fifowr

  val RDFIFO = busif.newRdFifoAt(0x0008, "read FIFO")(SymbolName("RDFIFO"))
  RDFIFO.field(32, doc = "data")("data")
  io.fiford >> RDFIFO.bus

  val RAM = busif.newRAMAt(0x0100, 0x0800, "RAM")(SymbolName("RAM"))
  RAM.field(32, doc = "data")("data")
  RAM.bus >> io.ram
}

class RegIfTilelinkTester extends SpinalAnyFunSuite {
  def toBytes(value: BigInt, count: Int = 4): Seq[Byte] = (0 until count).map(i => (value >> (i * 8)).toByte)
  def toValue(d: TransactionD): BigInt = BigInt(1, d.data.reverse)

  lazy val compiled = SimConfig.compile(new RegIfTilelinkDut())

  // Checks that the D payload doesn't change while it is stalled
  def forkDStableChecker(bus: tilelink.Bus, cd: ClockDomain) = fork {
    def snapshot() = (bus.d.opcode.toEnum, bus.d.source.toInt, bus.d.size.toInt, bus.d.denied.toBoolean, bus.d.data.toBigInt, bus.d.corrupt.toBoolean)
    var stalled: Option[Any] = None
    while (true) {
      cd.waitSampling()
      stalled.foreach { s =>
        assert(bus.d.valid.toBoolean, "D valid dropped while stalled")
        assert(snapshot() == s, "D payload changed while stalled")
      }
      stalled = if (bus.d.valid.toBoolean && !bus.d.ready.toBoolean) Some(snapshot()) else None
    }
  }

  def newAgent(bus: tilelink.Bus, cd: ClockDomain): MasterAgent = {
    val agent = new MasterAgent(bus, cd)(null)
    Checker(agent.monitor)
    forkDStableChecker(bus, cd)
    agent
  }

  def forkStallRandomizer(agent: MasterAgent) = periodically(500) {
    agent.driver.driver.randomizeStallRate()
  }

  def doSimWithAgent(name: String)(body: (RegIfTilelinkDut, MasterAgent) => Unit): Unit = compiled.doSim(name) { dut =>
    dut.clockDomain.forkStimulus(10)
    dut.io.halt #= false
    val agent = newAgent(dut.io.bus, dut.clockDomain)
    dut.clockDomain.waitSampling(10)
    body(dut, agent)
  }

  test("random_concurrent_sources") {
    doSimWithAgent("random_concurrent_sources") { (dut, agent) =>
      forkStallRandomizer(agent)
      val threads = for (source <- 0 until 4) yield fork {
        val rand = new Random(source)
        val address = source * 4
        var model = BigInt(0)
        for (_ <- 0 until 300) {
          rand.nextInt(3) match {
            case 0 => {
              val data = BigInt(32, rand)
              assert(!agent.putFullData(source, address, toBytes(data)).denied)
              model = data
            }
            case 1 => {
              val data = BigInt(32, rand)
              val mask = Seq.fill(4)(rand.nextBoolean())
              assert(!agent.putPartialData(source, address, toBytes(data), mask).denied)
              for (i <- 0 until 4 if mask(i)) {
                model = (model & ~(BigInt(0xFF) << (i * 8))) | (data & (BigInt(0xFF) << (i * 8)))
              }
            }
            case 2 => {
              val d = agent.get(source, address, 4)
              assert(!d.denied)
              assert(toValue(d) == model, f"source $source: read 0x${toValue(d)}%08x, expected 0x$model%08x")
            }
          }
        }
      }
      threads.foreach(_.join())
    }
  }

  test("pipelined_and_stalled") {
    compiled.doSim("pipelined_and_stalled") { dut =>
      dut.clockDomain.forkStimulus(10)
      dut.io.halt #= false
      val a = dut.io.bus.a
      val d = dut.io.bus.d
      a.valid #= false
      d.ready #= true
      forkDStableChecker(dut.io.bus, dut.clockDomain)
      dut.clockDomain.waitSampling(10)

      def setA(opcode: Opcode.A.E, source: Int, address: Int, data: BigInt = 0): Unit = {
        a.valid   #= true
        a.opcode  #= opcode
        a.param   #= 0
        a.source  #= source
        a.address #= address
        a.size    #= 2
        a.mask    #= 0xF
        a.data    #= data
        a.corrupt #= false
      }
      // Returns whether A fired, and the (source, data) of D if it fired
      def step(): (Boolean, Option[(Int, BigInt)]) = {
        dut.clockDomain.waitSampling()
        val aFire = a.valid.toBoolean && a.ready.toBoolean
        val dFire = if (d.valid.toBoolean && d.ready.toBoolean) Some(d.source.toInt -> d.data.toBigInt) else None
        (aFire, dFire)
      }

      // Back to back requests are accepted every cycle, and answered every cycle
      val requests = List(
        (Opcode.A.PUT_FULL_DATA, 0, 0x0, BigInt(0xA0)),
        (Opcode.A.PUT_FULL_DATA, 1, 0x4, BigInt(0xA1)),
        (Opcode.A.GET, 2, 0x0, BigInt(0)),
        (Opcode.A.GET, 3, 0x4, BigInt(0))
      )
      val responses = mutable.ArrayBuffer[(Int, (Int, BigInt))]()
      var cycle = 0
      for ((opcode, source, address, data) <- requests) {
        setA(opcode, source, address, data)
        val (aFire, dFire) = step()
        assert(aFire, s"request from source $source not accepted in a single cycle")
        dFire.foreach(responses += cycle -> _)
        cycle += 1
      }
      a.valid #= false
      for (_ <- 0 until 3) {
        step()._2.foreach(responses += cycle -> _)
        cycle += 1
      }
      assert(responses.map(_._1) == List(1, 2, 3, 4), s"responses not back to back: $responses")
      assert(responses.map(_._2._1) == List(0, 1, 2, 3))
      assert(responses(2)._2._2 == 0xA0 && responses(3)._2._2 == 0xA1)

      // While D is stalled, no new request is accepted and the response stays stable
      d.ready #= false
      setA(Opcode.A.GET, 0, 0x0)
      assert(step()._1)
      setA(Opcode.A.GET, 1, 0x4)
      for (_ <- 0 until 10) {
        val (aFire, dFire) = step()
        assert(!aFire, "request accepted while the response is stalled")
        assert(dFire.isEmpty)
        assert(d.valid.toBoolean && d.source.toInt == 0 && d.data.toBigInt == 0xA0)
      }
      d.ready #= true
      val (aFire, dFire) = step()
      assert(aFire, "request not accepted when the stalled response is consumed")
      assert(dFire.contains(0 -> BigInt(0xA0)))
      a.valid #= false
      assert(step()._2.contains(1 -> BigInt(0xA1)))
    }
  }

  test("sub_word") {
    doSimWithAgent("sub_word") { (dut, agent) =>
      agent.putFullData(0, 0x44, toBytes(0x11223344))
      agent.putFullData(0, 0x45, Seq(0xAB.toByte))
      assert(toValue(agent.get(0, 0x44, 4)) == 0x1122AB44)
      agent.putFullData(0, 0x46, toBytes(0xCDEF, 2))
      assert(toValue(agent.get(0, 0x44, 4)) == BigInt("CDEFAB44", 16))
      assert(dut.rw.toBigInt == BigInt("CDEFAB44", 16))
      assert(toValue(agent.get(0, 0x47, 1)) == 0xCD)
      assert(toValue(agent.get(0, 0x44, 2)) == 0xAB44)
    }
  }

  test("denied") {
    doSimWithAgent("denied") { (dut, agent) =>
      // The Checker also verifies that a denied AccessAckData is corrupt
      assert(!agent.putFullData(0, 0x40, toBytes(0x12345678)).denied, "write to a WO register denied")
      assert(dut.wo.toBigInt == 0x12345678)
      assert(agent.get(0, 0x40, 4).denied, "read of a WO register not denied")
      assert(agent.get(1, 0x100, 4).denied, "read of a reserved address not denied")
      assert(!agent.get(2, 0x00, 4).denied)
    }
  }

  test("halt") {
    doSimWithAgent("halt") { (dut, agent) =>
      agent.putFullData(0, 0x00, toBytes(0x5A5A5A5A))
      dut.io.halt #= true
      var done = false
      val thread = fork {
        assert(toValue(agent.get(0, 0x00, 4)) == 0x5A5A5A5A)
        done = true
      }
      dut.clockDomain.waitSampling(50)
      assert(!done, "request completed while halted")
      dut.io.halt #= false
      thread.join()
    }
  }

  test("fabric") {
    SimConfig.compile(new RegIfTilelinkFabricDut).doSim { dut =>
      val bus = dut.regs.node.bus
      assert(bus.p.sizeBytes == 4, s"negotiation didn't narrow the transfers down to single beat: ${bus.p.sizeBytes} bytes")

      dut.clockDomain.forkStimulus(10)
      val agent = newAgent(dut.m0.node.bus, dut.clockDomain)
      dut.clockDomain.waitSampling(10)

      agent.putFullData(0, 0x1000, toBytes(0xCAFEF00DL))
      assert(toValue(agent.get(1, 0x1000, 4)) == BigInt("CAFEF00D", 16))
      agent.putFullData(2, 0x1001, Seq(0x42.toByte))
      assert(toValue(agent.get(3, 0x1000, 4)) == BigInt("CAFE420D", 16))
    }
  }

  test("access_types_reference_model") {
    SimConfig.compile(new RegIfTilelinkRefDut).doSim { dut =>
      dut.sim()
      assert(!dut.isFail, "\n" + dut.failures.mkString("\n"))
    }
  }

  test("ram_and_fifos") {
    SimConfig.compile(new RegIfTilelinkMemFifoDut).doSim { dut =>
      val cd = dut.clockDomain
      cd.forkStimulus(10)
      dut.io.fiford.valid #= false
      MemBusDriver(dut.io.ram, cd).hangMem()
      val agent = newAgent(dut.io.bus, cd)
      forkStallRandomizer(agent)
      cd.waitSampling(10)

      def write(address: Long, data: BigInt) = assert(!agent.putFullData(0, address, toBytes(data)).denied)
      def read(address: Long) = {
        val d = agent.get(0, address, 4)
        assert(!d.denied, f"read of 0x$address%x denied")
        toValue(d)
      }
      val rand = new Random(0)

      assert(read(0x0000) == 0x12345678)

      for (i <- 0 until 0x800 / 4) {
        val data = BigInt(32, rand)
        write(0x0100 + i * 4, data)
        assert(read(0x0100 + i * 4) == data, s"RAM word $i")
      }

      val wrExpected = mutable.Queue[BigInt]()
      FlowMonitor(dut.io.fifowr, cd) { p =>
        assert(wrExpected.nonEmpty, "unexpected write FIFO push")
        assert(p.toBigInt == wrExpected.dequeue(), "write FIFO data mismatch")
      }
      for (_ <- 0 until 100) {
        val data = BigInt(32, rand)
        wrExpected.enqueue(data)
        write(0x0004, data)
      }
      cd.waitSampling(5)
      assert(wrExpected.isEmpty)

      // The read FIFO is kept valid until it runs out of data
      val rdData = Seq.fill(100)(BigInt(32, rand))
      val rdQueue = mutable.Queue(rdData: _*)
      StreamDriver(dut.io.fiford, cd) { p =>
        if (rdQueue.isEmpty) false else {
          p #= rdQueue.dequeue()
          true
        }
      }.transactionDelay = () => 0
      cd.waitSamplingWhere(dut.io.fiford.valid.toBoolean)
      for (data <- rdData) {
        assert(read(0x0008) == data, "read FIFO data mismatch")
      }
      cd.waitSamplingWhere(!dut.io.fiford.valid.toBoolean)
      assert(agent.get(0, 0x0008, 4).denied, "read of an empty read FIFO not denied")
    }
  }
}
