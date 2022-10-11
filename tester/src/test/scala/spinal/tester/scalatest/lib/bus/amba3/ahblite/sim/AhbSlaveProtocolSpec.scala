package spinal.lib.bus.amba3.ahblite.sim

import org.scalatest.funsuite.AnyFunSuite

import spinal.core._
import spinal.core.sim._
import spinal.lib._

import hardware.Identity
import software.IdentityDut

class AhbSlaveProtocolSpec extends AnyFunSuite {
  val config = SpinalConfig(
    defaultConfigForClockDomains = ClockDomainConfig(
      resetActiveLevel = LOW
    )
  )
  val bench = SimConfig
    .withConfig(config)
    .withWave
    .compile(new Identity)
  def simulateTest(name: String)(body: IdentityDut => Unit) = test(name) {
    bench.doSim(name) { bareDut =>
      val dut = IdentityDut(bareDut)
      SimTimeout(1000)
      dut.init()
      dut.cd.waitSampling()
      body(dut)
    }
  }

  val A = 0xaaaaaaaaL
  val B = 0xbbbbbbbbL
  val C = 0xccccccccL
  val Y = 0x11111111L
  val Z = 0x22222222L
  def Data(addr: BigInt) = 0xdddd0000L | (0xffff & addr)

  class WriteChecker {
    var count = 0
    def apply(expected: BigInt): (BigInt, BigInt) => Unit = { (addr, data) =>
      count += 1
      assert(addr == expected)
      assert(data == Data(expected))
    }
    val justMatch: (BigInt, BigInt) => Unit = { (addr, data) =>
      count += 1
      assert(data == Data(addr))
    }
  }

  // All tests have the same structure:
  // * Define master behavior
  // * Define slave behavior
  // * Import signals to assert (slave-read (master-driven) then master-read (slave-driven))
  // * Boot master sequence
  // * Do assertions on signals
  // * Do assertions on transaction results

  simulateTest("Figure 3-1 Read transfer") { dut =>
    val read = dut.master.Read(A)

    dut.slave.setOnReads(Data)

    import dut.slave.{HADDR, HWRITE}
    import dut.master.{HRDATA, HREADYOUT}

    read.boot()

    dut.cd.waitSampling()
    assert(HADDR == A)
    assert(HWRITE == false)
    assert(HREADYOUT == true)

    dut.cd.waitSampling()
    assert(HRDATA == Data(A))
    assert(HREADYOUT == true)

    assert(read.result == Data(A))
    assert(read.duration == 2)
    assert(read.waitStates == 0)
  }

  simulateTest("Figure 3-2 Write transfer") { dut =>
    val write = dut.master.Write(A, Data(A))

    val checker = new WriteChecker
    dut.slave.setOnWrites(checker(A))

    import dut.slave.{HADDR, HWRITE, HWDATA}
    import dut.master.HREADYOUT

    write.boot()

    dut.cd.waitSampling()
    assert(HADDR == A)
    assert(HWRITE == true)
    assert(HREADYOUT == true)

    dut.cd.waitSampling()
    assert(HWDATA == Data(A))
    assert(HREADYOUT == true)

    assert(write.isDone)
    assert(write.duration == 2)
    assert(write.waitStates == 0)
    assert(checker.count == 1)
  }

  simulateTest("Figure 3-3 Read transfer with wait states") { dut =>
    val read = dut.master.Read(A)

    dut.slave.setOnReadsAfter(2)(Data)

    import dut.slave.{HADDR, HWRITE}
    import dut.master.{HREADYOUT, HRDATA}

    read.boot()

    dut.cd.waitSampling()
    assert(HADDR == A)
    assert(HWRITE == false)
    assert(HREADYOUT == true)

    dut.cd.waitSampling()
    assert(HREADYOUT == false)

    dut.cd.waitSampling()
    assert(HREADYOUT == false)

    dut.cd.waitSampling()
    assert(HRDATA == Data(A))
    assert(HREADYOUT == true)

    assert(read.result == Data(A))
    assert(read.duration == 4)
    assert(read.waitStates == 2)
  }

  simulateTest("Figure 3-4 Write transfer with wait state") { dut =>
    val write = dut.master.Write(A, Data(A))

    val checker = new WriteChecker
    dut.slave.setOnWritesAfter(1)(checker(A))

    import dut.slave.{HADDR, HWRITE, HWDATA}
    import dut.master.HREADYOUT

    write.boot()

    dut.cd.waitSampling()
    assert(HADDR == A)
    assert(HWRITE == true)
    assert(HREADYOUT == true)

    dut.cd.waitSampling()
    assert(HWDATA == Data(A))
    assert(HREADYOUT == false)

    dut.cd.waitSampling()
    assert(HWDATA == Data(A))
    assert(HREADYOUT == true)

    dut.cd.waitSampling()
    assert(write.isDone)
    assert(write.duration == 3)
    assert(write.waitStates == 1)
    assert(checker.count == 1)
  }

  simulateTest("Figure 3-5 Multiple transfers") { dut =>
    val writeA = dut.master.Write(A, Data(A))
    val readB = dut.master.Read(B)
    val writeC = dut.master.Write(C, Data(C))
    val sequence = writeA >> readB >> writeC

    val checker = new WriteChecker
    dut.slave.setOnWrites(checker(A))
    dut.slave.setOnReadsAfter(1)(Data)

    import dut.slave.{HADDR, HWRITE, HWDATA}
    import dut.master.{HREADYOUT, HRDATA}

    sequence.boot()

    dut.cd.waitSampling()
    assert(HADDR == A)
    assert(HWRITE)
    assert(HREADYOUT)

    dut.cd.waitSampling()
    assert(HADDR == B)
    assert(!HWRITE)
    assert(HREADYOUT)
    assert(HWDATA == Data(A))

    dut.slave.setOnWrites(checker(C))

    dut.cd.waitSampling()
    assert(HADDR == C)
    assert(HWRITE)
    assert(!HREADYOUT)

    dut.cd.waitSampling()
    assert(HADDR == C)
    assert(HWRITE)
    assert(HRDATA == Data(B))
    assert(HREADYOUT)

    dut.cd.waitSampling()
    assert(HREADYOUT)
    assert(HWDATA == Data(C))

    assert(sequence.allDone)
    assert(writeA.duration == 2)
    assert(writeA.waitStates == 0)
    assert(readB.result == Data(B))
    assert(readB.duration == 3)
    assert(readB.waitStates == 1)
    assert(writeC.duration == 3)
    assert(writeC.waitStates == 0)
    assert(checker.count == 2)
  }

  simulateTest("Figure 3-6 Transfer type examples") { dut =>
    var burst = dut.master.burstReadSeq(0x20, Hburst.incr(4))
    burst(1).busy = 1

    dut.slave._setOnReads { req =>
      import dut.slave._
      import spinal.lib.bus.amba3.ahblite.sim.slave.AhbResp
      delay(
        if (req.addr == 0x28) 1 else 0,
        resp = AhbResp(HREADY = false, HRDATA = Data(req.addr))
      )(
        fromReader(Data)
      )(req)
    }

    import dut.master.{HREADYOUT, HRDATA}
    import dut.slave.{HTRANS, HADDR, HWRITE, HBURST}

    burst.boot()

    // T1
    dut.cd.waitSampling()
    assert(HTRANS == Htrans.NONSEQ)
    assert(HADDR == 0x20)
    assert(!HWRITE)
    assert(HBURST == Hburst.INCR)
    assert(HREADYOUT)

    // T2
    dut.cd.waitSampling()
    assert(HTRANS == Htrans.BUSY)
    assert(HADDR == 0x24)
    assert(!HWRITE)
    assert(HBURST == Hburst.INCR)
    assert(HREADYOUT)
    assert(HRDATA == Data(0x20))

    // T3
    dut.cd.waitSampling()
    assert(HTRANS == Htrans.SEQ)
    assert(HADDR == 0x24)
    assert(!HWRITE)
    assert(HBURST == Hburst.INCR)
    assert(HREADYOUT)

    // T4
    dut.cd.waitSampling()
    assert(HTRANS == Htrans.SEQ)
    assert(HADDR == 0x28)
    assert(!HWRITE)
    assert(HBURST == Hburst.INCR)
    assert(HREADYOUT)
    assert(HRDATA == Data(0x24))

    // T5
    dut.cd.waitSampling()
    assert(HTRANS == Htrans.SEQ)
    assert(HADDR == 0x2c)
    assert(!HWRITE)
    assert(HBURST == Hburst.INCR)
    assert(!HREADYOUT)
    assert(HRDATA == Data(0x28))

    // T6
    dut.cd.waitSampling()
    assert(HTRANS == Htrans.SEQ)
    assert(HADDR == 0x2c)
    assert(!HWRITE)
    assert(HBURST == Hburst.INCR)
    assert(HREADYOUT)
    assert(HRDATA == Data(0x28))

    // T7
    dut.cd.waitSampling()
    assert(HREADYOUT)
    assert(HRDATA == Data(0x2c))

    burst.foreach { r => assert(r.result == Data(r.addr)) }
    assert(burst(0).duration == 2)
    assert(burst(0).waitStates == 0)
    assert(burst(1).duration == 3)
    assert(burst(1).waitStates == 0)
    assert(burst(2).duration == 3)
    assert(burst(2).waitStates == 1)
    assert(burst(3).duration == 3)
    assert(burst(3).waitStates == 0)
  }

  simulateTest("Figure 3-7 Locked transfer") { dut =>
    val readA = dut.master.Read(A, hmastlock = true)
    val writeA = dut.master.Write(A, Data(A), hmastlock = true)
    val idle = dut.master.Idle()

    val checker = new WriteChecker()
    dut.slave.setOnReads(Data)
    dut.slave.setOnWrites(checker(A))

    import dut.slave.{HSEL, HTRANS, HADDR, HWRITE, HMASTLOCK, HWDATA}
    import dut.master.HRDATA

    (readA >> writeA >> idle).boot()

    dut.cd.waitSampling()
    assert(HSEL)
    assert(HTRANS == Htrans.NONSEQ)
    assert(HADDR == A)
    assert(HWRITE == Hwrite.READ)
    assert(HMASTLOCK)

    dut.cd.waitSampling()
    assert(HSEL)
    assert(HTRANS == Htrans.NONSEQ)
    assert(HADDR == A)
    assert(HWRITE == Hwrite.WRITE)
    assert(HMASTLOCK)
    assert(HRDATA == Data(A))

    dut.cd.waitSampling()
    assert(HSEL)
    assert(HTRANS == Htrans.IDLE)
    assert(!HMASTLOCK)
    assert(HWDATA == Data(A))

    idle.await()

    assert(readA.result == Data(A))
    assert(readA.duration == 2)
    assert(readA.waitStates == 0)
    assert(writeA.duration == 2)
    assert(writeA.waitStates == 0)
    assert(idle.duration == 2)
    assert(idle.waitStates == 0)
  }

  simulateTest("Section 3.5 four-beat example") { dut =>
    val burst =
      dut.master.burstReadSeq(0x34, Hburst.wrap4(), hsize = Hsize.Word)
    assert(burst.map(_.addr) == Seq(0x34, 0x38, 0x3c, 0x30))
  }

  simulateTest("Figure 3-8 Four-beat wrapping burst") { dut =>
    val burst = dut.master.burstWriteSeq(
      0x38,
      Seq(Data(0x38), Data(0x3c), Data(0x30), Data(0x34)),
      Hburst.wrap4()
    )

    val checker = new WriteChecker()
    dut.slave._setOnWrites { req =>
      import dut.slave._
      delay(if (req.HTRANS == Htrans.NONSEQ) 1 else 0)(
        fromWriter(checker.justMatch)
      )(req)
    }

    import dut.slave.{HTRANS, HADDR, HWRITE, HBURST, HSIZE, HWDATA}
    import dut.master.HREADYOUT

    burst.boot()

    // T1
    dut.cd.waitSampling()
    assert(HTRANS == Htrans.NONSEQ)
    assert(HADDR == 0x38)
    assert(HWRITE)
    assert(HBURST == Hburst.WRAP4)
    assert(HSIZE == Hsize.Word)
    assert(HREADYOUT)

    // T2
    dut.cd.waitSampling()
    assert(HTRANS == Htrans.SEQ)
    assert(HADDR == 0x3c)
    assert(HWRITE)
    assert(HBURST == Hburst.WRAP4)
    assert(HSIZE == Hsize.Word)
    assert(!HREADYOUT)
    assert(HWDATA == Data(0x38))

    // T3
    dut.cd.waitSampling()
    assert(HTRANS == Htrans.SEQ)
    assert(HADDR == 0x3c)
    assert(HWRITE)
    assert(HBURST == Hburst.WRAP4)
    assert(HSIZE == Hsize.Word)
    assert(HREADYOUT)
    assert(HWDATA == Data(0x38))

    // T4
    dut.cd.waitSampling()
    assert(HTRANS == Htrans.SEQ)
    assert(HADDR == 0x30)
    assert(HWRITE)
    assert(HBURST == Hburst.WRAP4)
    assert(HSIZE == Hsize.Word)
    assert(HREADYOUT)
    assert(HWDATA == Data(0x3c))

    // T5
    dut.cd.waitSampling()
    assert(HTRANS == Htrans.SEQ)
    assert(HADDR == 0x34)
    assert(HWRITE)
    assert(HBURST == Hburst.WRAP4)
    assert(HSIZE == Hsize.Word)
    assert(HREADYOUT)
    assert(HWDATA == Data(0x30))

    // T6
    dut.cd.waitSampling()
    assert(HREADYOUT)
    assert(HWDATA == Data(0x34))

    assert(checker.count == 4)
    assert(burst(0).duration == 3)
    assert(burst(0).waitStates == 1)
    assert(burst(1).duration == 3)
    assert(burst(1).waitStates == 0)
    assert(burst(2).duration == 2)
    assert(burst(2).waitStates == 0)
    assert(burst(3).duration == 2)
    assert(burst(3).waitStates == 0)
  }

  simulateTest("Figure 3-9 Four-beat incrementing burst") { dut =>
    val burst = dut.master.burstReadSeq(0x38, Hburst.incr4())

    dut.slave._setOnReads { req =>
      import dut.slave._
      delay(if (req.HTRANS == Htrans.NONSEQ) 1 else 0)(fromReader(Data))(req)
    }

    import dut.slave.{HTRANS, HADDR, HWRITE, HBURST, HSIZE}
    import dut.master.{HREADYOUT, HRDATA}

    burst.boot()

    // T1
    dut.cd.waitSampling()
    assert(HTRANS == Htrans.NONSEQ)
    assert(HADDR == 0x38)
    assert(!HWRITE)
    assert(HBURST == Hburst.INCR4)
    assert(HSIZE == Hsize.Word)
    assert(HREADYOUT)

    // T2
    dut.cd.waitSampling()
    assert(HTRANS == Htrans.SEQ)
    assert(HADDR == 0x3c)
    assert(!HWRITE)
    assert(HBURST == Hburst.INCR4)
    assert(HSIZE == Hsize.Word)
    assert(!HREADYOUT)

    // T3
    dut.cd.waitSampling()
    assert(HTRANS == Htrans.SEQ)
    assert(HADDR == 0x3c)
    assert(!HWRITE)
    assert(HBURST == Hburst.INCR4)
    assert(HSIZE == Hsize.Word)
    assert(HREADYOUT)
    assert(HRDATA == Data(0x38))

    // T4
    dut.cd.waitSampling()
    assert(HTRANS == Htrans.SEQ)
    assert(HADDR == 0x40)
    assert(!HWRITE)
    assert(HBURST == Hburst.INCR4)
    assert(HSIZE == Hsize.Word)
    assert(HREADYOUT)
    assert(HRDATA == Data(0x3c))

    // T5
    dut.cd.waitSampling()
    assert(HTRANS == Htrans.SEQ)
    assert(HADDR == 0x44)
    assert(!HWRITE)
    assert(HBURST == Hburst.INCR4)
    assert(HSIZE == Hsize.Word)
    assert(HREADYOUT)
    assert(HRDATA == Data(0x40))

    // T6
    dut.cd.waitSampling()
    assert(HREADYOUT)
    assert(HRDATA == Data(0x44))

    burst.foreach(r => assert(r.result == Data(r.addr)))
    assert(burst(0).duration == 3)
    assert(burst(0).waitStates == 1)
    assert(burst(1).duration == 3)
    assert(burst(1).waitStates == 0)
    assert(burst(2).duration == 2)
    assert(burst(2).waitStates == 0)
    assert(burst(3).duration == 2)
    assert(burst(3).waitStates == 0)
  }

  simulateTest("Figure 3-10 Eight-beat wrapping burst") { dut =>
    val burst = dut.master.burstReadSeq(0x34, Hburst.wrap8())

    dut.slave.setOnReads(Data)

    import dut.slave.{HTRANS, HADDR, HWRITE, HBURST, HSIZE}
    import dut.master.{HREADYOUT, HRDATA}

    burst.boot()

    // T1
    dut.cd.waitSampling()
    assert(HTRANS == Htrans.NONSEQ)
    assert(HADDR == 0x34)
    assert(!HWRITE)
    assert(HBURST == Hburst.WRAP8)
    assert(HSIZE == Hsize.Word)
    assert(HREADYOUT)

    // T2
    dut.cd.waitSampling()
    assert(HTRANS == Htrans.SEQ)
    assert(HADDR == 0x38)
    assert(!HWRITE)
    assert(HBURST == Hburst.WRAP8)
    assert(HSIZE == Hsize.Word)
    assert(HREADYOUT)
    assert(HRDATA == Data(0x34))

    // T3
    dut.cd.waitSampling()
    assert(HTRANS == Htrans.SEQ)
    assert(HADDR == 0x3c)
    assert(!HWRITE)
    assert(HBURST == Hburst.WRAP8)
    assert(HSIZE == Hsize.Word)
    assert(HREADYOUT)
    assert(HRDATA == Data(0x38))

    // T4
    dut.cd.waitSampling()
    assert(HTRANS == Htrans.SEQ)
    assert(HADDR == 0x20)
    assert(!HWRITE)
    assert(HBURST == Hburst.WRAP8)
    assert(HSIZE == Hsize.Word)
    assert(HREADYOUT)
    assert(HRDATA == Data(0x3c))

    // T5
    dut.cd.waitSampling()
    assert(HTRANS == Htrans.SEQ)
    assert(HADDR == 0x24)
    assert(!HWRITE)
    assert(HBURST == Hburst.WRAP8)
    assert(HSIZE == Hsize.Word)
    assert(HREADYOUT)
    assert(HRDATA == Data(0x20))

    // T6
    dut.cd.waitSampling()
    assert(HTRANS == Htrans.SEQ)
    assert(HADDR == 0x28)
    assert(!HWRITE)
    assert(HBURST == Hburst.WRAP8)
    assert(HSIZE == Hsize.Word)
    assert(HREADYOUT)
    assert(HRDATA == Data(0x24))

    // T7
    dut.cd.waitSampling()
    assert(HTRANS == Htrans.SEQ)
    assert(HADDR == 0x2c)
    assert(!HWRITE)
    assert(HBURST == Hburst.WRAP8)
    assert(HSIZE == Hsize.Word)
    assert(HREADYOUT)
    assert(HRDATA == Data(0x28))

    // T8
    dut.cd.waitSampling()
    assert(HTRANS == Htrans.SEQ)
    assert(HADDR == 0x30)
    assert(!HWRITE)
    assert(HBURST == Hburst.WRAP8)
    assert(HSIZE == Hsize.Word)
    assert(HREADYOUT)
    assert(HRDATA == Data(0x2c))

    // T9
    dut.cd.waitSampling()
    assert(HREADYOUT)
    assert(HRDATA == Data(0x30))

    burst.foreach { r =>
      assert(r.result == Data(r.addr))
      assert(r.duration == 2)
      assert(r.waitStates == 0)
    }
  }

  simulateTest("Figure 3-11 Eight-beat incrementing burst") { dut =>
    val burst = dut.master.burstWriteSeq(
      0x34,
      Seq(
        Data(0x34),
        Data(0x36),
        Data(0x38),
        Data(0x3a),
        Data(0x3c),
        Data(0x3e),
        Data(0x40),
        Data(0x42)
      ),
      Hburst.incr8(),
      hsize = Hsize.Halfword
    )

    val checker = new WriteChecker()
    dut.slave.setOnWrites(checker.justMatch)

    import dut.slave.{HTRANS, HADDR, HWRITE, HBURST, HSIZE, HWDATA}
    import dut.master.HREADYOUT

    burst.boot()

    // T1
    dut.cd.waitSampling()
    assert(HTRANS == Htrans.NONSEQ)
    assert(HADDR == 0x34)
    assert(HWRITE)
    assert(HBURST == Hburst.INCR8)
    assert(HSIZE == Hsize.Halfword)
    assert(HREADYOUT)

    // T2
    dut.cd.waitSampling()
    assert(HTRANS == Htrans.SEQ)
    assert(HADDR == 0x36)
    assert(HWRITE)
    assert(HBURST == Hburst.INCR8)
    assert(HSIZE == Hsize.Halfword)
    assert(HREADYOUT)
    assert(HWDATA == Data(0x34))

    // T3
    dut.cd.waitSampling()
    assert(HTRANS == Htrans.SEQ)
    assert(HADDR == 0x38)
    assert(HWRITE)
    assert(HBURST == Hburst.INCR8)
    assert(HSIZE == Hsize.Halfword)
    assert(HREADYOUT)
    assert(HWDATA == Data(0x36))

    // T4
    dut.cd.waitSampling()
    assert(HTRANS == Htrans.SEQ)
    assert(HADDR == 0x3a)
    assert(HWRITE)
    assert(HBURST == Hburst.INCR8)
    assert(HSIZE == Hsize.Halfword)
    assert(HREADYOUT)
    assert(HWDATA == Data(0x38))

    // T5
    dut.cd.waitSampling()
    assert(HTRANS == Htrans.SEQ)
    assert(HADDR == 0x3c)
    assert(HWRITE)
    assert(HBURST == Hburst.INCR8)
    assert(HSIZE == Hsize.Halfword)
    assert(HREADYOUT)
    assert(HWDATA == Data(0x3a))

    // T6
    dut.cd.waitSampling()
    assert(HTRANS == Htrans.SEQ)
    assert(HADDR == 0x3e)
    assert(HWRITE)
    assert(HBURST == Hburst.INCR8)
    assert(HSIZE == Hsize.Halfword)
    assert(HREADYOUT)
    assert(HWDATA == Data(0x3c))

    // T7
    dut.cd.waitSampling()
    assert(HTRANS == Htrans.SEQ)
    assert(HADDR == 0x40)
    assert(HWRITE)
    assert(HBURST == Hburst.INCR8)
    assert(HSIZE == Hsize.Halfword)
    assert(HREADYOUT)
    assert(HWDATA == Data(0x3e))

    // T8
    dut.cd.waitSampling()
    assert(HTRANS == Htrans.SEQ)
    assert(HADDR == 0x42)
    assert(HWRITE)
    assert(HBURST == Hburst.INCR8)
    assert(HSIZE == Hsize.Halfword)
    assert(HREADYOUT)
    assert(HWDATA == Data(0x40))

    // T9
    dut.cd.waitSampling()
    assert(HREADYOUT)
    assert(HWDATA == Data(0x42))

    assert(checker.count == 8)
    burst.foreach { w =>
      assert(w.duration == 2)
      assert(w.waitStates == 0)
    }
  }

  simulateTest("Figure 3-12 Undefined length bursts") { dut =>
    val writeBurst = dut.master.burstWriteSeq(
      0x20,
      Seq(Data(0x20), Data(0x22)),
      Hburst.incr(2),
      hsize = Hsize.Halfword
    )
    val readBurst = dut.master.burstReadSeq(0x5c, Hburst.incr(3))

    val checker = new WriteChecker()
    dut.slave.setOnWrites(checker.justMatch)
    dut.slave._setOnReads { req =>
      import dut.slave._
      delay(if (HTRANS == Htrans.NONSEQ) 1 else 0)(fromReader(Data))(req)
    }

    import dut.slave.{HTRANS, HADDR, HWRITE, HBURST, HSIZE, HWDATA}
    import dut.master.{HREADYOUT, HRDATA}

    (writeBurst >> readBurst).boot()

    // T1
    dut.cd.waitSampling()
    assert(HTRANS == Htrans.NONSEQ)
    assert(HADDR == 0x20)
    assert(HWRITE)
    assert(HBURST == Hburst.INCR)
    assert(HSIZE == Hsize.Halfword)
    assert(HREADYOUT)

    // T2
    dut.cd.waitSampling()
    assert(HTRANS == Htrans.SEQ)
    assert(HADDR == 0x22)
    assert(HWRITE)
    assert(HBURST == Hburst.INCR)
    assert(HSIZE == Hsize.Halfword)
    assert(HREADYOUT)
    assert(HWDATA == Data(0x20))

    // T3
    dut.cd.waitSampling()
    assert(HTRANS == Htrans.NONSEQ)
    assert(HADDR == 0x5c)
    assert(!HWRITE)
    assert(HBURST == Hburst.INCR)
    assert(HSIZE == Hsize.Word)
    assert(HREADYOUT)
    assert(HWDATA == Data(0x22))

    // T4
    dut.cd.waitSampling()
    assert(HTRANS == Htrans.SEQ)
    assert(HADDR == 0x60)
    assert(!HWRITE)
    assert(HBURST == Hburst.INCR)
    assert(HSIZE == Hsize.Word)
    assert(!HREADYOUT)

    // T5
    dut.cd.waitSampling()
    assert(HTRANS == Htrans.SEQ)
    assert(HADDR == 0x60)
    assert(!HWRITE)
    assert(HBURST == Hburst.INCR)
    assert(HSIZE == Hsize.Word)
    assert(HREADYOUT)
    assert(HRDATA == Data(0x5c))

    // T6
    dut.cd.waitSampling()
    assert(HTRANS == Htrans.SEQ)
    assert(HADDR == 0x64)
    assert(!HWRITE)
    assert(HBURST == Hburst.INCR)
    assert(HSIZE == Hsize.Word)
    assert(HREADYOUT)
    assert(HRDATA == Data(0x60))

    // T7
    dut.cd.waitSampling()
    assert(HREADYOUT)
    assert(HRDATA == Data(0x64))

    assert(checker.count == 2)
    writeBurst.foreach { w =>
      assert(w.duration == 2)
      assert(w.waitStates == 0)
    }
    readBurst.foreach(r => assert(r.result == Data(r.addr)))
    assert(readBurst(0).duration == 3)
    assert(readBurst(0).waitStates == 1)
    assert(readBurst(1).duration == 3)
    assert(readBurst(1).waitStates == 0)
    assert(readBurst(2).duration == 2)
  }

  simulateTest("Figure 3-13 Waited transfer, IDLE to NONSEQ") { dut =>
    val skip = () => true
    val readA = dut.master.Read(A)
    val idleY = dut.master.Idle(Y, skip)
    val idleZ = dut.master.Idle(Z, skip)
    val burst = dut.master.burstReadSeq(B, Hburst.incr4())

    dut.slave._setOnReads { req =>
      import dut.slave._
      delay(if (req.addr == A) 4 else 0)(fromReader(Data))(req)
    }

    import dut.slave.{HTRANS, HADDR, HBURST}
    import dut.master.{HREADYOUT, HRDATA}

    (readA >> idleY >> idleZ >> burst).boot()

    // T1
    dut.cd.waitSampling()
    assert(HTRANS == Htrans.NONSEQ)
    assert(HADDR == A)
    assert(HBURST == Hburst.SINGLE)
    assert(HREADYOUT)

    // T2
    dut.cd.waitSampling()
    assert(HTRANS == Htrans.IDLE)
    assert(HADDR == Y)
    assert(!HREADYOUT)

    // T3
    dut.cd.waitSampling()
    assert(HTRANS == Htrans.IDLE)
    assert(HADDR == Z)
    assert(!HREADYOUT)

    // T4
    dut.cd.waitSampling()
    assert(HTRANS == Htrans.NONSEQ)
    assert(HADDR == B)
    assert(HBURST == Hburst.INCR4)
    assert(!HREADYOUT)

    // T5
    dut.cd.waitSampling()
    assert(HTRANS == Htrans.NONSEQ)
    assert(HADDR == B)
    assert(HBURST == Hburst.INCR4)
    assert(!HREADYOUT)

    // T6
    dut.cd.waitSampling()
    assert(HTRANS == Htrans.NONSEQ)
    assert(HADDR == B)
    assert(HBURST == Hburst.INCR4)
    assert(HREADYOUT)
    assert(HRDATA == Data(A))

    // T7
    dut.cd.waitSampling()
    assert(HTRANS == Htrans.SEQ)
    assert(HADDR == B + 4)
    assert(HBURST == Hburst.INCR4)
    assert(HREADYOUT)
    assert(HRDATA == Data(B))

    assert(readA.result == Data(A))
    assert(readA.duration == 6)
    assert(readA.waitStates == 4)
    assert(burst(0).result == Data(B))
    assert(burst(0).duration == 4)
    assert(burst(0).waitStates == 0)
  }

  simulateTest("Figure 3-14 Waited transfer, BUSY to SEQ") { dut =>
    val burst = dut.master.burstReadSeq(0x20, Hburst.incr4())
    burst(2).busy = 2

    dut.slave._setOnReads { req =>
      import dut.slave._
      delay(if (HADDR == 0x24) 4 else 0)(fromReader(Data))(req)
    }

    import dut.slave.{HTRANS, HADDR, HBURST}
    import dut.master.{HREADYOUT, HRDATA}

    burst.boot()

    // T0
    dut.cd.waitSampling()

    // T1
    dut.cd.waitSampling()
    assert(HTRANS == Htrans.SEQ)
    assert(HADDR == 0x24)
    assert(HBURST == Hburst.INCR4)
    assert(HREADYOUT)

    // T2
    dut.cd.waitSampling()
    assert(HTRANS == Htrans.BUSY)
    assert(HADDR == 0x28)
    assert(HBURST == Hburst.INCR4)
    assert(!HREADYOUT)

    // T3
    dut.cd.waitSampling()
    assert(HTRANS == Htrans.BUSY)
    assert(HADDR == 0x28)
    assert(HBURST == Hburst.INCR4)
    assert(!HREADYOUT)

    // T4
    dut.cd.waitSampling()
    assert(HTRANS == Htrans.SEQ)
    assert(HADDR == 0x28)
    assert(HBURST == Hburst.INCR4)
    assert(!HREADYOUT)

    // T5
    dut.cd.waitSampling()
    assert(HTRANS == Htrans.SEQ)
    assert(HADDR == 0x28)
    assert(HBURST == Hburst.INCR4)
    assert(!HREADYOUT)

    // T6
    dut.cd.waitSampling()
    assert(HTRANS == Htrans.SEQ)
    assert(HADDR == 0x28)
    assert(HBURST == Hburst.INCR4)
    assert(HREADYOUT)
    assert(HRDATA == Data(0x24))

    // T7
    dut.cd.waitSampling()
    assert(HTRANS == Htrans.SEQ)
    assert(HADDR == 0x2c)
    assert(HBURST == Hburst.INCR4)
    assert(HREADYOUT)
    assert(HRDATA == Data(0x28))

    assert(burst(0).duration == 2)
    assert(burst(0).waitStates == 0)
    assert(burst(1).duration == 6)
    assert(burst(1).waitStates == 4)
    assert(burst(2).duration == 6)
    assert(burst(2).waitStates == 0)
  }

  simulateTest(
    "Figure 3-15 Waited transfer, BUSY to NONSEQ for an undefined length burst"
  ) { dut =>
    val burst60 = dut.master.burstReadSeq(0x60, Hburst.incr(3))
    burst60(2).cancelAfter(2)
    val burst10 = dut.master.burstReadSeq(0x10, Hburst.incr4())

    dut.slave._setOnReads { req =>
      import dut.slave._
      delay(if (req.addr == 0x64) 4 else 0)(fromReader(Data))(req)
    }

    import dut.slave.{HTRANS, HADDR, HBURST}
    import dut.master.{HREADYOUT, HRDATA}

    (burst60 >> burst10).boot()

    // T0
    dut.cd.waitSampling()

    // T1
    dut.cd.waitSampling()
    assert(HTRANS == Htrans.SEQ)
    assert(HADDR == 0x64)
    assert(HBURST == Hburst.INCR)
    assert(HREADYOUT)

    // T2
    dut.cd.waitSampling()
    assert(HTRANS == Htrans.BUSY)
    assert(HADDR == 0x68)
    assert(HBURST == Hburst.INCR)
    assert(!HREADYOUT)

    // T3
    dut.cd.waitSampling()
    assert(HTRANS == Htrans.BUSY)
    assert(HADDR == 0x68)
    assert(HBURST == Hburst.INCR)
    assert(!HREADYOUT)

    // T4
    dut.cd.waitSampling()
    assert(HTRANS == Htrans.NONSEQ)
    assert(HADDR == 0x10)
    assert(HBURST == Hburst.INCR4)
    assert(!HREADYOUT)

    // T5
    dut.cd.waitSampling()
    assert(HTRANS == Htrans.NONSEQ)
    assert(HADDR == 0x10)
    assert(HBURST == Hburst.INCR4)
    assert(!HREADYOUT)

    // T6
    dut.cd.waitSampling()
    assert(HTRANS == Htrans.NONSEQ)
    assert(HADDR == 0x10)
    assert(HBURST == Hburst.INCR4)
    assert(HREADYOUT)
    assert(HRDATA == Data(0x64))

    // T7
    dut.cd.waitSampling()
    assert(HTRANS == Htrans.SEQ)
    assert(HADDR == 0x14)
    assert(HBURST == Hburst.INCR4)
    assert(HREADYOUT)
    assert(HRDATA == Data(0x10))

    assert(burst60(0).result == Data(0x60))
    assert(burst60(0).duration == 2)
    assert(burst60(0).waitStates == 0)
    assert(burst60(1).result == Data(0x64))
    assert(burst60(1).duration == 6)
    assert(burst60(1).waitStates == 4)
    assert(burst60(2).isDone == false)
    assert(burst60(2).duration == 2)
    assert(burst60(2).waitStates == 0)
    assert(burst10(0).result == Data(0x10))
    assert(burst10(0).duration == 4)
    assert(burst10(0).waitStates == 0)
  }

  simulateTest(
    "Figure 3-16 Address changes during a waited transfer, with an IDLE transfer"
  ) { dut =>
    val skip = () => true
    val readA = dut.master.Read(A)
    val idleY = dut.master.Idle(Y, skip)
    val idleZ = dut.master.Idle(Z, skip)
    val burst = dut.master.burstReadSeq(B, Hburst.incr4())

    dut.slave._setOnReads { req =>
      import dut.slave._
      delay(if (req.addr == A) 4 else 0)(fromReader(Data))(req)
    }

    import dut.slave.{HTRANS, HADDR, HBURST}
    import dut.master.{HREADYOUT, HRDATA}

    (readA >> idleY >> idleZ >> burst).boot()

    // T1
    dut.cd.waitSampling()
    assert(HTRANS == Htrans.NONSEQ)
    assert(HADDR == A)
    assert(HBURST == Hburst.SINGLE)
    assert(HREADYOUT)

    // T2
    dut.cd.waitSampling()
    assert(HTRANS == Htrans.IDLE)
    assert(HADDR == Y)
    assert(!HREADYOUT)

    // T3
    dut.cd.waitSampling()
    assert(HTRANS == Htrans.IDLE)
    assert(HADDR == Z)
    assert(!HREADYOUT)

    // T4
    dut.cd.waitSampling()
    assert(HTRANS == Htrans.NONSEQ)
    assert(HADDR == B)
    assert(HBURST == Hburst.INCR4)
    assert(!HREADYOUT)

    // T5
    dut.cd.waitSampling()
    assert(HTRANS == Htrans.NONSEQ)
    assert(HADDR == B)
    assert(HBURST == Hburst.INCR4)
    assert(!HREADYOUT)

    // T6
    dut.cd.waitSampling()
    assert(HTRANS == Htrans.NONSEQ)
    assert(HADDR == B)
    assert(HBURST == Hburst.INCR4)
    assert(HREADYOUT)
    assert(HRDATA == Data(A))

    // T7
    dut.cd.waitSampling()
    assert(HTRANS == Htrans.SEQ)
    assert(HADDR == B + 4)
    assert(HBURST == Hburst.INCR4)
    assert(HREADYOUT)
    assert(HRDATA == Data(B))

    assert(readA.result == Data(A))
    assert(readA.duration == 6)
    assert(readA.waitStates == 4)
    assert(burst(0).result == Data(B))
    assert(burst(0).duration == 4)
    assert(burst(0).waitStates == 0)
  }

  simulateTest(
    "Figure 3-17 Address changes during a waited transfer, after an ERROR"
  ) { dut =>
    val burst = dut.master.burstReadSeq(0x20, Hburst.incr4())
    val idle = dut.master.Idle(0xc0)

    dut.slave._setOnReads { req =>
      import dut.slave._
      if (req.addr == 0x24)
        delay(2)(errorBuilder)(req)
      else
        behavior2builder(fromReader(Data))(req)
    }

    import dut.slave.{HTRANS, HADDR, HBURST}
    import dut.master.{HREADYOUT, HRESP}

    burst(1).onError(idle)
    burst.boot()

    // T0
    dut.cd.waitSampling()

    // T1
    dut.cd.waitSampling()
    assert(HTRANS == Htrans.SEQ)
    assert(HADDR == 0x24)
    assert(HBURST == Hburst.INCR4)
    assert(HREADYOUT)
    assert(HRESP == Hresp.OKAY)

    // T2
    dut.cd.waitSampling()
    assert(HTRANS == Htrans.SEQ)
    assert(HADDR == 0x28)
    assert(HBURST == Hburst.INCR4)
    assert(!HREADYOUT)
    assert(HRESP == Hresp.OKAY)

    // T3
    dut.cd.waitSampling()
    assert(HTRANS == Htrans.SEQ)
    assert(HADDR == 0x28)
    assert(HBURST == Hburst.INCR4)
    assert(!HREADYOUT)
    assert(HRESP == Hresp.OKAY)

    // T4
    dut.cd.waitSampling()
    assert(HTRANS == Htrans.SEQ)
    assert(HADDR == 0x28)
    assert(HBURST == Hburst.INCR4)
    assert(!HREADYOUT)
    assert(HRESP == Hresp.ERROR)

    // T5
    dut.cd.waitSampling()
    assert(HTRANS == Htrans.IDLE)
    assert(HADDR == 0xc0)
    assert(HREADYOUT)
    assert(HRESP == Hresp.ERROR)

    // T6
    dut.cd.waitSampling()
    assert(HREADYOUT)
    assert(HRESP == Hresp.OKAY)

    assert(burst(1).duration == 4)
    assert(burst(1).waitStates == 2)
    assert(!burst(1).isDone)
    assert(burst(2).duration == 3)
    assert(burst(2).waitStates == 0)
    assert(!burst(2).isDone)
  }

  simulateTest("Section 3.7 Protection control example") { dut =>
    def idInt(a: Int): Int = a

    val read =
      dut.master.Read(0x10, hprot = Hprot.dataAccess.withUserAccess)

    var hasRead = false
    dut.slave._setOnReads { req =>
      import dut.slave._
      assert(req.HPROT == 1)

      val p = Hprot(req.HPROT)
      assert(p is Hprot.DataAccess)
      assert(p is Hprot.UserAccess)

      () => {
        hasRead = true
        done(0)
      }
    }

    read.run()
    assert(hasRead)

    val p =
      Hprot.dataAccess.withPriviledgedAccess.withNonBufferable.withNonCacheable

    // Properties can be checked
    assert(p is Hprot.NonCacheable)
    assert(p is Hprot.NonBufferable)
    assert(p is Hprot.PriviledgedAccess)
    assert(p is Hprot.DataAccess)

    // Get value explicitly or implicitly
    assert(p.value == 3)
    assert(idInt(p) == 3)

    // Property checks work on integers
    assert(Hprot.is(Hprot.NonCacheable)(3))
    assert(Hprot.is(Hprot.NonBufferable)(3))
    assert(Hprot.is(Hprot.PriviledgedAccess)(3))
    assert(Hprot.is(Hprot.DataAccess)(3))

    // With partial definition (other properties are defaulted)
    assert(Hprot.dataAccess.withPriviledgedAccess.value == 3)

    // Last property erases its previous negations
    assert(
      Hprot.dataAccess.withPriviledgedAccess.withBufferable.withNonBufferable.value == 3
    )
  }

  simulateTest("Figure 5-1 ERROR response") { dut =>
    val write = dut.master.Write(A, Data(A))
    write.onError(dut.master.Idle())
    val read = dut.master.Read(B)

    val checker = new WriteChecker()
    dut.slave._setOnWrites { req =>
      import dut.slave._
      delay(1)(errorBuilder)(req)
    }

    (write >> read).boot()

    import dut.slave.{HTRANS, HADDR, HWRITE, HWDATA}
    import dut.master.{HREADYOUT, HRESP}

    // T1
    dut.cd.waitSampling()
    assert(HTRANS == Htrans.NONSEQ)
    assert(HADDR == A)
    assert(HWRITE)
    assert(HREADYOUT)
    assert(HRESP == Hresp.OKAY)

    // T2
    dut.cd.waitSampling()
    assert(HADDR == B)
    assert(HWDATA == Data(A))
    assert(!HREADYOUT)
    assert(HRESP == Hresp.OKAY)

    // T3
    dut.cd.waitSampling()
    assert(HADDR == B)
    assert(HWDATA == Data(A))
    assert(!HREADYOUT)
    assert(HRESP == Hresp.ERROR)

    // T4
    dut.cd.waitSampling()
    assert(HTRANS == Htrans.IDLE)
    assert(HREADYOUT)
    assert(HRESP == Hresp.ERROR)

    // T5
    dut.cd.waitSampling()
    assert(HREADYOUT)
    assert(HRESP == Hresp.OKAY)
  }
}
