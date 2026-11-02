package spinal.lib.cpu.riscv.debug

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.tester.SpinalAnyFunSuite

import scala.collection.mutable
import scala.util.Random

/**
 * System Bus Access of the DebugModule, driven through its DebugBus (DMI) with a memory model
 * on io.sysBus. The model answers every command, writes included, as makeSysbusTilelink does.
 *
 * Run: sbt "tester/testOnly spinal.lib.cpu.riscv.debug.DebugModuleSysBusTest"
 */

case class DebugModuleSysBusTestTop() extends Component {
  val io = new Bundle {
    val ctrl   = slave(DebugBus(7))
    val sysBus = master(DebugSysBus())
  }

  val dm = DebugModule(DebugModuleParameter(
    version     = 2,
    harts       = 1,
    progBufSize = 2,
    datacount   = 1,
    hartsConfig = List(DebugModuleCpuConfig(xlen = 32, flen = 0, withFpuRegAccess = false)),
    withSysBus  = true
  ))

  // Stubbed hart, System Bus Access doesn't involve it.
  val hart = dm.io.harts(0)
  hart.halted      := False
  hart.running     := True
  hart.unavailable := False
  hart.haveReset   := False
  hart.exception   := False
  hart.commit      := False
  hart.ebreak      := False
  hart.redo        := False
  hart.regSuccess  := False
  hart.hartToDm.setIdle()
  hart.resume.rsp.setIdle()

  dm.io.ctrl   <> io.ctrl
  dm.io.sysBus <> io.sysBus
}

class DebugModuleSysBusTest extends SpinalAnyFunSuite {
  val DMCONTROL = 0x10
  val SBCS = 0x38
  val SBADDRESS0 = 0x39
  val SBDATA0 = 0x3c

  val SBBUSYERROR = BigInt(1) << 22
  val SBBUSY = BigInt(1) << 21
  val SBREADONADDR = BigInt(1) << 20
  val SBACCESS32 = BigInt(2) << 17
  val SBAUTOINCREMENT = BigInt(1) << 16
  val SBREADONDATA = BigInt(1) << 15
  val SBERROR_CLEAR = BigInt(7) << 12
  def sberror(sbcs : BigInt) = ((sbcs >> 12) & 7).toInt

  // Mapped memory of the model, everything else answers with an error.
  val MEM_BASE = 0x1000L
  val MEM_SIZE = 0x1000L
  val UNMAPPED = 0x8000L

  lazy val compiled = SimConfig.compile(DebugModuleSysBusTestTop())

  case class Access(write : Boolean, address : Long, data : BigInt)

  class Bench(dut : DebugModuleSysBusTestTop) {
    val cd = dut.clockDomain
    val mem = mutable.HashMap[Long, BigInt]()
    val accesses = mutable.ArrayBuffer[Access]()
    var latency = () => Random.nextInt(4)

    def memRead(address : Long) = mem.getOrElse(address, BigInt(0))

    dut.io.ctrl.cmd.valid #= false
    dut.io.sysBus.cmd.ready #= false
    dut.io.sysBus.rsp.valid #= false
    cd.forkStimulus(10)

    val slave = fork {
      val cmd = dut.io.sysBus.cmd
      val rsp = dut.io.sysBus.rsp
      // The registers of the dmactive domain hold their random simulation values until the first clock edge.
      cd.waitSampling(2)
      while (true) {
        cmd.ready #= true
        cd.waitSamplingWhere(cmd.valid.toBoolean)
        val write = cmd.wr.toBoolean
        val address = cmd.address.toLong
        val data = cmd.data.toBigInt
        cmd.ready #= false
        accesses += Access(write, address, data)
        val mapped = address >= MEM_BASE && address < MEM_BASE + MEM_SIZE
        if (mapped && write) mem(address) = data
        cd.waitSampling(latency())
        rsp.valid #= true
        rsp.error #= !mapped
        // A write acknowledge carries no data, drive something a buggy DM would visibly latch.
        rsp.data #= (if (mapped && !write) memRead(address) else BigInt("ffffffff", 16))
        cd.waitSampling()
        rsp.valid #= false
      }
    }

    def dmi(write : Boolean, address : Int, data : BigInt = 0) : BigInt = {
      val cmd = dut.io.ctrl.cmd
      cmd.valid #= true
      cmd.write #= write
      cmd.address #= address
      cmd.data #= data
      cd.waitSamplingWhere(cmd.ready.toBoolean)
      cmd.valid #= false
      cd.waitSamplingWhere(dut.io.ctrl.rsp.valid.toBoolean)
      dut.io.ctrl.rsp.data.toBigInt
    }
    def write(address : Int, data : BigInt) : Unit = dmi(true, address, data)
    def read(address : Int) : BigInt = dmi(false, address)

    def waitIdle() : BigInt = {
      var sbcs = read(SBCS)
      var timeout = 1000
      while ((sbcs & SBBUSY) != 0) {
        timeout -= 1
        assert(timeout > 0, "sbbusy is stuck")
        sbcs = read(SBCS)
      }
      sbcs
    }

    // dmactive, then a known sbcs with every sticky error cleared
    def init(sbcs : BigInt = SBACCESS32) : Unit = {
      cd.waitSampling(5)
      write(DMCONTROL, 1)
      write(SBCS, sbcs | SBBUSYERROR | SBERROR_CLEAR)
      accesses.clear()
    }
  }

  def bench(name : String)(body : Bench => Unit) : Unit = test(name) {
    compiled.doSim(name) { dut =>
      body(new Bench(dut))
    }
  }

  bench("sbcs reset value") { b =>
    import b._
    cd.waitSampling(5)
    write(DMCONTROL, 1)
    // sbversion 1, sbaccess 2, sbasize 32, sbaccess32
    assert(read(SBCS) == BigInt("20040404", 16))
  }

  bench("single write and read") { b =>
    import b._
    init()
    write(SBADDRESS0, MEM_BASE)
    write(SBDATA0, BigInt("11223344", 16))
    assert(sberror(waitIdle()) == 0)
    assert(accesses.toList == List(Access(true, MEM_BASE, BigInt("11223344", 16))))
    // The write acknowledge doesn't update sbdata0, and without sbautoincrement sbaddress0 stays
    assert(read(SBDATA0) == BigInt("11223344", 16))
    assert(read(SBADDRESS0) == MEM_BASE)

    mem(MEM_BASE + 4) = BigInt("cafef00d", 16)
    write(SBCS, SBACCESS32 | SBREADONADDR)
    write(SBADDRESS0, MEM_BASE + 4)
    assert(sberror(waitIdle()) == 0)
    assert(read(SBDATA0) == BigInt("cafef00d", 16))
    assert(read(SBADDRESS0) == MEM_BASE + 4)
    assert(accesses.size == 2)
  }

  bench("sbautoincrement on writes") { b =>
    import b._
    init(SBACCESS32 | SBAUTOINCREMENT)
    write(SBADDRESS0, MEM_BASE)
    val words = List.tabulate(8)(i => BigInt("a5000000", 16) + i)
    for ((word, i) <- words.zipWithIndex) {
      write(SBDATA0, word)
      assert(sberror(waitIdle()) == 0)
      assert(read(SBADDRESS0) == MEM_BASE + 4 * (i + 1))
    }
    assert(accesses.toList == words.zipWithIndex.map { case (word, i) => Access(true, MEM_BASE + 4 * i, word) })
    for ((word, i) <- words.zipWithIndex) assert(memRead(MEM_BASE + 4 * i) == word)
  }

  bench("sbautoincrement on reads") { b =>
    import b._
    val words = List.tabulate(8)(i => BigInt("5a000000", 16) + i)
    for ((word, i) <- words.zipWithIndex) mem(MEM_BASE + 0x100 + 4 * i) = word
    init(SBACCESS32 | SBAUTOINCREMENT | SBREADONADDR | SBREADONDATA)

    // The sbreadonaddr read increments sbaddress0 once it has completed
    write(SBADDRESS0, MEM_BASE + 0x100)
    assert(sberror(waitIdle()) == 0)
    assert(read(SBADDRESS0) == MEM_BASE + 0x104)

    // Each sbdata0 read returns the previous word and reads the next one
    for (i <- 0 until 7) {
      assert(read(SBDATA0) == words(i))
      assert(sberror(waitIdle()) == 0)
      assert(read(SBADDRESS0) == MEM_BASE + 0x100 + 4 * (i + 2))
    }
    write(SBCS, SBACCESS32)
    assert(read(SBDATA0) == words(7))
    waitIdle()
    assert(accesses.toList == List.tabulate(8)(i => Access(false, MEM_BASE + 0x100 + 4 * i, accesses(i).data)))
  }

  bench("bus error") { b =>
    import b._
    mem(MEM_BASE) = BigInt("12345678", 16)
    init(SBACCESS32 | SBAUTOINCREMENT | SBREADONADDR)
    write(SBADDRESS0, MEM_BASE)
    assert(sberror(waitIdle()) == 0)
    assert(read(SBDATA0) == BigInt("12345678", 16))

    // A failed read reports sberror, keeps sbdata0 and doesn't increment sbaddress0
    write(SBADDRESS0, UNMAPPED)
    assert(sberror(waitIdle()) == 7)
    assert(read(SBDATA0) == BigInt("12345678", 16))
    assert(read(SBADDRESS0) == UNMAPPED)

    // While sberror is set no access starts
    val count = accesses.size
    write(SBDATA0, 0x55)
    read(SBDATA0)
    cd.waitSampling(20)
    assert(accesses.size == count)
    assert(read(SBDATA0) == BigInt("12345678", 16))

    // A failed write does the same
    write(SBCS, SBACCESS32 | SBAUTOINCREMENT | SBERROR_CLEAR)
    assert(sberror(read(SBCS)) == 0)
    write(SBDATA0, 0x55)
    assert(sberror(waitIdle()) == 7)
    assert(read(SBADDRESS0) == UNMAPPED)
    assert(accesses.last == Access(true, UNMAPPED, 0x55))
  }

  bench("sbaddress0 write while sberror is set") { b =>
    import b._
    init(SBACCESS32 | SBREADONADDR)
    write(SBADDRESS0, UNMAPPED)
    assert(sberror(waitIdle()) == 7)
    val count = accesses.size

    // The address is written, only the sbreadonaddr read is suppressed
    write(SBADDRESS0, MEM_BASE + 0x40)
    cd.waitSampling(20)
    assert(read(SBADDRESS0) == MEM_BASE + 0x40)
    assert(accesses.size == count)
    assert(sberror(read(SBCS)) == 7)
  }

  bench("alignment error") { b =>
    import b._
    for (offset <- 1 to 3) {
      mem(MEM_BASE) = BigInt("0badc0de", 16)
      init(SBACCESS32 | SBREADONADDR | SBAUTOINCREMENT)

      write(SBADDRESS0, MEM_BASE + offset)
      cd.waitSampling(20)
      assert(sberror(read(SBCS)) == 3)
      assert(read(SBADDRESS0) == MEM_BASE + offset)

      write(SBCS, SBACCESS32 | SBERROR_CLEAR)
      write(SBDATA0, 0x77)
      cd.waitSampling(20)
      assert(sberror(read(SBCS)) == 3)

      write(SBCS, SBACCESS32 | SBREADONDATA | SBERROR_CLEAR)
      read(SBDATA0)
      cd.waitSampling(20)
      assert(sberror(read(SBCS)) == 3)

      // None of them reached the bus
      assert(accesses.isEmpty)
      assert(memRead(MEM_BASE) == BigInt("0badc0de", 16))
    }
  }

  bench("unsupported access size") { b =>
    import b._
    for (sbaccess <- List(0, 1, 3, 4)) {
      init(BigInt(sbaccess) << 17)
      write(SBADDRESS0, MEM_BASE)
      write(SBDATA0, 0x99)
      cd.waitSampling(20)
      assert(sberror(read(SBCS)) == 4)
      assert(accesses.isEmpty)
    }
  }

  bench("sbbusyerror") { b =>
    import b._
    mem(MEM_BASE) = BigInt("600dbeef", 16)
    latency = () => 40

    // sbdata0 read while a read is in progress
    init(SBACCESS32 | SBREADONADDR)
    write(SBADDRESS0, MEM_BASE)
    assert((read(SBCS) & SBBUSY) != 0)
    read(SBDATA0)
    var sbcs = waitIdle()
    assert((sbcs & SBBUSYERROR) != 0)
    assert(read(SBDATA0) == BigInt("600dbeef", 16))
    assert(accesses.size == 1)

    // sbdata0 write while a write is in progress, it is dropped
    init(SBACCESS32)
    write(SBADDRESS0, MEM_BASE + 8)
    write(SBDATA0, 0x1)
    write(SBDATA0, 0x2)
    sbcs = waitIdle()
    assert((sbcs & SBBUSYERROR) != 0)
    assert(accesses.toList == List(Access(true, MEM_BASE + 8, 0x1)))

    // sbaddress0 write while busy, it is dropped
    init(SBACCESS32)
    write(SBDATA0, 0x3)
    write(SBADDRESS0, MEM_BASE + 0x80)
    sbcs = waitIdle()
    assert((sbcs & SBBUSYERROR) != 0)
    assert(read(SBADDRESS0) == MEM_BASE + 8)

    // While sbbusyerror is set no access starts
    val count = accesses.size
    write(SBDATA0, 0x4)
    cd.waitSampling(60)
    assert(accesses.size == count)
  }
}
