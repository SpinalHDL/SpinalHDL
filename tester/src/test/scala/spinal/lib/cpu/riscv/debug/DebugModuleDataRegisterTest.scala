package spinal.lib.cpu.riscv.debug

import org.scalatest.funsuite.AnyFunSuite
import spinal.core.sim._
import scala.collection.mutable.ArrayBuffer

/** data0-N keep what the debugger wrote until a command or the hart changes them.
  * Run: sbt "tester/testOnly spinal.lib.cpu.riscv.debug.DebugModuleDataRegisterTest"
  */
class DebugModuleDataRegisterTest extends AnyFunSuite {
  def parameter(datacount: Int, configs: DebugModuleCpuConfig*) = DebugModuleParameter(
    version = 2, harts = configs.size, progBufSize = 2, datacount = datacount,
    hartsConfig = configs)

  lazy val single = SimConfig.compile(DebugModule(parameter(1, DebugModuleCpuConfig(32, 0, false))))
  lazy val wide = SimConfig.compile(DebugModule(parameter(2, DebugModuleCpuConfig(32, 64, true))))
  lazy val twoHarts = SimConfig.compile(DebugModule(parameter(1,
    DebugModuleCpuConfig(32, 0, false),
    DebugModuleCpuConfig(32, 0, false))))

  val readX5 = BigInt("00221005", 16)  // Access Register, transfer, aarsize 2, x5
  val writeX5 = BigInt("00231005", 16) // same, write
  val patterns = Seq(BigInt("12345678", 16), BigInt("a5a5a5a5", 16), BigInt(0), BigInt("ffffffff", 16))

  class Bench(dut: DebugModule) {
    val cd = dut.clockDomain
    val harts = dut.io.harts
    val instructions = ArrayBuffer[BigInt]()
    // (hart, data register index, value) for every DATA transfer sent to a hart
    val dataWrites = ArrayBuffer[(Int, Int, BigInt)]()
    dut.io.ctrl.cmd.valid #= false
    for (hart <- harts) {
      hart.halted #= true
      hart.running #= false
      hart.unavailable #= false
      hart.haveReset #= false
      hart.exception #= false
      hart.commit #= false
      hart.ebreak #= false
      hart.redo #= false
      hart.regSuccess #= false
      hart.resume.rsp.valid #= false
      hart.hartToDm.valid #= false
      hart.hartToDm.address #= 0
      hart.hartToDm.data #= 0
    }
    cd.forkStimulus(10)
    SimTimeout(100000)
    fork {
      while (true) {
        cd.waitSampling()
        for ((hart, id) <- harts.zipWithIndex) {
          if (hart.dmToHart.valid.toBoolean) {
            val op = hart.dmToHart.op.toEnum
            if (op == DebugDmToHartOp.EXECUTE) instructions += hart.dmToHart.data.toBigInt
            if (op == DebugDmToHartOp.DATA)
              dataWrites += ((id, hart.dmToHart.address.toInt, hart.dmToHart.data.toBigInt))
          }
        }
      }
    }
    def dmi(wr: Boolean, addr: Int, data: BigInt = 0): BigInt = {
      val cmd = dut.io.ctrl.cmd
      cmd.valid #= true
      cmd.write #= wr
      cmd.address #= addr
      cmd.data #= data
      cd.waitSamplingWhere(cmd.ready.toBoolean)
      cmd.valid #= false
      cd.waitSamplingWhere(dut.io.ctrl.rsp.valid.toBoolean)
      assert(!dut.io.ctrl.rsp.error.toBoolean)
      dut.io.ctrl.rsp.data.toBigInt
    }
    def write(addr: Int, data: BigInt): Unit = { dmi(true, addr, data) }
    def read(addr: Int): BigInt = dmi(false, addr)
    def error: Int = ((read(0x16) >> 8) & 7).toInt
    def busy: Boolean = (read(0x16) & 0x1000) != 0
    def select(hart: Int): Unit = {
      write(0x10, (hart << 16) | 1)
      cd.waitSampling(2)
    }
    def issue(cmd: BigInt): Unit = {
      instructions.clear()
      write(0x17, cmd)
      var timeout = 100
      while (instructions.isEmpty && timeout > 0) {
        cd.waitSampling(); timeout -= 1
      }
      assert(instructions.size == 1, s"Expected one instruction, got $instructions")
    }
    // The hart writes a data register, as the injected csrw does for a register read.
    def hartWrites(hart: Int, index: Int, value: BigInt): Unit = {
      harts(hart).hartToDm.valid #= true
      harts(hart).hartToDm.address #= index
      harts(hart).hartToDm.data #= value
      cd.waitSampling()
      harts(hart).hartToDm.valid #= false
    }
    def commit(hart: Int = 0): Unit = {
      harts(hart).commit #= true
      cd.waitSampling()
      harts(hart).commit #= false
      cd.waitSampling(3)
    }
    cd.waitSampling(5)
    write(0x10, 1)
  }

  test("a debugger write reads back and still reaches the hart") {
    single.doSim { dut =>
      val b = new Bench(dut)
      import b._
      for (v <- patterns) {
        dataWrites.clear()
        write(0x04, v)
        assert(read(0x04) == v)
        assert(dataWrites.toList == List((0, 0, v)))
      }
      assert(error == 0)
    }
  }

  test("a register write leaves data0 as the debugger wrote it") {
    single.doSim { dut =>
      val b = new Bench(dut)
      import b._
      val v = BigInt("cafe0001", 16)
      write(0x04, v)
      issue(writeX5)
      commit()
      assert(!busy)
      assert(error == 0)
      assert(read(0x04) == v)
    }
  }

  test("a register read replaces data0 and a later debugger write replaces it again") {
    single.doSim { dut =>
      val b = new Bench(dut)
      import b._
      val fromDebugger = BigInt("11112222", 16)
      val fromHart = BigInt("33334444", 16)
      write(0x04, fromDebugger)
      issue(readX5)
      hartWrites(0, 0, fromHart)
      commit()
      assert(!busy)
      assert(error == 0)
      assert(read(0x04) == fromHart)
      write(0x04, fromDebugger)
      assert(read(0x04) == fromDebugger)
    }
  }

  test("a debugger write while busy is dropped and reported") {
    single.doSim { dut =>
      val b = new Bench(dut)
      import b._
      val before = BigInt("55556666", 16)
      val during = BigInt("deadbeef", 16)
      write(0x04, before)
      issue(writeX5)
      dataWrites.clear()
      write(0x04, during)
      commit()
      assert(!busy)
      assert(error == 1)
      assert(dataWrites.isEmpty)
      assert(read(0x04) == before)
    }
  }

  test("data0 and data1 are independent") {
    wide.doSim { dut =>
      val b = new Bench(dut)
      import b._
      val low = BigInt("01234567", 16)
      val high = BigInt("89abcdef", 16)
      write(0x04, low)
      write(0x05, high)
      assert(read(0x04) == low)
      assert(read(0x05) == high)
      write(0x04, high)
      assert(read(0x04) == high)
      assert(read(0x05) == high)
      hartWrites(0, 1, low)
      cd.waitSampling(2)
      assert(read(0x04) == high)
      assert(read(0x05) == low)
    }
  }

  test("the value read back does not depend on the selected hart") {
    twoHarts.doSim { dut =>
      val b = new Bench(dut)
      import b._
      val v = BigInt("0badf00d", 16)
      dataWrites.clear()
      write(0x04, v)
      assert(dataWrites.toSet == Set((0, 0, v), (1, 0, v)))
      select(1)
      assert(read(0x04) == v)
      issue(readX5)
      hartWrites(1, 0, BigInt(0x77))
      commit(1)
      assert(read(0x04) == 0x77)
    }
  }
}
