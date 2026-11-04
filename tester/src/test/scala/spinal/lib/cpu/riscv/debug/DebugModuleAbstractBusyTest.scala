package spinal.lib.cpu.riscv.debug

import org.scalatest.funsuite.AnyFunSuite
import spinal.core.sim._
import scala.collection.mutable.ArrayBuffer

/** Direct DMI regression: the hart deliberately stalls so accesses occur while busy.
  * Run: sbt "tester/testOnly spinal.lib.cpu.riscv.debug.DebugModuleAbstractBusyTest"
  */
class DebugModuleAbstractBusyTest extends AnyFunSuite {
  lazy val compiled = SimConfig.compile(DebugModule(DebugModuleParameter(
    version = 2, harts = 1, progBufSize = 2, datacount = 1,
    hartsConfig = List(DebugModuleCpuConfig(32, 0, false)))))

  val original = BigInt("00261005", 16) // read x5, then execute program buffer
  val replacement = BigInt("00221006", 16) // read x6, no postexec
  val nop = BigInt("00000013", 16)
  val ebreak = BigInt("00100073", 16)
  val readX5 = BigInt("7b429073", 16) // csrw 0x7b4, x5

  class Bench(dut: DebugModule) {
    val cd = dut.clockDomain
    val hart = dut.io.harts(0)
    val instructions = ArrayBuffer[BigInt]()
    val dataWrites = ArrayBuffer[BigInt]()
    dut.io.ctrl.cmd.valid #= false
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
    cd.forkStimulus(10)
    SimTimeout(100000)
    fork {
      while (true) {
        cd.waitSampling()
        if (hart.dmToHart.valid.toBoolean) {
          if (hart.dmToHart.op.toEnum == DebugDmToHartOp.EXECUTE)
            instructions += hart.dmToHart.data.toBigInt
          if (hart.dmToHart.op.toEnum == DebugDmToHartOp.DATA)
            dataWrites += hart.dmToHart.data.toBigInt
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
    def awaitInstruction(count: Int): Unit = {
      var timeout = 100
      while (instructions.size < count && timeout > 0) {
        cd.waitSampling(); timeout -= 1
      }
      assert(instructions.size == count, s"Expected $count instructions, got $instructions")
    }
    def commit(): Unit = {
      hart.commit #= true
      cd.waitSampling()
      hart.commit #= false
      cd.waitSampling(2)
    }
    def finish(): Unit = {
      commit()
      awaitInstruction(2)
      commit()
      awaitInstruction(3)
      hart.ebreak #= true
      cd.waitSampling()
      hart.ebreak #= false
      cd.waitSampling(3)
      assert(!busy)
      assert(instructions.toList == List(readX5, nop, ebreak))
    }
    def start(): Unit = {
      write(0x17, original)
      awaitInstruction(1)
      assert(busy)
    }
    cd.waitSampling(5)
    write(0x10, 1)
    assert((read(0x11) & 15) == 2) // no compliance version bump
    write(0x20, nop)
    write(0x21, ebreak)
    write(0x04, 0x12345678)
  }

  val accesses = Seq(
    ("command write", true, 0x17, replacement),
    ("abstractcs write", true, 0x16, BigInt(0x700)),
    ("abstractauto write", true, 0x18, BigInt(0x30001)),
    ("data write", true, 0x04, BigInt(0xdeadbeefL)),
    ("data read", false, 0x04, BigInt(0)),
    ("progbuf write", true, 0x21, nop),
    ("progbuf read", false, 0x20, BigInt(0)))

  for ((name, wr, addr, value) <- accesses) test(s"busy $name preserves the running command") {
    compiled.doSim { dut =>
      val b = new Bench(dut)
      import b._
      start()
      dmi(wr, addr, value)
      assert(busy)
      finish()
      assert(error == 1)
      assert(read(0x18) == 0)
      assert(dataWrites.toList == List(BigInt(0x12345678)))
      write(0x16, 0x700)
      assert(error == 0)
      // Autoexec proves the remembered command survived the rejected write.
      instructions.clear()
      write(0x18, 1)
      read(0x04)
      awaitInstruction(1)
      finish()
      assert(error == 0)
    }
  }

  test("command writes with a preexisting error do not replace the remembered command") {
    compiled.doSim { dut =>
      val b = new Bench(dut)
      import b._
      start()
      write(0x17, replacement)
      finish()
      assert(error == 1)
      instructions.clear()
      write(0x17, replacement)
      cd.waitSampling(5)
      assert(instructions.isEmpty)
      write(0x16, 0x700)
      write(0x18, 1 << 16)
      assert(read(0x20) == 0) // zero-read program buffer still triggers autoexec
      awaitInstruction(1)
      finish()
      assert(error == 0)
    }
  }

  test("busy error survives a later program buffer exception") {
    compiled.doSim { dut =>
      val b = new Bench(dut)
      import b._
      start()
      read(0x20)
      commit()
      awaitInstruction(2)
      hart.exception #= true
      cd.waitSampling()
      hart.exception #= false
      cd.waitSampling(3)
      assert(!busy)
      assert(error == 1)
    }
  }

  test("busy clear cannot erase an error and simultaneous exception loses to busy") {
    compiled.doSim { dut =>
      val b = new Bench(dut)
      import b._
      start()
      commit()
      awaitInstruction(2)
      // The DMI transfer and exception are sampled on the same edge.
      hart.exception #= true
      write(0x16, 0x700)
      hart.exception #= false
      cd.waitSampling(3)
      assert(!busy)
      assert(error == 1)
      write(0x16, 0x100)
      assert(error == 0)
      instructions.clear()
      start()
      read(0x04)
      write(0x16, 0x700)
      finish()
      assert(error == 1)
    }
  }

  test("ordinary command errors remain sticky and clear by writing ones") {
    compiled.doSim { dut =>
      val b = new Bench(dut)
      import b._
      write(0x17, BigInt("ff000000", 16))
      cd.waitSampling(5)
      assert(!busy)
      assert(error == 2)
      write(0x17, original)
      assert(instructions.isEmpty)
      write(0x16, 0x100) // bit zero does not clear error bit one
      assert(error == 2)
      write(0x16, 0x200)
      assert(error == 0)
      start()
      commit()
      awaitInstruction(2)
      hart.exception #= true
      cd.waitSampling()
      hart.exception #= false
      cd.waitSampling(3)
      assert(!busy)
      assert(error == 3)
    }
  }

  test("status reads while busy are harmless and normal completion succeeds") {
    compiled.doSim { dut =>
      val b = new Bench(dut)
      import b._
      start()
      read(0x16)
      read(0x18)
      finish()
      assert(error == 0)
    }
  }
}
