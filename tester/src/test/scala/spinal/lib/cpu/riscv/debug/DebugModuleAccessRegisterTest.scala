package spinal.lib.cpu.riscv.debug

import org.scalatest.funsuite.AnyFunSuite
import spinal.core.sim._
import scala.collection.mutable.ArrayBuffer

/** Access Register cmderr split: a missing FPR is an exception, an unimplemented
  * option or access path is unsupported, and transfer=0 ignores regno and aarsize.
  * Run: sbt "tester/testOnly spinal.lib.cpu.riscv.debug.DebugModuleAccessRegisterTest"
  */
class DebugModuleAccessRegisterTest extends AnyFunSuite {
  def parameter(configs: DebugModuleCpuConfig*) = DebugModuleParameter(
    version = 2, harts = configs.size, progBufSize = 2, datacount = 1,
    hartsConfig = configs)

  lazy val noFpu = SimConfig.compile(DebugModule(parameter(DebugModuleCpuConfig(32, 0, false))))
  lazy val mixed = SimConfig.compile(DebugModule(parameter(
    DebugModuleCpuConfig(32, 0, false),
    DebugModuleCpuConfig(32, 32, false))))
  lazy val fpu = SimConfig.compile(DebugModule(parameter(DebugModuleCpuConfig(32, 64, true))))

  val ebreak = BigInt("00100073", 16)
  val readX5 = BigInt("7b429073", 16)
  val gprX5 = 0x1005
  val fprF0 = 0x1020
  val fprF3 = 0x1023

  def access(regno: Int, write: Boolean = false, transfer: Boolean = true, postExec: Boolean = false,
             aarpostincrement: Boolean = false, aarsize: Int = 2, bit23: Boolean = false): BigInt = {
    var v = BigInt(regno & 0xffff)
    if (write) v |= 1 << 16
    if (transfer) v |= 1 << 17
    if (postExec) v |= 1 << 18
    if (aarpostincrement) v |= 1 << 19
    v |= BigInt(aarsize & 7) << 20
    if (bit23) v |= 1 << 23
    v
  }

  class Bench(dut: DebugModule) {
    val cd = dut.clockDomain
    val harts = dut.io.harts
    val instructions = ArrayBuffer[BigInt]()
    val regOps = ArrayBuffer[(DebugDmToHartOp.E, Int)]()
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
        for (hart <- harts) {
          if (hart.dmToHart.valid.toBoolean) {
            val op = hart.dmToHart.op.toEnum
            if (op == DebugDmToHartOp.EXECUTE) instructions += hart.dmToHart.data.toBigInt
            if (op == DebugDmToHartOp.REG_READ || op == DebugDmToHartOp.REG_WRITE)
              regOps += ((op, hart.dmToHart.address.toInt))
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
    def clearError(): Unit = {
      write(0x16, 0x700)
      assert(error == 0)
    }
    def select(hart: Int): Unit = {
      write(0x10, (hart << 16) | 1)
      cd.waitSampling(2)
    }
    def issue(cmd: BigInt): Unit = {
      instructions.clear()
      regOps.clear()
      write(0x17, cmd)
    }
    def quiescent(): Unit = {
      cd.waitSampling(8)
      assert(!busy)
      assert(instructions.isEmpty)
      assert(regOps.isEmpty)
    }
    def commit(): Unit = {
      harts.foreach(_.commit #= true)
      cd.waitSampling()
      harts.foreach(_.commit #= false)
      cd.waitSampling(3)
    }
    def regDone(): Unit = {
      harts.foreach(_.regSuccess #= true)
      cd.waitSampling()
      harts.foreach(_.regSuccess #= false)
      cd.waitSampling(3)
    }
    def awaitInstruction(count: Int): Unit = {
      var timeout = 100
      while (instructions.size < count && timeout > 0) {
        cd.waitSampling(); timeout -= 1
      }
      assert(instructions.size == count, s"Expected $count instructions, got $instructions")
    }
    def awaitRegOp(): Unit = {
      var timeout = 100
      while (regOps.isEmpty && timeout > 0) {
        cd.waitSampling(); timeout -= 1
      }
      assert(regOps.nonEmpty, "Expected a register access")
    }
    cd.waitSampling(5)
    write(0x10, 1)
    assert((read(0x11) & 15) == 2)
    write(0x20, ebreak)
  }

  test("a missing FPR is an exception and does not run the program buffer") {
    noFpu.doSim { dut =>
      val b = new Bench(dut)
      import b._
      for (cmd <- Seq(access(fprF0, postExec = true), access(fprF0, write = true, postExec = true))) {
        issue(cmd)
        quiescent()
        assert(error == 3)
        clearError()
      }
    }
  }

  test("an illegal option on a missing FPR stays unsupported") {
    noFpu.doSim { dut =>
      val b = new Bench(dut)
      import b._
      val cmds = Seq(
        access(fprF0, aarsize = 1),
        access(fprF0, aarpostincrement = true),
        access(fprF0, bit23 = true),
        access(fprF0, aarsize = 5))
      for (cmd <- cmds) {
        issue(cmd)
        quiescent()
        assert(error == 2)
        clearError()
      }
    }
  }

  test("CSR and custom regnos stay unsupported") {
    noFpu.doSim { dut =>
      val b = new Bench(dut)
      import b._
      for (regno <- Seq(0x0300, 0x0fff, 0xc000)) {
        issue(access(regno))
        quiescent()
        assert(error == 2)
        clearError()
      }
    }
  }

  test("GPR size is checked and a legal read still completes") {
    noFpu.doSim { dut =>
      val b = new Bench(dut)
      import b._
      issue(access(gprX5, aarsize = 3))
      quiescent()
      assert(error == 2)
      clearError()
      issue(access(gprX5))
      awaitInstruction(1)
      assert(instructions.toList == List(readX5))
      commit()
      assert(!busy)
      assert(error == 0)
    }
  }

  test("transfer=0 ignores regno and aarsize and runs only the program buffer") {
    noFpu.doSim { dut =>
      val b = new Bench(dut)
      import b._
      issue(access(fprF0, transfer = false, postExec = true, aarsize = 0))
      awaitInstruction(1)
      assert(instructions.toList == List(ebreak))
      assert(regOps.isEmpty)
      harts(0).ebreak #= true
      cd.waitSampling()
      harts(0).ebreak #= false
      cd.waitSampling(3)
      assert(!busy)
      assert(error == 0)
    }
  }

  test("hartsel chooses exception or unsupported from that hart's flen") {
    mixed.doSim { dut =>
      val b = new Bench(dut)
      import b._
      issue(access(fprF0))
      quiescent()
      assert(error == 3)
      clearError()
      select(1)
      issue(access(fprF0))
      quiescent()
      assert(error == 2)
      clearError()
      select(0)
      issue(access(fprF3, write = true))
      quiescent()
      assert(error == 3)
    }
  }

  test("a supported FPR access reaches the hart and an oversized one is refused") {
    fpu.doSim { dut =>
      val b = new Bench(dut)
      import b._
      issue(access(fprF3, aarsize = 4))
      quiescent()
      assert(error == 2)
      clearError()
      issue(access(fprF3, aarsize = 3))
      awaitRegOp()
      assert(regOps.toList == List((DebugDmToHartOp.REG_READ, 3)))
      regDone()
      assert(!busy)
      assert(error == 0)
      issue(access(fprF3, write = true, aarsize = 3))
      awaitRegOp()
      assert(regOps.toList == List((DebugDmToHartOp.REG_WRITE, 3)))
      regDone()
      assert(!busy)
      assert(error == 0)
    }
  }
}
