package spinal.tester.scalatest

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.bus.bmb.{Bmb, BmbAlignedSpliter, BmbAligner, BmbParameter}
import spinal.lib.bus.bmb.sim.BmbMemoryAgent

// Pins the BmbAligner read-rsp last for tail-trimmed (unaligned-length) commands behind a
// BmbAlignedSpliter. The aligner rounds the command length up to its alignment and trims the
// extra tail rsp beats via transferCounter > context.transfers; the rsp last must land on the
// trimmed final beat.
//
// 6ee13a9f0 replaced the transferCounter-derived last with a pass-through of the spliter's
// command-level last, which sits on a (trimmed) tail beat and is lost: the read rsp then never
// terminates. This test fails on that version and passes with the transferCounter derivation.
class SpinalSimBmbAlignedSpliterAlignerTester extends SpinalAnyFunSuite {
  test("BmbAligner_alignedSpliter_tailTrimmedRead") {
    SimConfig.compile {
      val ip = BmbParameter(
        addressWidth = 16,
        dataWidth = 32,
        lengthWidth = 8,
        sourceWidth = 0,
        contextWidth = 0,
        canRead = true,
        canWrite = true,
        alignment = BmbParameter.BurstAlignement.WORD
      )
      val alignerOp = BmbAligner.outputParameter(ip.access, 5)
      val spliterOp = BmbAlignedSpliter.outputParameter(alignerOp, 32)
      val dut = new Component {
        val io = new Bundle {
          val input = slave(Bmb(ip))
          val output = master(Bmb(spliterOp))
        }
        val aligner = BmbAligner(ip, alignmentWidth = 5)
        val spliter = BmbAlignedSpliter(BmbParameter(alignerOp), lengthMax = 32)
        io.input <> aligner.io.input
        spliter.io.input << aligner.io.output
        io.output <> spliter.io.output
      }
      dut
    }.doSimUntilVoid("test") { dut =>
      dut.clockDomain.forkStimulus(10)
      val mem = new BmbMemoryAgent(1 << 16)
      mem.addPort(dut.io.output, busAddress = 0, clockDomain = dut.clockDomain, withDriver = true)
      for (a <- 0 until 0x80) mem.setByte(a, (a & 0xff).toByte)

      // Read 34 bytes at 0x20 (32B-aligned base, unaligned end). The aligner rounds the command
      // up to 64 bytes -> 2 split tasks -> 16 raw rsp beats; 7 tail beats are trimmed. Expect
      // exactly 9 beats (0x20..0x41), last on the final beat.
      val addr = 0x20
      val len = 33
      val expectBeats = len / 4 + 1 // 9

      dut.io.input.cmd.valid #= true
      dut.io.input.cmd.address #= addr
      dut.io.input.cmd.opcode #= Bmb.Cmd.Opcode.READ
      dut.io.input.cmd.length #= len
      dut.io.input.cmd.last #= true
      dut.clockDomain.waitSamplingWhere(dut.io.input.cmd.ready.toBoolean)
      dut.io.input.cmd.valid #= false
      dut.io.input.cmd.randomize()

      dut.io.input.rsp.ready #= true
      var beats = 0
      var dataOk = true
      var lastOnFinal = false
      var c = 0
      while (beats < expectBeats && c < 20000) {
        if (dut.io.input.rsp.valid.toBoolean) {
          val d = dut.io.input.rsp.data.toBigInt
          for (b <- 0 until 4) {
            val byteAddr = addr + beats * 4 + b
            val exp = (byteAddr & 0xff).toByte
            if (((d >> (8 * b)) & 0xff).toByte != exp) dataOk = false
          }
          beats += 1
          if (dut.io.input.rsp.last.toBoolean) lastOnFinal = beats == expectBeats
        }
        dut.clockDomain.waitSampling()
        c += 1
      }
      assert(beats == expectBeats, s"rsp did not terminate on the trimmed final beat: beats=$beats")
      assert(dataOk, "rsp data mismatch")
      assert(lastOnFinal, "rsp last not on final beat")
      simSuccess()
    }
  }
}
