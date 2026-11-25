package spinal.core

import spinal.core.internals.{MULTIPLE_RAM, MemBitsMaskKind, SINGLE_RAM}
import spinal.core.sim._
import spinal.tester.{SpinalAnyFunSuite, SpinalSimFunSuite}

class MemBitsMaskKindDut(withInit: Boolean) extends Component {
  val mem = Mem(Bits(32 bits), 16).simPublic()
  if (withInit) mem.initBigInt((0 until 16).map(BigInt(_) * 0x01010101)) else mem.randBoot()
  val wr = new Bundle {
    val valid   = in port Bool()
    val address = in port UInt(4 bits)
    val data    = in port Bits(32 bits)
    val mask    = in port Bits(4 bits)
  }
  mem.write(wr.address, wr.data, wr.valid, wr.mask)
  val rd = new Bundle {
    val address = in port UInt(4 bits)
    val data    = out port mem.readSync(address)
  }
}

class SpinalSimMemBitsMaskKindTester extends SpinalSimFunSuite {
  // SINGLE_RAM is only supported by the Verilog backend
  ghdlEnabled = false

  for (kind <- Seq[MemBitsMaskKind](MULTIPLE_RAM, SINGLE_RAM); withInit <- Seq(true, false)) {
    test(s"${kind.getClass.getSimpleName.stripSuffix("$")} ${if (withInit) "initialContent" else "randBoot"}") {
      val config = SimConfig.withConfig(SimConfig._spinalConfig.copy(memBitsMaskKind = kind))
      config.compile(new MemBitsMaskKindDut(withInit)).doSim(seed = 42) { dut =>
        dut.clockDomain.forkStimulus(10)
        val model = Array.tabulate(16)(i => if (withInit) BigInt(i) * 0x01010101 else BigInt("ffffffff", 16))
        for (_ <- 0 until 500) {
          val wrAddress = simRandom.nextInt(16)
          val rdAddress = (wrAddress + 1 + simRandom.nextInt(15)) % 16
          val wrValid   = simRandom.nextBoolean()
          val wrData    = BigInt(32, simRandom)
          val wrMask    = simRandom.nextInt(16)
          dut.wr.valid #= wrValid
          dut.wr.address #= wrAddress
          dut.wr.data #= wrData
          dut.wr.mask #= wrMask
          dut.rd.address #= rdAddress
          dut.clockDomain.waitSampling()
          sleep(1)
          assert(dut.rd.data.toBigInt == model(rdAddress))
          if (wrValid) for (i <- 0 until 4 if (wrMask >> i & 1) == 1) {
            val field = BigInt(0xff) << (i * 8)
            model(wrAddress) = (model(wrAddress) & ~field) | (wrData & field)
          }
        }
        for (i <- 0 until 16) assert(dut.mem.getBigInt(i) == model(i))
      }
    }
  }
}

class MemBitsMaskKindEmissionTester extends SpinalAnyFunSuite {
  test("SINGLE_RAM emits one array with part-select writes") {
    val report = SpinalConfig(memBitsMaskKind = SINGLE_RAM, targetDirectory = "simWorkspace/memBitsMaskKind")
      .generateVerilog(new MemBitsMaskKindDut(withInit = true))
    val text = scala.io.Source.fromFile(report.generatedSourcesPaths.filter(_.endsWith(".v")).head).mkString
    assert(text.contains("reg [31:0] mem [0:15];"))
    assert(text.contains("mem[wr_address][15 : 8] <= wr_data[15 : 8];"))
    assert("\\$readmemb".r.findAllMatchIn(text).size == 1)
  }
}
