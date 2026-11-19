package spinal.lib.memory.sdram.dfi

import spinal.tester.code.SpinalAnyFunSuite

/**
  * Golden truth table for the DDR4 command encoding on the DFI control bus (JESD79-4B Table 16).
  *
  * Drives [[CmdTxd]] directly with one task per command type and samples the RAW DFI output
  * pins (actN / rasN / casN / weN + A10). Deliberately independent of [[DfiMemoryAgent]]'s
  * decoder, so encoder and decoder can no longer both be wrong in the same way while the
  * dataflow testers stay green. The expected constants are transcribed from JESD79-4B
  * Table 16 (active-low pin states, 1 = pin HIGH):
  *
  *   CMD   ACT_n  RAS_n/A16  CAS_n/A15  WE_n/A14  A10
  *   ------ ------ ---------- ---------- -------- -----
  *   ACT    L      RA16       RA15       RA14      -
  *   WR     H      H          L          L         L
  *   RD     H      H          L          H         L
  *   PRE    H      L          H          L         L
  *   PREA   H      L          H          L         H
  *   REF    H      L          L          H         -
  *   MRS    H      L          L          L         OP
  *   ZQCL   H      H          H          L         H
  *   NOP    H      H          H          H         -   (must never be produced)
  *
  * Run for rowWidth = 17 (full 17-bit row, all three multiplexed pins live) AND rowWidth = 16
  * (typical 64M-row part; RA16 absent). The rowWidth=16 case is the regression guard for the
  * old `row >> (rowWidth-3)` slicing, which coincidentally matched the spec only when
  * rowWidth == 17 and was off-by-one for every other width.
  */
class SdramCmdTruthTableSpec extends SpinalAnyFunSuite {
  import spinal.core._
  import spinal.core.sim._

  case class Pin(actN: Int, ras: Int, cas: Int, we: Int)

  private def runCase(rowWidth: Int): Unit = {
    val fr = 2
    val cmdPhase = 0
    val taskParam = TaskParameter(timingWidth = 5, refWidth = 23, cmdBufferSize = 1024, dataBufferSize = 1024, rspBufferSize = 1024)
    val taskConfig = TaskConfig(taskParam, contextWidth = 2, writeTokenInterfaceWidth = 2, writeTokenBufferSize = 16, canRead = true, canWrite = true)
    val sdramtime = SdramTiming(generation = 4, RFC = 260, RAS = 38, RP = 15, RCD = 15, WTR = 8, WTP = 0, RTP = 8, RRD = 6, REF = 64000, FAW = 35)
    val sdram = SdramConfig(
      SdramGeneration.DDR4,
      bgWidth = 1,
      cidWidth = 1,
      bankWidth = 4,
      columnWidth = 10,
      rowWidth = rowWidth,
      dataWidth = 16,
      ddrMHZ = 200,
      ddrWrLat = 4,
      ddrRdLat = 4,
      sdramtime = sdramtime
    )
    val timeConfig = DfiTimeConfig(
      frequencyRatio = fr,
      cmdPhase = cmdPhase,
      tPhyWrLat = sdram.tPhyWrlat,
      tPhyWrData = 0,
      tPhyWrCsGap = 3,
      tRddataEn = sdram.tRddataEn,
      tPhyRdlat = 4,
      tPhyRdCsGap = 3,
      tPhyRdCslat = 0,
      tPhyWrCsLat = 0,
      tWrlvlResp = 16
    )
    val dfiConfig = DfiConfig(
      chipSelectNumber = 1,
      dataSlice = 1,
      signalConfig = new DDR4SignalConfig(DfiFunctionConfig(), false),
      timeConfig = timeConfig,
      sdram = sdram
    )

    SimConfig.compile(CmdTxd(taskConfig, dfiConfig)).doSim { dut =>
      dut.clockDomain.forkStimulus(10)

      def clrFlags(): Unit = {
        dut.io.task.active #= false
        dut.io.task.write #= false
        dut.io.task.read #= false
        dut.io.task.precharge #= false
        dut.io.task.prechargeAll #= false
        dut.io.task.refresh #= false
        dut.io.task.mrs #= false
        dut.io.task.zq #= false
      }
      clrFlags()
      dut.io.task.address.byte #= 0
      dut.io.task.address.column #= 0
      dut.io.task.address.bank #= 0
      dut.io.task.address.row #= 0
      dut.io.task.address.cs #= 0
      dut.io.task.context #= 0
      dut.io.task.initCsN #= (1 << dfiConfig.chipSelectNumber) - 1 // all-ones: runtime one-hot CS mode
      dut.clockDomain.waitRisingEdge()

      def sample(): (Pin, Int, BigInt) = {
        val c = dut.io.cmd(cmdPhase).payload
        val a = dut.io.address(cmdPhase).payload
        assert(dut.io.cmd(cmdPhase).valid.toBoolean, "expected a valid command on phase 0")
        assert(!dut.io.cmd((1 + cmdPhase) % fr).valid.toBoolean, "unexpected valid command on the other phase")
        assert(c.csN.toBigInt == 0, s"csN must select rank 0, got ${c.csN}")
        assert(!(c.rasN.toBoolean && c.casN.toBoolean && c.weN.toBoolean), "encoding collapsed to NOP (RAS=CAS=WE=H)")
        (
          Pin(if (c.actN.toBoolean) 1 else 0, if (c.rasN.toBoolean) 1 else 0, if (c.casN.toBoolean) 1 else 0, if (c.weN.toBoolean) 1 else 0),
          ((a.address.toBigInt >> 10) & 1).toInt,
          a.address.toBigInt
        )
      }

      def fire(name: String, drive: => Unit): (Pin, Int, BigInt) = {
        clrFlags()
        drive
        dut.clockDomain.waitRisingEdge()
        val out = sample()
        clrFlags()
        dut.clockDomain.waitRisingEdge()
        out
      }

      def col(c: Int): Unit = dut.io.task.address.column #= c
      def bank(b: Int): Unit = dut.io.task.address.bank #= b
      def row(r: Int): Unit = dut.io.task.address.row #= r

      // (name, expected pins, expected A10 (-1 = don't care))
      val golden: Seq[(String, Pin, Int)] = Seq(
        ("WR", Pin(1, 1, 0, 0), 0),
        ("RD", Pin(1, 1, 0, 1), 0),
        ("PRE", Pin(1, 0, 1, 0), 0),
        ("PREA", Pin(1, 0, 1, 0), 1),
        ("REF", Pin(1, 0, 0, 1), -1),
        ("MRS", Pin(1, 0, 0, 0), -1),
        ("ZQCL", Pin(1, 1, 1, 0), 1)
      )
      golden.foreach { case (name, exp, a10Exp) =>
        val (got, a10, _) = fire(name, name match {
          case "WR" => { dut.io.task.write #= true; col(3) }
          case "RD" => { dut.io.task.read #= true; col(7) }
          case "PRE" => { dut.io.task.precharge #= true; bank(2) }
          case "PREA" => { dut.io.task.prechargeAll #= true }
          case "REF" => { dut.io.task.refresh #= true }
          case "MRS" => { dut.io.task.mrs #= true; bank(1); row(0x00B0) }
          case "ZQCL" => { dut.io.task.zq #= true }
        })
        assert(got == exp, s"$name: pins (actN,ras,cas,we)=$got, JESD79-4B Table 16 expects $exp")
        if (a10Exp >= 0) assert(a10 == a10Exp, s"$name: A10=$a10, expected $a10Exp")
      }

      // ACT: A16/A15/A14 pins carry RA16/RA15/RA14 (Table 16 NOTE 1); the DFI address bus
      // carries the full row independently of the pin multiplexing. Absent row bits (rowWidth
      // < 17) must read as 0.
      def actExp(r: Int): Pin = Pin(
        0,
        if (rowWidth > 16) (r >> 16) & 1 else 0,
        if (rowWidth > 15) (r >> 15) & 1 else 0,
        if (rowWidth > 14) (r >> 14) & 1 else 0
      )
      Seq(0x14000, 0x8000, 0x00000).foreach { r =>
        val rr = r & ((1 << rowWidth) - 1)
        val (got, _, addrBus) = fire("ACT", { dut.io.task.active #= true; row(rr); bank(5); col(0) })
        assert(got == actExp(rr), s"ACT row=0x${Integer.toHexString(rr)} (rowWidth=$rowWidth): pins=$got, expected ${actExp(rr)} (RA16/RA15/RA14 on RAS/CAS/WE)")
        assert(addrBus == rr, s"ACT row=0x${Integer.toHexString(rr)}: address bus=$addrBus, expected full row $rr")
      }
    }
  }

  test("DDR4 command encoding matches JESD79-4B Table 16 (rowWidth=17)") { runCase(17) }
  test("DDR4 command encoding matches JESD79-4B Table 16 (rowWidth=16)") { runCase(16) }
}
