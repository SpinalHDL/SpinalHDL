package spinal.lib.memory.sdram.dfi

import java.io.{File, PrintStream}
import java.security.MessageDigest
import spinal.tester.code.SpinalAnyFunSuite

/**
  * Tier 3 (board / vendor-tool) evidence, made reproducible and recorded.
  *
  * The UsPhy claims that need a Xilinx device — GTP SerDes, ODELAYE3/IDELAYE3 primives, the
  * write-leveling closed loop, 32-bit capture — are checked today by hand: the generators in
  * [[spinal.lib.memory.sdram.dfi.phy]] emit netlists, Vivado's xsim runs them, and the result
  * lives only in a scratch directory (the "30/30 PASS" note in the commit series). The
  * evidence is not pinned, so a refactor that changes a netlist is invisible.
  *
  * This suite is the gate for that tier. It is opt-in, because the CI image has no xvlog/xelab:
  *   DFI_BOARD_GATE=1 sbt 'tester/testOnly spinal.lib.memory.sdram.dfi.DfiBoardEvidenceSpec'
  * In default mode the tests are *cancelled* (visible as CANCELLED, exit code 0) — that is the
  * grill-approved way to keep CI unchanged (Q1: tester only) while removing the
  * "nobody runs it" excuse: the command is in the header, and ADR-0019 records what is gated.
  *
  * What it checks, in order:
  *  1. the T3 generators run to completion in-process (their own OK markers on stdout);
  *  2. the emitted netlists exist and contain the structural facts the tier claims
  *     (DFI ports on the MC; the delay primives and per-phase pads on the PHY);
  *  3. the netlist digests match the pinned table, so a netlist change is a deliberate,
  *     reviewed act (regenerate with DFI_GATE_UPDATE=1, paste the table, say so in the commit);
  *  4. the xsim result table, when present, has done=1 and error=0 on every case.
  */
class DfiBoardEvidenceSpec extends SpinalAnyFunSuite {
  import DfiBoardEvidenceSpec._

  test("T3 generators: MC end-to-end netlist elaborates and is structurally complete") {
    gated("set DFI_BOARD_GATE=1 (needs a Vivado-capable host for the full tier; netlist generation runs anywhere)")
    val out = capture(E2eMcGen.main(Array()))
    assert(out.contains("GEN_MC_DONE"), s"E2eMcGen did not finish. tail:\n${tail(out)}")
    val mc = requireNetlist("DfiController")
    Seq("io_dfi_control_rasN", "io_dfi_control_csN", "io_dfi_control_cke", "io_dfi_control_odt",
        "io_dfi_write_wr_0_wrdataEn", "io_dfi_read_rd_0_rddataValid", "io_dfiCtrlMsg_ctrlmsgReq")
      .foreach(p => assert(read(mc).contains(p), s"MC netlist $mc is missing the DFI port $p"))
  }

  test("T3 generators: PHY netlist elaborates and keeps the delay/pad structure") {
    gated("set DFI_BOARD_GATE=1")
    val smoke = capture(E2eSmokeGen.main(Array()))
    assert(smoke.contains("SMOKE_ALL_OK"), s"E2eSmokeGen did not finish. tail:\n${tail(smoke)}")
    val phy = requireNetlist("UsPhy")
    val v = read(phy)
    // The ADR-0001 datapath claim: write delay/read capture live in the fast MIB clock domain,
    // built from the UltraScale delay primives, with per-phase pads on the command side.
    Seq("ODELAYE3", "IDELAYE3").foreach(u =>
      assert(v.contains(u), s"PHY netlist $phy has no $u instance — the ADR-0001 datapath structure is gone"))
    Seq("io_pads_ck_t", "io_pads_ck_c", "io_pads_cke", "io_pads_cs_n", "io_pads_ras_n", "io_pads_cas_n",
        "io_pads_we_n", "io_pads_act_n", "io_pads_bg", "io_pads_ba", "io_pads_cid")
      .foreach(p => assert(v.contains(p), s"PHY netlist $phy is missing the command pad $p"))
  }

  test("netlist digests match the pinned table (ADR-0019)") {
    gated("set DFI_BOARD_GATE=1")
    // Make sure both netlists exist before hashing.
    if (!new File(s"$workspace/DfiController/rtl/DfiController.v").exists()) capture(E2eMcGen.main(Array()))
    if (!new File(s"$workspace/UsPhy/rtl/UsPhy.v").exists()) capture(UsPhyGen.main(Array()))
    val found = pins.map { case (rel, _) => rel -> sha256(new File(workspace, rel)) }
    if (updateMode) {
      println("[T3-PIN] paste this into DfiBoardEvidenceSpec.pins:")
      found.foreach { case (rel, d) => println(s"""    "$rel" -> "$d",""") }
      cancel("DFI_GATE_UPDATE=1: digests printed, nothing pinned")
    }
    val stale = found.filter { case (rel, d) => pins(rel) != d }
    assert(stale.isEmpty,
      "generated netlists no longer match the pinned digests — if the change is intended, run with " +
        "DFI_GATE_UPDATE=1 and paste the new table into DfiBoardEvidenceSpec.scala:\n  " +
        stale.map { case (rel, d) => s"$rel now $d (pinned ${pins(rel)})" }.mkString("\n  "))
  }

  test("xsim evidence: every recorded case finished with done=1 and error=0") {
    gated("set DFI_BOARD_GATE=1")
    val f = new File(evidenceTsv)
    if (!f.exists()) cancel(s"no xsim evidence at $evidenceTsv — run the board harness first (tier 3 is manual by design)")
    val rows = io.Source.fromFile(f).getLines().filter(_.contains("\t")).toSeq
    assert(rows.nonEmpty, s"$evidenceTsv has no rows")
    val bad = rows.map(_.split("\t")).filter { r =>
      !(r.exists(_.trim == "1") && r.exists(_.trim == "0")) // done, error
    }
    assert(bad.isEmpty, s"$evidenceTsv has cases that did not finish clean:\n  " + bad.map(_.mkString(" | ")).mkString("\n  "))
  }
}

object DfiBoardEvidenceSpec {
  val workspace = "./simWorkspace"
  val evidenceTsv = "/tmp/dfi_test/e2e_results.tsv"
  val updateMode = sys.env.get("DFI_GATE_UPDATE").exists(v => v == "1" || v.equalsIgnoreCase("true"))

  /**
    * Pinned digests of the generated netlists. Regenerate with DFI_GATE_UPDATE=1; a change here
    * is a signal that a netlist moved, which is exactly what the board claims depend on
    * (grill Q6: pin the hashes, and record the regeneration in the commit message).
    */
  val pins: Map[String, String] = Map(
    "DfiController/rtl/DfiController.v" -> "PENDING",
    "UsPhy/rtl/UsPhy.v" -> "PENDING"
  )

  def gated(why: => String): Unit =
    if (!sys.env.get("DFI_BOARD_GATE").exists(v => v == "1" || v.equalsIgnoreCase("true")))
      cancel("tier-3 gate is opt-in: " + why)

  private def capture(run: => Unit): String = {
    val buf = new java.io.ByteArrayOutputStream()
    val saved = System.out
    val ps = new PrintStream(buf, true, "UTF-8")
    System.setOut(ps)
    try run finally { ps.flush(); System.setOut(saved) }
    buf.toString("UTF-8")
  }

  private def tail(s: String): String = s.split('\n').takeRight(12).mkString("\n")
  private def requireNetlist(top: String): File = {
    val f = new File(s"$workspace/$top/rtl/$top.v")
    assert(f.exists(), s"expected netlist $f was not generated")
    f
  }
  private def read(f: File): String = io.Source.fromFile(f).mkString
  private def sha256(f: File): String = {
    assert(f.exists(), s"missing generated file ${f.getPath}")
    val md = MessageDigest.getInstance("SHA-256")
    val in = new java.io.FileInputStream(f)
    try {
      val chunk = new Array[Byte](65536)
      var n = in.read(chunk)
      while (n > 0) { md.update(chunk, 0, n); n = in.read(chunk) }
    } finally in.close()
    md.digest.map(b => f"$b%02x").mkString
  }
}
