package spinal.lib.memory.sdram.dfi

import java.io.{File, PrintStream}
import java.security.MessageDigest
import spinal.lib.memory.sdram.dfi.phy.{E2eMcGen, E2eSmokeGen, UsPhyGen}
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

  /** Opt-in switch: cancelled (not silently skipped) when the env flag is absent. */
  private def gate(): Unit =
    if (!sys.env.get("DFI_BOARD_GATE").exists(v => v == "1" || v.equalsIgnoreCase("true")))
      cancel("tier-3 gate is opt-in: set DFI_BOARD_GATE=1 (grill Q1/Q15: the CI job command stays unchanged)")

  test("T3 generators: MC end-to-end netlist elaborates and is structurally complete") {
    gate()
    val out = capture(E2eMcGen.main(Array()))
    assert(out.contains("GEN_MC_DONE"), s"E2eMcGen did not finish. tail:\n${tail(out)}")
    val mc = requireNetlist("DfiController/rtl/DfiController.v")
    val v = read(mc)
    // Only the channels E2eMcGen's config instantiates are checked: its signal config leaves the
    // message, low-power, status, update and training groups out (lib/.../dfi/phy/E2eMcGen.scala),
    // so demanding io_dfiCtrlMsg_* here greps for a bundle that does not exist. Full channel
    // coverage is DfiNetlistLintSpec's job (all capabilities elaborated).
    val channels = Seq("io_dfi_control_rasN", "io_dfi_control_csN", "io_dfi_control_cke",
      "io_dfi_control_odt", "io_dfi_write_wr_0_wrdataEn", "io_dfi_read_rd_0_rddataValid")
    channels.foreach(p => assert(v.contains(p), s"MC netlist $mc is missing the DFI port $p"))
    val mcTied = channels.filter(p => isConstantDriver(v, p))
    assert(mcTied.isEmpty, "the board MC ties a command/data channel to a constant:\n  " + mcTied.mkString("\n  "))
    println(s"[T3] MC netlist $mc: ${channels.size} channels declared, none tied to a constant")
  }

  test("T3 generators: PHY netlist elaborates and keeps the delay/pad structure") {
    gate()
    // E2eSmokeGen also drops a completion marker file: in this suite the MC test elaborates
    // first in the same JVM, and after that E2eSmokeGen's println output vanishes from both the
    // stdout capture and the log (observed; some stream reference cached during the first
    // elaboration). The marker file — deleted here first, so it can only come from THIS run —
    // is the authoritative completion proof; the stdout marker is the fast path.
    val marker = new File(workspace, "e2e_smoke.marker")
    if (marker.exists()) marker.delete()
    val smoke = capture(E2eSmokeGen.main(Array()))
    val finished = smoke.contains("SMOKE_ALL_OK") ||
      (marker.exists() && io.Source.fromFile(marker).mkString.trim == "SMOKE_ALL_OK")
    assert(finished, s"E2eSmokeGen did not finish. markerExists=${marker.exists()} absPath=${marker.getAbsolutePath} cwd=${new File(".").getAbsolutePath} content=[${try { io.Source.fromFile(marker).mkString } catch { case _: Throwable => "<unreadable>" }}] smokeLen=${smoke.length} tail:\n${tail(smoke)}")
    val phy = requireNetlist("UsPhy.v")
    val v = read(phy)
    // The ADR-0001 datapath claim: write delay/read capture live in the fast MIB clock domain,
    // built from the UltraScale delay primives, with per-phase pads on the command side.
    Seq("ODELAYE3", "IDELAYE3").foreach(u =>
      assert(v.contains(u), s"PHY netlist $phy has no $u instance — the ADR-0001 datapath structure is gone"))
    Seq("pads_ck_t", "pads_ck_c", "pads_cke_0", "pads_cs_n_0", "pads_ras_n", "pads_cas_n",
        "pads_we_n", "pads_act_n", "pads_bg_0", "pads_ba_0", "pads_cid_0")
      .foreach(p => assert(v.contains(p), s"PHY netlist $phy is missing the command pad $p"))
  }

  test("netlist digests match the pinned table (ADR-0019)") {
    gate()
    // ALWAYS regenerate before hashing: the SpinalSim suites share the same workspace
    // paths and overwrite these files with their own elaborations (different digests),
    // so a pre-existing file is not trustworthy evidence of what the generators emit.
    capture(E2eMcGen.main(Array()))
    capture(UsPhyGen.main(Array()))
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
    gate()
    val f = new File(evidenceTsv)
    if (!f.exists()) cancel(s"no xsim evidence at $evidenceTsv — run the board harness first (tier 3 is manual by design)")
    // Harness row format: RESULT<TAB>case<TAB>rc=N<TAB>pass=yes|no<TAB>time<TAB>description
    val rows = io.Source.fromFile(f).getLines().filter(_.startsWith("RESULT")).toSeq
    assert(rows.nonEmpty, s"$evidenceTsv has no RESULT rows")
    val bad = rows.map(_.split("\t")).filter { r =>
      !r.exists(_.trim == "rc=0") || !r.exists(_.trim == "pass=yes")
    }
    assert(bad.isEmpty, s"$evidenceTsv has cases that did not finish clean (need rc=0 and pass=yes):\n  " +
      bad.map(_.mkString(" | ")).mkString("\n  "))
    println(s"[T3] $evidenceTsv: ${rows.size} board cases, all rc=0 pass=yes")
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
    "DfiController/rtl/DfiController.v" -> "1aac8abd5589038e27ed22583184ec0351b90872d19eb9a8f27b0effaeac92d7",
    "UsPhy.v" -> "c045465f8b82ee71ef8bf62239675608a22a981bbd78c720649a6294df99fdd1"
  )



  /** Scala `println` goes through `Console.out`; SpinalHDL's logger writes to `System.out`. */
  private def capture(run: => Unit): String = {
    val buf = new java.io.ByteArrayOutputStream()
    val savedOut = System.out
    val savedErr = System.err
    val ps = new PrintStream(buf, true, "UTF-8")
    System.setOut(ps); System.setErr(ps)
    try Console.withOut(ps)(Console.withErr(ps)(run))
    finally { ps.flush(); System.setOut(savedOut); System.setErr(savedErr) }
    buf.toString("UTF-8")
  }

  private def tail(s: String): String = s.split('\n').takeRight(12).mkString("\n")

  /** True when every driver of `port` in `v` is a Verilog literal (i.e. a constant tie-off). */
  private def isConstantDriver(v: String, port: String): Boolean = {
    val q = java.util.regex.Pattern.quote(port)
    val re = (raw"(?m)^\s*assign\s+" + q + raw"\s*=\s*([^;]+);").r
    val lit = raw"(\d+'[bBoOdDhH][0-9a-fA-Fx_]+)|\(\d+'[bBoOdDhH][0-9a-fA-F_]+\)".r
    val ds = re.findAllMatchIn(v).map(_.group(1).trim).toSeq
    ds.nonEmpty && ds.forall(d => lit.pattern.matcher(d.replace(" ", "")).matches())
  }
  /** `top` is the path under ./simWorkspace, e.g. "UsPhy/rtl/UsPhy.v". */
  private def requireNetlist(top: String): File = {
    val f = new File(s"$workspace/$top")
    assert(f.exists(), s"expected netlist $f was not generated")
    f
  }
  private def read(f: File): String = io.Source.fromFile(f).mkString
  /**
    * SHA-256 of the netlist **with the generation header removed**: SpinalHDL stamps the file
    * with the library version and the generation date, so hashing the raw bytes would break the
    * pin on any day it is regenerated. The logic body is what the board claims depend on.
    */
  private def sha256(f: File): String = {
    assert(f.exists(), s"missing generated file ${f.getPath}")
    val body = io.Source.fromFile(f).getLines()
      .filterNot(l => l.trim.startsWith("//") || l.trim.startsWith("`timescale"))
      .mkString("\n")
    MessageDigest.getInstance("SHA-256").digest(body.getBytes("UTF-8")).map(b => f"$b%02x").mkString
  }
}
