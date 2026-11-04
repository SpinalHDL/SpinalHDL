package spinal.lib.memory.sdram.dfi

import scala.collection.mutable

/**
  * DRAM-side AC-timing watchdog, checked on the OBSERVED command stream.
  *
  * Why this exists (conformance audit item C3 / G2): the controller enforces timings from
  * `SdramConfig` counters, and the in-repo tests used expectations derived from those same
  * counters (`dut.dfiConfig.timeConfig.*`), so a wrong conversion factor agrees between the DUT
  * and the test and the suite stays green while a real DRAM fails. This watchdog closes that
  * loop from the DRAM's side:
  *
  *  - it consumes `DfiMemoryAgent`'s own decode of JESD79-4B Table 16 (a second decoder, not the
  *    DUT's encoder), and
  *  - its limits come from the ns fields of `SdramTiming` (`RAS/RP/RCD/RFC/FAW/RRD/WTR/RTP`,
  *    `REF_NS`, `WTR_S`, `RRD_S`) plus the generation's `tCCD_L/tCCD_S`, converted here
  *    ns -> DRAM cycles -> MC cycles. No expectation is read from `dut.*`.
  *
  * Unit note (the trap the audit found): `SdramConfig`'s `def tRAS/tRP/tRCD/tRFC/tFAW/tRRD/
  * tWTR/tRTP` are cycles at `cycleTime_ns = 1000/ddrMHZ`, i.e. DRAM-clock cycles, while the
  * scheduler counts MC cycles (1 MC cycle = `frequencyRatio` DRAM cycles). Loading a DRAM-cycle
  * value into an MC-cycle counter is `frequencyRatio` times too long: conservative on the
  * minima, and for the refresh period it stretches `tREFI` by `frequencyRatio`, which is a
  * data-retention violation. `TaskTimingConfig` is the layer that converts; this file converts
  * independently and asserts against the DRAM's requirement.
  *
  * Enforced (JESD79-4B sec. 4.19 key AC timing, parameter tables p.169-176):
  *   minima  tRAS, tRP, tRCD, tRFC, tRRD_S/tRRD_L, tFAW, tWTR_S/tWTR_L, tRTP, tCCD_S/tCCD_L,
  *           tRTW, tMRD, tMOD, tZQoper
  *   maxima  tRAS <= 9 x tREFI, and the tREFI window itself (a refresh must be issued inside
  *           every tREFI)
  * Not enforced: tXP/tXAR/tSR/tPD/tCKE (no CKE or low-power path exists in the controller yet),
  * tRFCsb, tECS, tAL.
  */
class DfiTimingWatchdog(cfg: DfiConfig, verbose: Boolean = sys.env.get("DFI_TIMING_WATCHDOG_VERBOSE").isDefined) {
  private val sdram = cfg.sdram
  private val fr = cfg.frequencyRatio
  private val tDramNs = 1000.0 / sdram.ddrMHZ
  private val tMcNs = tDramNs * fr

  private def mcOfNs(ns: Double): Long = math.ceil(ns / tMcNs).toLong
  private def mcOfDram(dramCycles: Int): Long = math.ceil(dramCycles.toDouble / fr).toLong

  val tRASmc = mcOfDram(sdram.tRAS)
  val tRPmc = mcOfDram(sdram.tRP)
  val tRCDmc = mcOfDram(sdram.tRCD)
  val tRFCmc = mcOfDram(sdram.tRFC)
  val tFAWmc = mcOfDram(sdram.tFAW)
  val tRRDmc = mcOfDram(sdram.tRRD)
  val tRRD_Smc = mcOfDram(sdram.tRRD_S)
  val tWTRmc = mcOfDram(sdram.tWTR)
  val tWTR_Smc = mcOfDram(sdram.tWTR_S)
  val tRTPmc = mcOfDram(sdram.tRTP)
  val tCCD_Lmc = mcOfDram(sdram.generation.tCCD_L)
  val tCCD_Smc = mcOfDram(sdram.generation.tCCD_S)
  val tRTWmc = mcOfDram(sdram.tRTW)
  val tMRDmc = mcOfNs(14.0) // JESD79-4B: tMRD = 14 ns
  val tMODmc = mcOfNs(14.0) // JESD79-4B: tMOD = max(14 ns, 4 nCK)
  val tZQCLmc = mcOfDram(512) // tZQoper = 512 nCK (tZQinit = 1024 nCK covers the init ZQ)
  val tREFImc = mcOfNs((if (sdram.sdramtime.REF_NS != 0) sdram.sdramtime.REF_NS
                        else sdram.sdramtime.REF * 1000.0) / sdram.sdramtime.REF_RATE)
  val tRASmaxmc = 9 * tREFImc // JESD79-4B: tRAS(max) = 9 x tREFI

  val summary = f"tRAS=$tRASmc tRP=$tRPmc tRCD=$tRCDmc tRFC=$tRFCmc tFAW=$tFAWmc tRRD=$tRRDmc/$tRRD_Smc " +
    f"tWTR=$tWTRmc/$tWTR_Smc tRTP=$tRTPmc tCCD=$tCCD_Lmc/$tCCD_Smc tREFI=$tREFImc tMRD=$tMRDmc MC cycles " +
    f"(DRAM $tDramNs%.2f ns, MC $tMcNs%.2f ns, fr=$fr)"

  private val ranks = cfg.chipSelectNumber
  private val banks = sdram.bankCount
  private def idx(rank: Int, bank: Int): Int = rank * banks + (bank % banks)

  private val lastAct = Array.fill(ranks * banks)(-1L)
  private val lastPre = Array.fill(ranks * banks)(-1L)
  private val lastRead = Array.fill(ranks * banks)(-1L)
  private val lastWrite = Array.fill(ranks * banks)(-1L)
  private val lastRef = Array.fill(ranks)(-1L)
  private val lastActAny = Array.fill(ranks)(-1L)
  private val lastActBg = Array.fill(ranks)(-1)
  private val lastCas = Array.fill(ranks)(-1L)
  private val lastCasBg = Array.fill(ranks)(-1)
  private val lastMrs = Array.fill(ranks)(-1L)
  private val lastZq = Array.fill(ranks)(-1L)
  private val actWindow = Array.fill(ranks)(mutable.Queue.empty[Long])
  private val failures = mutable.ArrayBuffer.empty[String]

  def violations: Seq[String] = failures.toSeq
  def clean: Boolean = failures.isEmpty

  private def fail(cycle: Long, what: String, got: Long, need: String): Unit = {
    val msg = s"JESD79-4B timing violation at MC cycle $cycle: $what observed $got, requires $need"
    failures += msg
    if (verbose) println(s"[TIMING] $msg")
  }

  /**
    * Observe one decoded command of one rank. `bank` is the flat {BG, BA} index of the command's
    * bank (PREA/ref all-bank variants pass the bank field the DRAM ignores; PREA/ref close the
    * per-bank state for every bank of that rank).
    */
  def observe(rank: Int, cycle: Long, act: Boolean, pre: Boolean, preAll: Boolean, wr: Boolean,
              rd: Boolean, ref: Boolean, mrs: Boolean, zq: Boolean, bank: Int, bg: Int): Unit = {
    if (rank >= ranks) return
    val b = bank % banks

    if (act) {
      if (lastActAny(rank) >= 0) {
        val need = if (lastActBg(rank) == bg) tRRDmc else tRRD_Smc
        val gap = cycle - lastActAny(rank)
        if (gap < need) fail(cycle, s"tRRD (ACT->ACT, same bank group = ${lastActBg(rank) == bg})", gap, s">= $need MC cycles")
      }
      val w = actWindow(rank)
      w.enqueue(cycle)
      while (w.length > 4) w.dequeue()
      if (w.length == 4 && (w.last - w.head) < tFAWmc)
        fail(cycle, "tFAW (four ACTs inside one window)", w.last - w.head, s">= $tFAWmc MC cycles")
      if (lastRef(rank) >= 0 && cycle - lastRef(rank) < tRFCmc)
        fail(cycle, "tRFC (REF->ACT)", cycle - lastRef(rank), s">= $tRFCmc MC cycles")
      if (lastAct(idx(rank, b)) >= 0 && cycle - lastAct(idx(rank, b)) < tRASmc)
        fail(cycle, s"tRAS min (ACT->ACT bank $b, bank never precharged)", cycle - lastAct(idx(rank, b)), s">= $tRASmc MC cycles")
      if (lastPre(idx(rank, b)) >= 0 && cycle - lastPre(idx(rank, b)) < tRPmc)
        fail(cycle, s"tRP (PRE->ACT bank $b)", cycle - lastPre(idx(rank, b)), s">= $tRPmc MC cycles")
      if (lastMrs(rank) >= 0 && cycle - lastMrs(rank) < tMRDmc)
        fail(cycle, "tMRD (MRS->ACT)", cycle - lastMrs(rank), s">= $tMRDmc MC cycles")
      if (lastZq(rank) >= 0 && cycle - lastZq(rank) < tZQCLmc)
        fail(cycle, "tZQoper (ZQ->command)", cycle - lastZq(rank), s">= $tZQCLmc MC cycles")
      lastAct(idx(rank, b)) = cycle
      lastActAny(rank) = cycle
      lastActBg(rank) = bg
    }

    if (pre || preAll) {
      for (x <- (if (preAll) 0 until banks else Seq(b))) {
        val i = idx(rank, x)
        // A PRE only constrains the row it actually closes. On a PRE_ALL sweep the banks
        // whose rows were already closed (lastPre >= lastAct) must not be timed: timing
        // them from their stale lastAct produces false tRAS max hits long after the row
        // was properly closed (mirrors the open-row predicate used in the REF check below).
        if (lastAct(i) >= 0 && lastPre(i) < lastAct(i)) {
          val held = cycle - lastAct(i)
          if (held < tRASmc) fail(cycle, s"tRAS min (ACT->PRE bank $x)", held, s">= $tRASmc MC cycles")
          if (held > tRASmaxmc) fail(cycle, s"tRAS max (ACT->PRE bank $x)", held, s"<= $tRASmaxmc MC cycles (9 x tREFI)")
          if (lastRead(i) >= 0 && cycle - lastRead(i) < tRTPmc)
            fail(cycle, s"tRTP (RD->PRE bank $x)", cycle - lastRead(i), s">= $tRTPmc MC cycles")
        }
        lastPre(i) = cycle
      }
      if (lastMrs(rank) >= 0 && cycle - lastMrs(rank) < tMODmc)
        fail(cycle, "tMOD (MRS->PRE)", cycle - lastMrs(rank), s">= $tMODmc MC cycles")
    }

    if (wr || rd) {
      val i = idx(rank, b)
      if (lastAct(i) < 0 || cycle - lastAct(i) < tRCDmc)
        fail(cycle, s"tRCD (ACT->${if (rd) "RD" else "WR"} bank $b)", (cycle - lastAct(i)).max(0), s">= $tRCDmc MC cycles")
      if (lastCas(rank) >= 0) {
        val gap = cycle - lastCas(rank)
        val need = if (lastCasBg(rank) == bg) tCCD_Lmc else tCCD_Smc
        if (gap < need)
          fail(cycle, s"tCCD_${if (lastCasBg(rank) == bg) "L" else "S"} (CAS->CAS bank $b, same group = ${lastCasBg(rank) == bg})",
            gap, s">= $need MC cycles")
      }
      if (wr && lastRead(i) >= 0 && cycle - lastRead(i) < tRTWmc)
        fail(cycle, s"tRTW (RD->WR bank $b)", cycle - lastRead(i), s">= $tRTWmc MC cycles")
      if (rd && lastWrite(i) >= 0) {
        val gap = cycle - lastWrite(i)
        val need = if (lastCasBg(rank) == bg) tWTRmc else tWTR_Smc
        if (gap < need)
          fail(cycle, s"tWTR (WR->RD bank $b, same group = ${lastCasBg(rank) == bg})", gap, s">= $need MC cycles")
      }
      if (rd) lastRead(i) = cycle else lastWrite(i) = cycle
      lastCas(rank) = cycle
      lastCasBg(rank) = bg
      if (lastMrs(rank) >= 0 && cycle - lastMrs(rank) < tMRDmc)
        fail(cycle, "tMRD (MRS->column command)", cycle - lastMrs(rank), s">= $tMRDmc MC cycles")
    }

    if (ref) {
      if (lastRef(rank) >= 0) {
        val gap = cycle - lastRef(rank)
        // tREFI is a window, not a period: a refresh must be issued INSIDE every tREFI, so an
        // observed gap larger than tREFI (plus one MC cycle of sampling slack) is a
        // data-retention violation regardless of how the scheduler counts.
        if (gap > tREFImc + 1)
          fail(cycle, "tREFI (REF->REF window)", gap, s"<= ${tREFImc + 1} MC cycles (tREFI = $tREFImc)")
      }
      val anyOpen = (0 until banks).exists(x => lastAct(idx(rank, x)) >= 0 && lastPre(idx(rank, x)) < lastAct(idx(rank, x)))
      if (anyOpen && lastActAny(rank) >= 0 && cycle - lastActAny(rank) < tRASmc)
        fail(cycle, "tRAS (ACT open at REF)", cycle - lastActAny(rank), s">= $tRASmc MC cycles")
      lastRef(rank) = cycle
      for (x <- 0 until banks) if (lastAct(idx(rank, x)) >= 0 && lastPre(idx(rank, x)) < lastAct(idx(rank, x)))
        lastPre(idx(rank, x)) = cycle
    }

    if (mrs) {
      if (lastMrs(rank) >= 0 && cycle - lastMrs(rank) < tMRDmc)
        fail(cycle, "tMRD (MRS->MRS)", cycle - lastMrs(rank), s">= $tMRDmc MC cycles")
      lastMrs(rank) = cycle
    }
    if (zq) {
      if (lastZq(rank) >= 0 && cycle - lastZq(rank) < tZQCLmc)
        fail(cycle, "tZQoper (ZQ->ZQ)", cycle - lastZq(rank), s">= $tZQCLmc MC cycles")
      lastZq(rank) = cycle
    }
  }
}
