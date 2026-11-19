package spinal.lib.memory.sdram.dfi

import spinal.core._

class SdramGeneration(
    val RESETn: Boolean,
    val ODT: Boolean,
    val DQS: Boolean,
    val FAW: Boolean,
    val CCD: Int,
    val burstLength: Int,
    val dataRate: Int,
    // Bank-group aware CAS-to-CAS (tCCD) in nCK, JESD79-4B Table 34.
    // 0 => collapse to CCD (no bank groups): DDR/DDR2/DDR3 keep exactly the old single-value behavior.
    val ccdL: Int = 0,
    val ccdS: Int = 0
) {
  def tCCD_L = if (ccdL != 0) ccdL else CCD
  def tCCD_S = if (ccdS != 0) ccdS else CCD
}

object SdramGeneration {
  val SDR = new SdramGeneration(
    RESETn = false,
    ODT = false,
    DQS = false,
    FAW = false,
    CCD = 1,
    burstLength = 1,
    dataRate = 1
  )
  val DDR2 = new SdramGeneration(
    RESETn = false,
    ODT = true,
    DQS = true,
    FAW = true,
    CCD = 2,
    burstLength = 4,
    dataRate = 2
  )
  val DDR3 = new SdramGeneration(
    RESETn = true,
    ODT = true,
    DQS = true,
    FAW = true,
    CCD = 4,
    burstLength = 8,
    dataRate = 2
  )
  val MYDDR = new SdramGeneration(
    RESETn = true,
    ODT = true,
    DQS = true,
    FAW = true,
    CCD = 4,
    burstLength = 8,
    dataRate = 2
  )
  // DDR4: fixed BL8, 2nCK data rate, bank-group aware timing (tCCD_L=4/tCCD_S=2 nCK,
  // JESD79-4B Table 34). tRRD/tWTR L/S variants live in SdramTiming (ns-based, like all
  // other cross-bank timings in this codebase).
  val DDR4 = new SdramGeneration(
    RESETn = true,
    ODT = true,
    DQS = true,
    FAW = true,
    CCD = 4,
    burstLength = 8,
    dataRate = 2,
    ccdL = 4,
    ccdS = 2
  )
}

case class SdramConfig(
    generation: SdramGeneration,
    bgWidth: Int,
    cidWidth: Int,
    bankWidth: Int,
    columnWidth: Int,
    rowWidth: Int,
    dataWidth: Int,
    ddrMHZ: Int,
    ddrWrLat: Int,
    ddrRdLat: Int,
    sdramtime: SdramTiming
) {

  def burstLength = generation.burstLength
  def wordAddressWidth = bankWidth + columnWidth + rowWidth
  def chipAddressWidth = Math.max(columnWidth, rowWidth)
  def bankCount = 1 << bankWidth
  def capacity = BigInt(1) << byteAddressWidth
  def byteAddressWidth = bankWidth + columnWidth + rowWidth + log2Up(bytePerWord)
  def bytePerWord = dataWidth / 8
  def columnSize = 1 << columnWidth
  def ddrStartDelay = 600000 / (1000 / ddrMHZ) // 600uS

  def tREF =
    if (sdramtime.REF_NS != 0) timeCycle(sdramtime.REF_NS, cycleTime_ns)
    else (sdramtime.REF * ddrMHZ) / rowSize
  def rowSize = 1 << rowWidth
  def tRCD = timeCycle(sdramtime.RCD, cycleTime_ns)
  def tRP = timeCycle(sdramtime.RP, cycleTime_ns)
  def tRFC = timeCycle(sdramtime.RFC, cycleTime_ns)
  // All timing values below are in MC clock cycles:
  // - timeCycle() converts ns -> MC cycles (cycleTime_ns = 1000/ddrMHZ ns)
  // - ddrWrLat/ddrRdLat/tWR are already expressed in MC clock cycles
  def tWTR =
    math.max(timeCycle(sdramtime.WTR, cycleTime_ns), ddrWrLat + generation.burstLength / generation.dataRate + tWR)

  /** tWTR_S: WRITE to READ across different bank groups (DDR4 only).
    * Falls back to tWTR when WTR_S is unset. Floor is the write burst itself, NOT tWTR:
    * the S variant must be allowed to be shorter than the same-group (L) variant.
    */
  def tWTR_S =
    if (sdramtime.WTR_S == 0) tWTR
    else math.max(timeCycle(sdramtime.WTR_S, cycleTime_ns), generation.burstLength / generation.dataRate)

  def tRTW = ddrRdLat + generation.burstLength / generation.dataRate + tWR

  /** Write recovery time (tWR) in MC clock cycles.
    * Hardcoded approximation inherited from the legacy xdr controller (5ns + 1 cycle at 200MHz).
    */
  def tWR = 5 + 1

  def tRAS = timeCycle(sdramtime.RAS, cycleTime_ns)

  def tRTP = math.max(timeCycle(sdramtime.RTP, cycleTime_ns), generation.burstLength / generation.dataRate)

  def tRRD = math.max(timeCycle(sdramtime.RRD, cycleTime_ns), generation.burstLength / generation.dataRate)

  /** tRRD_S: ACT to ACT across different bank groups (DDR4 only).
    * Falls back to tRRD when RRD_S is unset; floor is the burst, not tRRD (S may be shorter).
    */
  def tRRD_S =
    if (sdramtime.RRD_S == 0) tRRD
    else math.max(timeCycle(sdramtime.RRD_S, cycleTime_ns), generation.burstLength / generation.dataRate)

  def tFAW = timeCycle(sdramtime.FAW, cycleTime_ns)

  def timeCycle(time: Int, cycTime: Int) = (time + cycTime - 1) / cycTime

  def cycleTime_ns = 1000 / ddrMHZ

  def tPhyWrlat = ddrWrLat - 2
  def tRddataEn = ddrRdLat - 2
}

case class SdramTiming(
    generation: Int,
    RFC: Int, // ns // Command Period (REF to ACT)
    RAS: Int, // ns// Command Period (ACT to PRE)   Per bank
    RP: Int, // ns // Command Period (PRE to ACT)
    RCD: Int, // ns // Active Command To Read / Write Command Delay Time
    WTR: Int, // ns// WRITE to READ
    WTP: Int, // ns// WRITE to PRE (WRITE recovery time)
    RTP: Int, // ns// READ to PRE
    RRD: Int, // ns// ACT to ACT cross bank
    REF: Int, // us // Refresh Cycle Time (single row)
    FAW: Int,
    // DDR4 bank-group variants (ns). 0 => fall back to the shared value, so pre-DDR4
    // SdramTiming literals keep compiling and behave exactly as before.
    WTR_S: Int = 0, // tWTR_S: WRITE to READ, cross bank group
    RRD_S: Int = 0, // tRRD_S: ACT to ACT, cross bank group
    // Absolute refresh interval tREFI in ns (DDR4: 7800). 0 => legacy per-row
    // derivation (REF us / rows), preserving pre-DDR4 behavior bit-for-bit.
    REF_NS: Int = 0
) //ns // Four ACTIVATE windows
