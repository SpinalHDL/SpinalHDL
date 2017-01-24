package spinal.lib.memory.sdram

import spinal.core._
import spinal.lib._
import spinal.lib.io.TriState

case class SdramLayout( bankWidth : Int,
                        columnWidth : Int,
                        rowWidth : Int,
                        dataWidth : Int){
  def bytePerWord = dataWidth/8
  def wordAddressWidth = bankWidth + columnWidth + rowWidth
  def byteAddressWidth = bankWidth + columnWidth + rowWidth + log2Up(bytePerWord)
  def chipAddressWidth = Math.max(columnWidth,rowWidth)
  def bankCount = 1 << bankWidth
  def capacity = BigInt(1) << byteAddressWidth
}

case class SdramTimings(
  bootRefreshCount : Int, // Number of refresh command done in the boot sequence
  tPOW  : TimeNumber,     // Powerup time
  tREF  : TimeNumber,     // Refresh Cycle Time (that cover all row)
  tRC   : TimeNumber,     // Command Period (ACT to ACT)   Per bank
  tRFC  : TimeNumber,     // Command Period (REF to REF)   Per bank
  tRAS  : TimeNumber,     // Command Period (ACT to PRE)                Per bank
  tRP   : TimeNumber,     // Command Period (PRE to ACT)
  tRCD  : TimeNumber,     // Active Command To Read / Write Command Delay Time
  cMRD  : Int,            // Mode Register Program Time
  tWR   : TimeNumber,     // WRITE recovery time
  cWR   : Int             // WRITE recovery cycle
)

case class SdramInterface(g : SdramLayout) extends Bundle with IMasterSlave{
  val ADDR  = Bits(g.chipAddressWidth bits)
  val BA    = Bits(g.bankWidth bits)
  val DQ    = TriState(Bits(g.dataWidth bits))
  val DQM   = Bits(g.bytePerWord bits)
  val CASn  = Bool
  val CKE   = Bool
  val CSn   = Bool
  val RASn  = Bool
  val WEn   = Bool

  override def asMaster(): Unit = {
    out(ADDR,BA,CASn,CKE,CSn,DQM,RASn,WEn)
    master(DQ)
  }
}
