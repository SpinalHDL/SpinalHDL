package spinal.lib.memory.sdram

import spinal.core._
import spinal.lib._
import spinal.lib.io.TriState

case class SdramLayout( bankWidth : Int,
                        columnWidth : Int,
                        rowWidth : Int,
                        dataWidth : Int){
  def symbolCount = dataWidth/8
  def totalAddressWidth = bankWidth + columnWidth + rowWidth
  def addressWidth = Math.max(columnWidth,rowWidth)
  def bankCount = 1 << bankWidth
}

case class SdramTimings(
  bootRefreshCount   : Int, // Number of refresh command done in the boot sequence
  tPOW  : BigDecimal, // Powerup time
  tREF  : BigDecimal, // Refresh Cycle Time (that cover all row)
  tRC   : BigDecimal, // Command Period (REF to REF / ACT to ACT)   Per bank
  tRAS  : BigDecimal, // Command Period (ACT to PRE)                Per bank
  tRP   : BigDecimal, // Command Period (PRE to ACT)
  tRCD  : BigDecimal, // Active Command To Read / Write Command Delay Time
  tMRD  : BigDecimal  // Mode Register Program Time
)

case class SdramInterface(g : SdramLayout) extends Bundle with IMasterSlave{
  val ADDR  = Bits(g.addressWidth bits)
  val BA    = Bits(g.bankWidth bits)
  val DQ    = TriState(Bits(g.dataWidth bits))
  val DQM   = Bits(g.symbolCount bits)
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