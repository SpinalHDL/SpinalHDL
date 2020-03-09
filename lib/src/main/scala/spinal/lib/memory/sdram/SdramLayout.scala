package spinal.lib.memory.sdram

import spinal.core._


class SdramGeneration( val RESETn : Boolean,
                       val ODT : Boolean,
                       val DQS : Boolean,
                       val FAW : Boolean,
                       val CCD : Int,
                       val burstLength : Int,
                       val dataRate : Int)

object SdramGeneration{
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
}


case class SdramLayout( generation : SdramGeneration,
                        bankWidth : Int,
                        columnWidth : Int,
                        rowWidth : Int,
                        dataWidth : Int){
  def bytePerWord = dataWidth/8
  def wordAddressWidth = bankWidth + columnWidth + rowWidth
  def byteAddressWidth = bankWidth + columnWidth + rowWidth + log2Up(bytePerWord)
  def chipAddressWidth = Math.max(columnWidth,rowWidth)
  def bankCount = 1 << bankWidth
  def capacity = BigInt(1) << byteAddressWidth
  def columnSize = 1 << columnWidth
  def rowSize = 1 << rowWidth
}
