package spinal.lib.memory.sdram

import spinal.core._


class SdramGeneration( val RESETn : Boolean,
                       val ODT : Boolean,
                       val DQS : Boolean,
                       val FAW : Boolean)

object SdramGeneration{
  val SDR = new SdramGeneration(
    RESETn = false,
    ODT = false,
    DQS = false,
    FAW = false
  )
  val DDR3 = new SdramGeneration(
    RESETn = true,
    ODT = true,
    DQS = true,
    FAW = true
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
