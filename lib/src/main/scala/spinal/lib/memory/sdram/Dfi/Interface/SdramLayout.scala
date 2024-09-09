package spinal.lib.memory.sdram.Dfi.Interface

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
  val MYDDR = new SdramGeneration(
    RESETn = true,
    ODT = false,
    DQS = true,
    FAW = false,
    CCD = 4,
    burstLength = 8,
    dataRate = 2
  )
}


case class SdramLayout( generation : SdramGeneration,
                        bankWidth : Int,
                        columnWidth : Int,
                        rowWidth : Int,
                        dataWidth : Int,
                        ddrMHZ:Int=100,
                        ddrWrLat:Int=4,
                        ddrRdLat:Int=4,
                        sdramtime:SdramTiming
                        ){
  import sdramtime._
  def bytePerWord = dataWidth/8
  def wordAddressWidth = bankWidth + columnWidth + rowWidth
  def byteAddressWidth = bankWidth + columnWidth + rowWidth + log2Up(bytePerWord)
  def chipAddressWidth = Math.max(columnWidth,rowWidth)
  def bankCount = 1 << bankWidth
  def capacity = BigInt(1) << byteAddressWidth
  def columnSize = 1 << columnWidth
  def rowSize = 1 << rowWidth
  def ddrStartdelay = 600000 / (1000 / ddrMHZ) // 600uS

  def timeCycle(time:Int,cycTime:Int) = (time + cycTime - 1) / cycTime
  def cycleTime_ns = 1000 / ddrMHZ
  def tREF = (REF * ddrMHZ) / (1<<rowWidth)
  def tRCD = timeCycle(RCD,cycleTime_ns)
  def tRP = timeCycle(RP,cycleTime_ns)
  def tRFC = timeCycle(RFC,cycleTime_ns)
  def tWR = 5 + 1
  def tWTR = math.max( timeCycle(WTR,cycleTime_ns),ddrWrLat + generation.burstLength/generation.dataRate + tWR)
  def tRTW = ddrRdLat + generation.burstLength/generation.dataRate + tWR
  def tRAS = timeCycle(RAS,cycleTime_ns)
  def tRTP = math.max(timeCycle(RTP,cycleTime_ns),generation.burstLength/generation.dataRate)
  def tRRD = math.max(timeCycle(RRD,cycleTime_ns),generation.burstLength/generation.dataRate)
  def tFAW = timeCycle(FAW,cycleTime_ns)
//  def tRAS = timeCycle(RAS,cycleTime_ns)
//  def tRAS = timeCycle(RAS,cycleTime_ns)

  def tPhyWrlat = ddrWrLat - 1
  def tRddataEn = ddrRdLat - 1
}

case class SdramTiming(generation : Int,
                       RFC : Int=260,//ns // Command Period (REF to ACT)
                       RAS : Int=38, // Command Period (ACT to PRE)   Per bank
                       RP  : Int=15,//ns // Command Period (PRE to ACT)
                       RCD : Int=15,//ns // Active Command To Read / Write Command Delay Time
                       WTR : Int=8, // WRITE to READ
                       WTP : Int=0, // WRITE to PRE (WRITE recovery time)
                       RTP : Int=8, // READ to PRE
                       RRD : Int=6, // ACT to ACT cross bank
                       REF : Int=64000,//us // Refresh Cycle Time (single row)
                       FAW : Int=35) // Four ACTIVATE windows