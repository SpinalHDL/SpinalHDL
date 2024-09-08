package spinal.lib.memory.sdram.xdr.Dfi.Tools

import spinal.lib.memory.sdram.xdr.Dfi.Interface.{DfiConfig, DfiTimeConfig}
import spinal.core._
import spinal.lib.{OHMasking, OHToUInt}

case class DelayCyc(config: DfiConfig, timeConfig: DfiTimeConfig){

//  def findsp(vailds: Vec[Bool]): Int = if(vailds.indexOf(True) == -1) 0 else vailds.indexOf(True)
//  def sp2np(startphase:Int,delaycyc:Int):Int = (startphase + delaycyc) % config.frequencyRatio
//  def mcdelaycyc(startphase: Int, phydelaycyc: Int): Int = (startphase + phydelaycyc) / config.frequencyRatio
  def findsp(bools: Seq[Bool]): UInt = OHToUInt(OHMasking.first(B(bools)).asBools)
  def findlp(bools: Seq[Bool]): UInt = OHToUInt(OHMasking.last(B(bools)).asBools)
  def sp2np(startphase:UInt,delaycyc:Int):UInt = (startphase + delaycyc) % config.frequencyRatio
  def sp2np(startphase:Int,delaycyc:Int):Int = (startphase + delaycyc) % config.frequencyRatio
//  def mcdelaycyc(startphase: UInt, phydelaycyc: Int): UInt =(startphase + phydelaycyc) / config.frequencyRatio
  def mcdelaycyc(startphase: UInt, phydelaycyc: Int): UInt =(phydelaycyc) / config.frequencyRatio
  def mcdelaycyc(startphase: Int, phydelaycyc: Int): Int =(phydelaycyc) / config.frequencyRatio
}
