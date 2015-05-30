package spinal.lib.bus.Hbl

import spinal.core._
import spinal.lib._

case class HblConfig(addressWidth : Int,dataWidth : Int)

case class HblReadCmd(config: HblConfig) extends Bundle{
  val address = UInt(config.addressWidth bit)
}
case class HblReadRet(config: HblConfig) extends Bundle{
  val data = Bits(config.dataWidth bit)
}
case class HblWriteCmd(config: HblConfig) extends Bundle{
  val address = UInt(config.addressWidth bit)
  val data = Bits(config.dataWidth bit)
}