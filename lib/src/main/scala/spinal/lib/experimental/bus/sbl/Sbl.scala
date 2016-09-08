package spinal.lib.experimental.bus.sbl

import spinal.core._

// SBL bus is a simple memory bus definition definition
// master Stream(SblCmd(cfg))
// master Stream(SblWriteCmd(cfg))    for write only
// master Stream(SblReadCmd(cfg))     for read only
// slave Flow(SblReadRet(cfg))

case class SblConfig(addressWidth : Int,dataWidth : Int)

case class SblCmd(config: SblConfig) extends Bundle{
  val isWrite = Bool
  def isRead = !isWrite
  val address = UInt(config.addressWidth bit)
  val data = Bits(config.dataWidth bit)
}
case class SblWriteCmd(config: SblConfig) extends Bundle{
  val address = UInt(config.addressWidth bit)
  val data = Bits(config.dataWidth bit)
}
case class SblReadCmd(config: SblConfig) extends Bundle{
  val address = UInt(config.addressWidth bit)
}
case class SblReadRet(config: SblConfig) extends Bundle{
  val data = Bits(config.dataWidth bit)
}
