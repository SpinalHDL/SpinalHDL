package spinal.lib.ethernet

import spinal.core._
import spinal.lib._

import EthernetProtocolConstant._

object MetaInterfaceConstrant {
  val PACKET_TOTAL_MAX = 4096
  val PACKET_TOTAL_LEN_WIDTH = log2Up(PACKET_TOTAL_MAX)   // 0 ~ 4096
}

import MetaInterfaceConstrant._

case class MetaInterface()
    extends Bundle
    with IMasterSlave {
  val dataLen = UInt(PACKET_TOTAL_LEN_WIDTH bits)
  val MacAddr = Bits(MAC_ADDR_WIDTH bits)
  val IpAddr = Bits(IP_ADDR_WIDTH bits)
  val dstPort = Bits(UDP_PORT_WIDTH bits)
  val srcPort = Bits(UDP_PORT_WIDTH bits)
  override def asMaster(): Unit = {
    out(dataLen, MacAddr, IpAddr, dstPort, srcPort)
  }
}
