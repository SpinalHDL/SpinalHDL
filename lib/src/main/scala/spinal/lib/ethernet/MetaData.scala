package spinal.lib.ethernet

import EthernetProtocolConstant._
import spinal.core._
import spinal.lib._

object MetaDataConstant {
  val PACKET_TOTAL_MAX = 4096
  val PACKET_TOTAL_LEN_WIDTH = log2Up(PACKET_TOTAL_MAX)   // 0 ~ 4096
}

import MetaDataConstant._

case class MetaData() extends Bundle {
  val dataLen = UInt(PACKET_TOTAL_LEN_WIDTH bits)
  val dstMacAddr = Bits(MAC_ADDR_WIDTH bits)
  val dstIpAddr = Bits(IP_ADDR_WIDTH bits)
  val srcMacAddr = Bits(MAC_ADDR_WIDTH bits)
  val srcIpAddr = Bits(IP_ADDR_WIDTH bits)
  val dstPort = Bits(UDP_PORT_WIDTH bits)
  val srcPort = Bits(UDP_PORT_WIDTH bits)

}
