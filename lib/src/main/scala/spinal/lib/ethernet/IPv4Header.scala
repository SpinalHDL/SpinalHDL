package spinal.lib.ethernet

import EthernetProtocolConstant._
import spinal.core._

import scala.collection.mutable

object IPv4Header extends HeaderOperate {
  def apply(array: Seq[Bits]): Seq[Bits] = {
    val gen = IPv4Header()
    this.generate(array, gen)
  }
  def unapply(header: Bits): (Seq[String], Seq[Bits]) = {
    val extract = IPv4Header()
    this.extract(header, extract)
  }
}

case class IPv4Header() extends FrameHeader {
  val frameFieldInit = mutable.LinkedHashMap(
    "protocolVersion" -> IP_VERSION_WIDTH,
    "internetHeaderLength" -> IHL_WIDTH,
    "differentiatedServicesCodePoint" -> DSCP_WIDTH,
    "explicitCongestionNotification" -> ECN_WIDTH,
    "ipLen" -> IP_LENGTH_WIDTH,
    "identification" -> IDENTIFICATION_WIDTH,
    "flags" -> FLAGS_WIDTH,
    "fragmentOffset" -> FRAGMENT_OFFSET_WIDTH,
    "ttl" -> TTL_WIDTH,
    "protocol" -> PROTOCOL_WIDTH,
    "ipChecksum" -> IP_HEADER_CHECKSUM_WIDTH,
    "srcAddr" -> IP_ADDR_WIDTH,
    "dstAddr" -> IP_ADDR_WIDTH
  )
  val header: Array[Bits] = this.constructHeader(frameFieldInit)
}