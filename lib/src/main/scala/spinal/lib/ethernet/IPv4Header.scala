package spinal.lib.ethernet

import EthernetProtocolConstant._
import spinal.core._

import scala.collection.mutable

object IPv4Header extends HeaderOperate {
  def apply(array: Seq[Bits]): Seq[Bits] = {
    val gen = IPv4Header()
    this.generate(array, gen)
  }
  def unapply(header: Bits): (Seq[EthernetProtocolField], Seq[Bits]) = {
    val extract = IPv4Header()
    this.extract(header, extract)
  }
}
object IPv4Fields {
  val protocolVersion = new EthernetProtocolField
  val internetHeaderLength = new EthernetProtocolField
  val differentiatedServicesCodePoint = new EthernetProtocolField
  val explicitCongestionNotification = new EthernetProtocolField
  val ipLen = new EthernetProtocolField
  val identification = new EthernetProtocolField
  val flags = new EthernetProtocolField
  val fragmentOffset = new EthernetProtocolField
  val ttl = new EthernetProtocolField
  val protocol = new EthernetProtocolField
  val ipChecksum = new EthernetProtocolField
  val srcAddr = new EthernetProtocolField
  val dstAddr = new EthernetProtocolField
}
case class IPv4Header() extends FrameHeader {
  val frameFieldInit = mutable.LinkedHashMap(
    IPv4Fields.protocolVersion -> IP_VERSION_WIDTH,
    IPv4Fields.internetHeaderLength -> IHL_WIDTH,
    IPv4Fields.differentiatedServicesCodePoint -> DSCP_WIDTH,
    IPv4Fields.explicitCongestionNotification -> ECN_WIDTH,
    IPv4Fields.ipLen -> IP_LENGTH_WIDTH,
    IPv4Fields.identification -> IDENTIFICATION_WIDTH,
    IPv4Fields.flags -> FLAGS_WIDTH,
    IPv4Fields.fragmentOffset -> FRAGMENT_OFFSET_WIDTH,
    IPv4Fields.ttl -> TTL_WIDTH,
    IPv4Fields.protocol -> PROTOCOL_WIDTH,
    IPv4Fields.ipChecksum -> IP_HEADER_CHECKSUM_WIDTH,
    IPv4Fields.srcAddr -> IP_ADDR_WIDTH,
    IPv4Fields.dstAddr -> IP_ADDR_WIDTH
  )
  val header: Array[Bits] = this.constructHeader(frameFieldInit)
}