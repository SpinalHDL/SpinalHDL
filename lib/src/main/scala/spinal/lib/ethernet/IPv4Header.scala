package spinal.lib.ethernet

import spinal.core._
import EthernetProtocolConstant._

import scala.collection.mutable

object IPv4Header {
  def apply(array: Array[Bits]): Array[Bits] = {
    val gen = new IPv4Header
    require(
      array.length == gen.header.length,
      s"Initializing parameters not enough! Require ${gen.header.length} but gave ${array.length}"
    )
    gen.header.zipWithIndex.foreach { case (data, idx) =>
      data := array(idx)
    }
    gen.header
  }

  def unapply(header: Bits): (Array[String], Array[Bits]) = {
    val gen = new IPv4Header
    val bitWidth = gen.frameFieldInit.values.sum
    require(
      bitWidth == header.getWidth,
      s"Initializing parameters not enough! Require ${bitWidth} but gave ${header.getWidth}"
    )
    val asLittleEnd = header.subdivideIn(bitWidth / 8 slices).reduce(_ ## _)
    val tmp = asLittleEnd
      .sliceBy(gen.frameFieldInit.values.toList.reverse)
      .reverse
      .toArray
    gen.header.zipWithIndex.foreach { case (data, idx) =>
      data := tmp(idx)
    }
    (gen.frameFieldInit.keys.toArray, gen.header)
  }
}

case class IPv4Header() extends Bundle with FrameHeader {
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
  val header: Array[Bits] =
    EthernetProtocolHeaderConstructor(frameFieldInit).constructHeader()
}