package spinal.lib.ethernet

import UserConfiguration._
import spinal.core._

import scala.collection.mutable

object EthernetProtocolConstant {

  val ETH_TYPE = 0x0800
  val IP_VERSION = 0x4
  val IHL = 0x5
  val DSCP = 0x0
  val ECN = 0x0
  val FLAGS = 0x2
  val FRAGMENT_OFFSET = 0x0
  val TTL = 0x40
  val PROTOCOL = 0x11


  val ETH_HEADER_LENGTH = 14
  val IP_HEADER_LENGTH = 20
  val UDP_HEADER_LENGTH = 8
  val ETH_HEADER_WIDTH = ETH_HEADER_LENGTH * BYTE_WIDTH
  val IP_HEADER_WIDTH = IP_HEADER_LENGTH * BYTE_WIDTH
  val UDP_HEADER_WIDTH = UDP_HEADER_LENGTH * BYTE_WIDTH
  val HEADER_TOTAL_LENGTH =
    ETH_HEADER_LENGTH + IP_HEADER_LENGTH + UDP_HEADER_LENGTH

  val MTU = 1500
  val IP_LENGTH_MAX = MTU
  val UDP_LENGTH_MAX = IP_LENGTH_MAX - IP_HEADER_LENGTH

  val MAX_DATA_NUM = MTU - IP_HEADER_LENGTH - UDP_HEADER_LENGTH
  val MIN_DATA_NUM = 22 // MIN_TRANSACTION_NUM(64) - MAC_LEN(14) - IP_LEN(20) - UDP_LEN(8)

  val MAC_ADDR_WIDTH = 48
  val ETH_TYPE_WIDTH = 16

  val IP_VERSION_WIDTH = 4
  val IHL_WIDTH = 4
  val DSCP_WIDTH = 6
  val ECN_WIDTH = 2
  val IP_LENGTH_WIDTH = 16
  val IDENTIFICATION_WIDTH = 16
  val FLAGS_WIDTH = 3
  val FRAGMENT_OFFSET_WIDTH = 13
  val TTL_WIDTH = 8
  val PROTOCOL_WIDTH = 8
  val IP_HEADER_CHECKSUM_WIDTH = 16
  val IP_ADDR_WIDTH = 32

  val UDP_PORT_WIDTH = 16
  val UDP_LENGTH_WIDTH = 16
  val UDP_CHECKSUM_WIDTH = 16
}
class EthernetProtocolHeaderConstructor {
  def constructHeader(initField: mutable.LinkedHashMap[String, Int]): Array[Bits] = {
    val fieldName: Array[String] = initField.keys.toArray
    val fieldWidth: Array[Int] = initField.values.toArray
    val protocolField = List.tabulate[Bits](initField.size) { index =>
      val tmp: Bits =
        Bits(fieldWidth(index) bits) setName fieldName(index)
      tmp
    }
    protocolField.toArray
  }

}
trait FrameHeader extends EthernetProtocolHeaderConstructor {
  val frameFieldInit: mutable.LinkedHashMap[String, Int]
  val header: Array[Bits]
}

class HeaderOperate {
  def generate(arr: Seq[Bits], gen: FrameHeader): Seq[Bits] = {
    require(
      arr.length == gen.header.length,
      s"Initializing parameters not enough! Require ${gen.header.length} but gave ${arr.length}"
    )
    gen.header.zipWithIndex.foreach { case (data, idx) =>
      data := arr(idx)
    }
    gen.header
  }
  def extract(header: Bits, extract: FrameHeader): (Seq[String], Seq[Bits]) = {
    val bitWidth = extract.frameFieldInit.values.sum
    require(
      bitWidth == header.getWidth,
      s"Initializing parameters not enough! Require ${bitWidth} but gave ${header.getWidth}"
    )
    val asLittleEnd = header.subdivideIn(bitWidth / 8 slices).reduce(_ ## _)
    val tmp = asLittleEnd
      .sliceBy(extract.frameFieldInit.values.toList.reverse)
      .reverse
      .toArray
    extract.header.zipWithIndex.foreach { case (data, idx) =>
      data := tmp(idx)
    }
    (extract.frameFieldInit.keys.toSeq, extract.header)
  }
}
