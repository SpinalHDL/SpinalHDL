package spinal.lib.ethernet

import spinal.core._
import spinal.core.internals.Operator
import spinal.core.sim._
import spinal.lib
import spinal.lib._
import spinal.lib.bus.amba4.axis.{Axi4Stream, Axi4StreamConfig}
import spinal.lib.fsm._

import scala.collection.mutable
import scala.math.pow

object EthernetProtocolConstant {
  val DATA_WIDTH = 256
  val BYTE_WIDTH = 8
  val DATA_BYTE_CNT = DATA_WIDTH / BYTE_WIDTH

  val SRC_MAC_ADDR = 0x0123456789abcL
  val SRC_IP_ADDR = 0xc0a80101L

  val ETH_TYPE = 0x0800
  val IP_VERSION = 0x4
  val IHL = 0x5
  val DSCP = 0x0
  val ECN = 0x0
  val FLAGS = 0x2
  val FRAGMENT_OFFSET = 0x0
  val TTL = 0x40
  val PROTOCOL = 0x11

  val UDP_CHECKSUM = 0x0000

  val ETH_HEADER_LENGTH = 14
  val IP_HEADER_LENGTH = 20
  val UDP_HEADER_LENGTH = 8
  val HEADER_TOTAL_LENGTH =
    ETH_HEADER_LENGTH + IP_HEADER_LENGTH + UDP_HEADER_LENGTH

  val MTU = 1500
  val IP_LENGTH_MAX = MTU
  val UDP_LENGTH_MAX = IP_LENGTH_MAX - IP_HEADER_LENGTH

  val MAX_DATA_NUM = MTU - IP_HEADER_LENGTH - UDP_HEADER_LENGTH
  val MIN_DATA_NUM =
    22 // MIN_TRANSACTION_NUM(64) - MAC_LEN(14) - IP_LEN(20) - UDP_LEN(8)

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

import EthernetProtocolConstant._
case class EthernetProtocolHeaderConstructor(
    initField: mutable.LinkedHashMap[String, Int]
) extends Bundle {
  def constructHeader(): Array[Bits] = {
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
// interface workshop
trait FrameHeader {
  val frameFieldInit: mutable.LinkedHashMap[String, Int]
  val header: Array[Bits]
}

//object ARPHeader {
//  def apply(array: Array[Bits]): Array[Bits] = {
//    val gen = new ARPHeader
//    require(
//      array.length == gen.header.length,
//      s"Initializing parameters not enough! Require ${gen.header.length} but gave ${array.length}"
//    )
//    gen.header.zipWithIndex.foreach { case (data, idx) =>
//      data := array(idx)
//    }
//    gen.header
//  }
//
//  def unapply(header: Bits): (Array[String], Array[Bits]) = {
//    val gen = new ARPHeader
//    val bitWidth = gen.frameFieldInit.values.sum
//    require(
//      bitWidth == header.getWidth,
//      s"Initializing parameters not enough! Require ${bitWidth} but gave ${header.getWidth}"
//    )
//    val asLittleEnd = header.subdivideIn(bitWidth / 8 slices).reduce(_ ## _)
//    val tmp = asLittleEnd
//      .sliceBy(gen.frameFieldInit.values.toList.reverse)
//      .reverse
//      .toArray
//    gen.header.zipWithIndex.foreach { case (data, idx) =>
//      data := tmp(idx)
//    }
//    (gen.frameFieldInit.keys.toArray, gen.header)
//  }
//}
//
//case class ARPHeader() extends FrameHeader {
//  val frameFieldInit = mutable.LinkedHashMap(
//    "hardwareType" -> 2 * 8,
//    "protocolType" -> 2 * 8,
//    "hardwareLen" -> 8,
//    "protocolLen" -> 8,
//    "operation" -> 2 * 8,
//    "senderHardwareAddr" -> 6 * 8,
//    "senderProtocolAddr" -> 4 * 8,
//    "targetHardwareAddr" -> 6 * 8,
//    "targetProtocolAddr" -> 4 * 8
//  )
//  val header: Array[Bits] =
//    EthernetProtocolHeaderConstructor(frameFieldInit).constructHeader()
//}
