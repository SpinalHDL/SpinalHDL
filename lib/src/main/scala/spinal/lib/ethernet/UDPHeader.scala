package spinal.lib.ethernet

import spinal.core._
import EthernetProtocolConstant._

import scala.collection.mutable

object UDPHeader {
  def apply(array: Array[Bits]): Array[Bits] = {
    val gen = new UDPHeader
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
    val gen = new UDPHeader
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

case class UDPHeader() extends Bundle with FrameHeader {
  val frameFieldInit = mutable.LinkedHashMap(
    "srcPort" -> UDP_PORT_WIDTH,
    "dstPort" -> UDP_PORT_WIDTH,
    "len" -> UDP_LENGTH_WIDTH,
    "udpChecksum" -> UDP_CHECKSUM_WIDTH
  )
  val header: Array[Bits] =
    EthernetProtocolHeaderConstructor(frameFieldInit).constructHeader()
}