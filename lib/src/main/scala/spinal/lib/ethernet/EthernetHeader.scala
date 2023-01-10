package spinal.lib.ethernet

import spinal.core._
import EthernetProtocolConstant._
import scala.collection.mutable

object EthernetHeader {
  def apply(array: Array[Bits]): Array[Bits] = {
    val gen = new EthernetHeader
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
    val gen = new EthernetHeader
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

case class EthernetHeader() extends FrameHeader {
  val frameFieldInit = mutable.LinkedHashMap(
    "dstMAC" -> MAC_ADDR_WIDTH,
    "srcMAC" -> MAC_ADDR_WIDTH,
    "ethType" -> ETH_TYPE_WIDTH
  )
  val header: Array[Bits] =
    EthernetProtocolHeaderConstructor(frameFieldInit).constructHeader()
}