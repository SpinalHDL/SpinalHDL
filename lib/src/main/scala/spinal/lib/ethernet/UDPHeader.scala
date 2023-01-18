package spinal.lib.ethernet

import EthernetProtocolConstant._
import spinal.core._

import scala.collection.mutable

object UDPHeader extends HeaderOperate{
  def apply(array: Seq[Bits]): Seq[Bits] = {
    val gen = UDPHeader()
    this.generate(array, gen)
  }
  def unapply(header: Bits): (Seq[String], Seq[Bits]) = {
    val extract = UDPHeader()
    this.extract(header, extract)
  }
}

case class UDPHeader() extends FrameHeader {
  val frameFieldInit = mutable.LinkedHashMap(
    "srcPort" -> UDP_PORT_WIDTH,
    "dstPort" -> UDP_PORT_WIDTH,
    "len" -> UDP_LENGTH_WIDTH,
    "udpChecksum" -> UDP_CHECKSUM_WIDTH
  )
  val header: Array[Bits] =
    this.constructHeader(frameFieldInit)
}

