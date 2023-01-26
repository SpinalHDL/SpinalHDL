package spinal.lib.ethernet

import EthernetProtocolConstant._
import spinal.core._

import scala.collection.mutable

object UDPHeader extends HeaderOperate{
  def apply(array: Seq[Bits]): Seq[Bits] = {
    val gen = UDPHeader()
    this.generate(array, gen)
  }
  def unapply(header: Bits): (Seq[EthernetProtocolField], Seq[Bits]) = {
    val extract = UDPHeader()
    this.extract(header, extract)
  }
}

object UDPFields {
  val srcPort = new EthernetProtocolField
  val dstPort = new EthernetProtocolField
  val len = new EthernetProtocolField
  val udpChecksum = new EthernetProtocolField
}

case class UDPHeader() extends FrameHeader {
  val frameFieldInit = mutable.LinkedHashMap(
    UDPFields.srcPort -> UDP_PORT_WIDTH,
    UDPFields.dstPort -> UDP_PORT_WIDTH,
    UDPFields.len -> UDP_LENGTH_WIDTH,
    UDPFields.udpChecksum -> UDP_CHECKSUM_WIDTH
  )
  val header: Array[Bits] =
    this.constructHeader(frameFieldInit)
}


