package spinal.lib.ethernet

import spinal.core._
import EthernetProtocolConstant._

import scala.collection.mutable

object EthernetHeader extends HeaderOperate{
  def apply(array: Seq[Bits]): Seq[Bits] = {
    val gen = EthernetHeader()
    this.generate(array, gen)
  }
  def unapply(header: Bits): (Seq[EthernetProtocolField], Seq[Bits]) = {
    val extract = EthernetHeader()
    this.extract(header, extract)
  }
}

object EthernetFields {
  val dstMAC = new EthernetProtocolField
  val srcMAC = new EthernetProtocolField
  val ethType = new EthernetProtocolField
}

case class EthernetHeader() extends FrameHeader {
  override val frameFieldInit = mutable.LinkedHashMap(
    EthernetFields.dstMAC -> MAC_ADDR_WIDTH,
    EthernetFields.srcMAC -> MAC_ADDR_WIDTH,
    EthernetFields.ethType -> ETH_TYPE_WIDTH
  )
  override val header: Array[Bits] =
    this.constructHeader(frameFieldInit)
}