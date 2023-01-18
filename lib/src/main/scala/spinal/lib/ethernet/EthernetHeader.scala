package spinal.lib.ethernet

import spinal.core._
import EthernetProtocolConstant._

import scala.collection.mutable

object EthernetHeader extends HeaderOperate{
  def apply(array: Seq[Bits]): Seq[Bits] = {
    val gen = EthernetHeader()
    this.generate(array, gen)
  }
  def unapply(header: Bits): (Seq[String], Seq[Bits]) = {
    val extract = EthernetHeader()
    this.extract(header, extract)
  }

}
case class EthernetHeader() extends FrameHeader {
  override val frameFieldInit = mutable.LinkedHashMap(
    "dstMAC" -> MAC_ADDR_WIDTH,
    "srcMAC" -> MAC_ADDR_WIDTH,
    "ethType" -> ETH_TYPE_WIDTH
  )
  override val header: Array[Bits] =
    this.constructHeader(frameFieldInit)
}