package spinal.lib.bus.pcie.sim

import spinal.lib.bus.pcie._
import spinal.lib._
import spinal.core._

case class MemCmdHeader(fmt: Int = 0,
                      typ: Int = 0,
                      tc: Int = 0,
                      attr: Int = 0,
                      th: Boolean = false,
                      td: Boolean = false,
                      ep: Boolean = false,
                      at: Int = 0,
                      length: Int = 0,
                      reqId: Int = 0,
                      tag: Int = 0,
                      lastBe: Int = 0,
                      firstBe: Int = 0,
                      addr: BigInt = 0,
                      ph: Int = 0) {
  def buildBits(): BigInt = {
    val ret: BigInt = BigInt(fmt) << 125 | //fmt
                BigInt(typ) << 120 | //typ
                BigInt(tc) << 116 | //tc
                BigInt(attr>>2) << 114 |
                BigInt(attr & ((1<<2)-1)) << 108 |
                BigInt(th.toInt) << 112 |
                BigInt(td.toInt) << 111 |
                BigInt(ep.toInt) << 110 |
                BigInt(at) << 106 |
                BigInt(length) << 96  | //len
                BigInt(reqId) << 80  | //reqId
                BigInt(tag) << 72  | //tag
                BigInt(lastBe) << 68  | //lastbe
                BigInt(firstBe) << 64  | //firstbe
                addr << 2   | //addr
                BigInt(ph) << 0     //ph
    ret
  }
}
object MemCmdHeader {
  def parse(bits: BigInt): MemCmdHeader = {
    import Util._
    val fmt = take(bits, 125 to 127).toInt
    val typ = take(bits, 120 to 124).toInt // type
    val tc = take(bits, 116 to 118).toInt // TC
    val th = take(bits, 112 to 112) == 0 // TH
    val td = take(bits, 111 to 111) == 0 // TD
    val ep = take(bits, 110 to 110) == 0 // EP
    val attr = (take(bits, 108 to 109) | (take(bits, 114 to 114) << 2)).toInt  // attr
    val at = take(bits, 106 to 107).toInt // AT
    val length = take(bits, 96 to 105).toInt  //len
    val reqId = take(bits, 80 to 95).toInt //reqId
    val tag = take(bits, 72 to 79).toInt //tag
    val lastBe = take(bits, 68 to 71).toInt //lastbe
    val firstBe = take(bits, 64 to 67).toInt //firstbe
    val addr = take(bits, 2 to 63) //addr
    val ph = take(bits, 0 to 1).toInt //ph
    MemCmdHeader(
      fmt = fmt,
      typ = typ,
      tc = tc,
      attr = attr,
      th = th,
      td = td,
      ep = ep,
      at = at,
      length = length,
      reqId = reqId,
      tag = tag,
      lastBe = lastBe,
      firstBe = firstBe,
      addr = addr,
      ph = ph
    )
  }

  def createWriteSimple(dwAddr: BigInt, dwCount: Int, firstBe: Int, lastBe: Int, tag: Int=0, reqId: Int=0): MemCmdHeader = {
    MemCmdHeader(
      fmt = 3,
      typ = 0,
      addr = dwAddr,
      length = dwCount,
      firstBe = firstBe,
      lastBe = lastBe,
      reqId = reqId,
      tag = tag
    )
  }

  def createReadSimple(dwAddr: BigInt, dwCount: Int, firstBe: Int, lastBe: Int, tag: Int, reqId: Int=0): MemCmdHeader = {
    MemCmdHeader(
      fmt = 0,
      typ = 0,
      addr = dwAddr,
      length = dwCount,
      firstBe = firstBe,
      lastBe = lastBe,
      reqId = reqId,
      tag=tag
    )
  }
}

case class CplHeader(fmt: Int = 0,
                      typ: Int = 0,
                      tc: Int = 0,
                      attr: Int = 0,
                      th: Boolean = false,
                      td: Boolean = false,
                      ep: Boolean = false,
                      at: Int = 0,
                      cplId: Int = 0,
                      cplStatus: Int = 0,
                      bcm: Boolean = false,
                      reqId: Int = 0,
                      tag: Int = 0,
                      lowerAddr: Int = 0,
                      length: Int = 0,
                      byteCount: Int = 0) {
  def buildBits(): BigInt = {
    val bits: BigInt = BigInt(fmt) << 125 |
                BigInt(typ) << 120 |
                BigInt(tc) << 116 |
                BigInt(th.toInt) << 112 |
                BigInt(td.toInt) << 111 |
                BigInt(ep.toInt) << 110 |
                BigInt(at) << 106 |
                BigInt(attr>>2) << 114 |
                BigInt(attr & ((1<<2)-1)) << 108 |
                BigInt(cplId) << 80 |
                BigInt(cplStatus) << 77 |
                BigInt(bcm.toInt) << 76 |
                BigInt(reqId) << 48 |
                BigInt(tag) << 40 |
                BigInt(lowerAddr) << 32 |
                BigInt(length) << 96 |
                BigInt(byteCount) << 64
    bits
  }

}

object CplHeader {
  def parse(bits: BigInt): CplHeader = {
    import Util._
    val fmt = take(bits, 125 to 127).toInt
    val typ = take(bits, 120 to 124).toInt // type
    val tc = take(bits, 116 to 118).toInt // TC
    val th = take(bits, 112 to 112) == 0 // TH
    val td = take(bits, 111 to 111) == 0 // TD
    val ep = take(bits, 110 to 110) == 0 // EP
    val attr = (take(bits, 108 to 109) | (take(bits, 114 to 114) << 2)).toInt  // attr
    val at = take(bits, 106 to 107).toInt // AT
    val cplId = take(bits, 80 to 95).toInt // completer ID
    val cplStatus = take(bits, 77 to 79).toInt // completion status
    val bcm = take(bits, 76 to 76) == 0 // BCM
    val reqId = take(bits, 48 to 63).toInt // requester ID
    val tag = take(bits, 40 to 47).toInt // tag
    val lowerAddr = take(bits, 32 to 38).toInt // lower address
    val length = take(bits, 96 to 105).toInt // length
    val byteCount = take(bits, (64 to 75)).toInt // byte count
    CplHeader(
      fmt = fmt,
      typ = typ,
      tc = tc,
      attr = attr,
      th = th,
      td = td,
      ep = ep,
      at = at,
      cplId = cplId,
      cplStatus = cplStatus,
      bcm = bcm,
      reqId = reqId,
      tag = tag,
      lowerAddr = lowerAddr,
      length = length,
      byteCount = byteCount)
  }

  def createCplDSimple(cplId: Int, reqId: Int, tag: Int, lowerAddr: Int, length: Int, byteCount: Int): CplHeader = {
    CplHeader(
      fmt = 2,
      typ = 10,
      cplId = cplId,
      reqId = reqId,
      tag = tag,
      lowerAddr = lowerAddr,
      length = length,
      byteCount = byteCount)
  }
}

object TlpSimTest extends App {
  // val hdr = MemCmdHeader(
  //   fmt = 1,
  //   typ = 3,
  //   tc = 2,
  //   attr = 1,
  //   th = true,
  //   td = false,
  //   ep = true,
  //   at = 3,
  //   length = 2,
  //   reqId = 1,
  //   tag = 3,
  //   lastBe = 2,
  //   firstBe = 1,
  //   addr = 3,
  //   ph = 2
  // )
  // val bits = hdr.buildBits()
  // val hdrEmit = MemCmdHeader.parse(bits)
  // println(hdr)
  // println(hdrEmit)
  // assert(hdr == hdrEmit)
  val hdr = CplHeader(fmt = 2,
                      typ = 3,
                      tc = 1,
                      attr = 2,
                      th = true,
                      td = false,
                      ep = false,
                      at = 1,
                      cplId = 3,
                      cplStatus = 2,
                      bcm = true,
                      reqId = 6,
                      tag = 2,
                      lowerAddr = 6,
                      length = 2,
                      byteCount = 4)
  val bits = hdr.buildBits()
  val hdrEmit = CplHeader.parse(bits)
  println(hdr)
  println(hdrEmit)
  assert(hdr == hdrEmit)
  
}