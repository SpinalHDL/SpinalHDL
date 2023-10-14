package spinal.lib.bus.pcie

import spinal.core._

abstract class Header extends Bundle {
  val fmt = Header.Fmt()
  val typ = Header.Type()
  val tc = Header.TC()
  val attr = Header.Attr()
  val th = Header.TH()
  val td = Header.TD()
  val ep = Header.EP()
  val at = Header.AT()
  val len = Header.Length()

  override def assignFromBits(bits: Bits): Unit = {
    fmt := bits(125, 3 bits).asUInt
    typ := bits(120, 5 bits).asUInt
    tc := bits(116, 3 bits)
    attr := bits(114) ## bits(108, 2 bits)
    th := bits(112)
    td := bits(111)
    ep := bits(110)
    at := bits(106, 2 bits)
    len := bits(96, 10 bits).asUInt
  }

  override def asBits: Bits = {
    val bits = Bits(128 bits)
    bits(125, 3 bits) := fmt.asBits
    bits(120, 5 bits) := typ.asBits
    bits(119) := False // reserved
    bits(116, 3 bits) := tc
    bits(115) := False // reserved
    bits(114) := attr.takeHigh(1).asBool
    bits(113) := False // reserved
    bits(112) := th
    bits(111) := td
    bits(110) := ep
    bits(108, 2 bits) := attr.dropHigh(1)
    bits(106, 2 bits) := at
    bits(96, 10 bits) := len.asBits
    bits
  }

  def assignSimple() {
    tc := tc.getZero
    attr := attr.getZero
    th := th.getZero
    td := td.getZero
    ep := ep.getZero
    at := at.getZero
  }

  override def assignFromBits(bits: Bits, offset: Int, bitCount: BitCount) = ???
}

case class MemCmdHeader() extends Header {
  val addr = Header.Addr()
  val reqId = Header.Requester_ID()
  val tag = Header.Tag()
  val lastBe = Header.Last_DW_BE()
  val firstBe = Header.First_DW_BE()
  val ph = Header.PH()

  // fmt
  // typ
  // addr
  // tag
  // lastBe
  // firstBe
  // len
  override def assignSimple(): Unit = {
    super.assignSimple()
    reqId := reqId.getZero
    ph := ph.getZero
  }

  override def assignFromBits(bits: Bits): Unit = {
    // val TLP_FORCE_64_BIT_ADDR = true

    super.assignFromBits(bits)
    reqId := bits(80, 16 bits).asUInt
    tag := bits(72, 8 bits).asUInt
    lastBe := bits(68, 4 bits)
    firstBe := bits(64, 4 bits)
    // when(fmt(0) || Bool(TLP_FORCE_64_BIT_ADDR)) {
      // 4 DW (64-bit address)
      // DW 2+3
    addr := bits(2, 62 bits).asUInt // addr
    ph := bits(0, 2 bits) // PH
    // } otherwise {
      // 3 DW (32-bit address)
      // DW 2
      // addr := (bits(34, 30 bits) ## B(0, 2 bits)).asUInt.resized // addr
      // ph := bits(32, 2 bits) // PH
    // }

  }

  override def asBits: Bits = {
    val bits = super.asBits
    // val TLP_FORCE_64_BIT_ADDR = true
    bits(80, 16 bits) := reqId.asBits
    bits(72, 8 bits) := tag.asBits
    bits(68, 4 bits) := lastBe
    bits(64, 4 bits) := firstBe
    // when(fmt(0) && (addr.takeHigh(32) =/= 0 || Bool(TLP_FORCE_64_BIT_ADDR))) {
      // 4 DW (64-bit address)
      // DW 2+3
    bits(2, 62 bits) := addr.asBits
    bits(0, 2 bits) := ph
    // } otherwise {
    //   // 3 DW (32-bit address)
    //   // DW 2
    //   bits(34, 30 bits) := addr.dropLow(2).resized
    //   bits(0, 32 bits) := 0
    //   bits(32, 2 bits) := ph
    // }
    bits
  }
}

case class CplHeader() extends Header {
  val cplId = Header.Completer_ID()
  val cplStatus = Header.Completer_Status()
  val bcm = Header.BCM()
  val byteCnt = Header.Byte_Count()
  val reqId = Header.Requester_ID()
  val tag = Header.Tag()
  val lowerAddr = Header.Lower_Address()

  // fmt
  // typ
  // byteCnt
  // reqId
  // tag
  // lowerAddr
  // cplId
  // len

  override def assignSimple(): Unit = {
    super.assignSimple()
    cplStatus := cplStatus.getZero
    bcm := bcm.getZero
  }

  override def assignFromBits(bits: Bits): Unit = {
    super.assignFromBits(bits)
    cplId := bits(80, 16 bits).asUInt
    cplStatus := bits(77, 3 bits).asUInt
    bcm := bits(76)
    byteCnt := bits(64, 12 bits).asUInt
    reqId := bits(48, 16 bits).asUInt
    tag := bits(40, 8 bits).asUInt
    lowerAddr := bits(32, 7 bits).asUInt
  }

  override def asBits: Bits = {
    val data = super.asBits
    data(80, 16 bits) := cplId.asBits
    data(77, 3 bits) := cplStatus.asBits
    data(76) := bcm
    data(64, 12 bits) := byteCnt.asBits
    data(48, 16 bits) := reqId.asBits
    data(40, 8 bits) := tag.asBits
    data(39) := False
    data(32, 7 bits) := lowerAddr.asBits
    data(0, 32 bits) := B(0, 32 bits)
    data
  }
}

object Header {
  object Completer_Status {
    def apply() = UInt(3 bits)

    def SC = 0 // successful completion
    def UR = 1 // unsupported request
    def CRS = 2 // configuration request retry status
    def CA = 4 // completer abort
  }
  object Byte_Count {
    def apply() = UInt(12 bits) 
  }
  object Lower_Address {
    def apply() = UInt(7 bits)
  }
  object Requester_ID {
    def apply() = UInt(16 bits)
  }
  object Completer_ID {
    def apply() = UInt(16 bits)
  }
  object Tag {
    def apply() = UInt(8 bits)
  }
  object TC {
    def apply() = Bits(3 bits)
  }
  object Attr {
    def apply() = Bits(3 bits)
    def STRONG = B"000"
    def RELAXED = B"010"
    def IDO = B"100"
    def RELAXED_AND_IDO = B"110"
    
    def NO_SNOOP = B"001"
  }
  object Last_DW_BE{
    def apply() = Bits(4 bits)
  }
  object First_DW_BE{
    def apply() = Bits(4 bits)
  }
  object Addr{
    def apply() = UInt(62 bits)
  }
  object PH{
    def apply() = Bits(2 bits)
  }
  object Fmt{
    def apply() = UInt(3 bits)
    def DW3 = U"000"
    def DW4 = U"001"
    def DW3_DATA = U"010"
    def DW4_DATA = U"011"
    def TLP_PREFIX = U"100"
  }
  object Type{
    def apply() = UInt(5 bits)
    def MRd = U"00000"
    def MRdLk = U"00001"
    def MWr = U"00000"
    def IORd = U"00010"
    def IOWr = U"00010"
    def CfgRd0 = U"00100"
    def CfgWr0 = U"00100"
    def CfgRd1 = U"00101"
    def CfgWr1 = U"00101"
    def TCfgRd = U"11011"
    def TCfgWr = U"11011"
    def Cpl = U"01010"
    def CplD = U"01010"
    def CplLk = U"01011"
    def CplDLk = U"01011"
    def FetchAdd = U"01100"
    def Swap = U"01101"
    def CAS = U"01110"
  }
  object TH{
    def apply() = Bool()
  }
  object TD{
    def apply() = Bool()
  }
  object EP{
    def apply() = Bool()
  }
  object BCM{
    def apply() = Bool()
  }
  object AT{
    def apply() = Bits(2 bits)
  }
  object Length{
    def apply() = UInt(10 bits)
  }
}