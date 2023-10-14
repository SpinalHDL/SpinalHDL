package spinal.lib.bus.pcie.phy

import spinal.core._
import spinal.lib.bus.amba4.axi._
import spinal.lib._
import spinal.lib.bus.amba4.axis._
import spinal.lib.fsm._
import spinal.lib.bus.pcie.Header
import spinal.lib.bus.pcie.CplHeader
import spinal.lib.bus.pcie.MemCmdHeader
import spinal.lib.bus.pcie.TlpConfig
import spinal.lib.bus.pcie.Tlp

object PcieUsIf {

  object reqTyp {
    def apply() = Bits(4 bits)

    def MRd = B"0000"
    def MWr = B"0001"
    def IORd = B"0010"
    def IOWr = B"0011"
    def FetchAdd = B"0100"
    def Swap = B"0101"
    def CAS = B"0110"
    def LockedReadRequest = B"0111" //allowed only in Legacy Devices
    def Type0ConfigurationReadRequest = B"1000" // (on Requester side only)
    def Type1ConfigurationReadRequest = B"1001" // (on Requester side only)
    def Type0ConfigurationWriteRequest = B"1010" // (on Requester side only)
    def Type1ConfigurationWriteRequest = B"1011" // (on Requester side only)
    def Anymessage = B"1100" // except ATS and Vendor-Defined Messages
    def VendorDefinedMessage = B"1101"
    def ATSMessage = B"1110"

    // us reqTyp => (tlpFmt, tlpTyp)
    def getTlpTyp(bits: Bits): (UInt, UInt) = {
      assert(bits.getWidth == 4)
      val fmt = Header.Fmt()
      val typ = Header.Type()
      switch(bits) {
        is(MRd) {fmt := Header.Fmt.DW4; typ:= Header.Type.MRd}
        is(MWr) {fmt := Header.Fmt.DW4_DATA; typ:= Header.Type.MWr}
        is(IORd) {fmt := Header.Fmt.DW3; typ:= Header.Type.IORd}
        is(IOWr) {fmt := Header.Fmt.DW3_DATA; typ:= Header.Type.IOWr}
        // is(FetchAdd) {fmt := Header.FetchAdd4.fmt; typ:= Header.FetchAdd4.typ}
        // is(Swap) {fmt := Header.Swap4.fmt; typ:= Header.Swap4.typ}
        // is(CAS) {fmt := Header.CAS4.fmt; typ:= Header.CAS4.typ}
        // is(LockedReadRequest) {fmt := Header.MRdLk4.fmt; typ:= Header.MRdLk4.typ}
        // is(Type0ConfigurationReadRequest) {fmt := ; typ:= }
        // is(Type1ConfigurationReadRequest) {fmt := ; typ:= }
        // is(Type0ConfigurationWriteRequest) {fmt := ; typ:= }
        // is(Type1ConfigurationWriteRequest) {fmt := ; typ:= }
        // is(Anymessage) {fmt := ; typ:= }
        // is(VendorDefinedMessage) {fmt := ; typ:= }
        // is(ATSMessage) {fmt := ; typ:= }
        
        // * default MRd
        default {fmt := Header.Fmt.DW4; typ := Header.Type.MRd}
      }
      (fmt, typ)
    }

    def getReqType(fmt: UInt, typ: UInt): Bits = {
      val ret = apply()
      switch(fmt ## typ) {
        is(Header.Fmt.DW4##Header.Type.MRd, Header.Fmt.DW3##Header.Type.MRd) {
          ret := MRd
        }
        is(Header.Fmt.DW4_DATA##Header.Type.MWr, Header.Fmt.DW3_DATA##Header.Type.MWr) {
          ret := MWr
        }
        is(Header.Fmt.DW3##Header.Type.IORd) {
          ret := IORd
        }
        is(Header.Fmt.DW3_DATA##Header.Type.IOWr) {
          ret := IOWr
        }
        default {
          ret := MRd
        }
      }
      ret
    }
  }

}


class PcieUsIfCq(usAxisConfig: PcieUsAxisIfConfig, tlpConfig: TlpConfig) extends Component {
  val io = new Bundle {
    val cq = slave(Stream Fragment PcieUsAxisIf(usAxisConfig))
    val rxReq = master(Stream Fragment Tlp(tlpConfig))
  }

  case class UsCqCmdGen() extends Bundle {
    val data = cloneOf(io.cq.data)
    val hdr = Bits(128 bits)
    val user = cloneOf(io.cq.user)
  }
  val hisGen = new Area {
    val raw = Stream Fragment UsCqCmdGen()
    // todo polish
    val staged = io.cq.stage
    when(staged.last) {
      raw.translateFrom(staged) {(to, from) =>
        to.last := True
        to.hdr := from.data.take(128)
        to.user := from.user
        to.data := from.data.drop(128).resized
      }
    } otherwise {
      raw.valid := staged.valid && io.cq.valid
      staged.ready := raw.ready && staged.valid && io.cq.valid
      raw.user := staged.user
      raw.hdr := staged.data.take(128)
      raw.data := (io.cq.data ## staged.data).drop(128).resized
      raw.last := False
    }
  }

  val hdr = MemCmdHeader()
  val (fmt, typ) = PcieUsIf.reqTyp.getTlpTyp(hisGen.raw.hdr(75, 4 bits))
  hdr.fmt := fmt
  hdr.typ := typ
  hdr.tc := hisGen.raw.hdr(121, 3 bits)
  hdr.attr :=  hisGen.raw.hdr(124, 3 bits)
  hdr.th := False
  hdr.td := False
  hdr.ep := False
  hdr.at := hisGen.raw.hdr(0, 2 bits)
  // ???
  hdr.len := hisGen.raw.hdr(64, 11 bits).asUInt.resized
  hdr.reqId := hisGen.raw.hdr(80, 16 bits).asUInt
  hdr.tag := hisGen.raw.hdr(96, 8 bits).asUInt
  hdr.lastBe := hisGen.raw.user(4, 4 bits)
  hdr.firstBe := hisGen.raw.user(0, 4 bits)
  hdr.addr := (hisGen.raw.hdr(2, 62 bits) ## B(0, 2 bits)).asUInt
  hdr.ph := 0
  
  io.rxReq.translateFrom(hisGen.raw) {(to, from) =>
    to.data := from.data
    to.last := from.last
    to.hdr :=  hdr.asBits
    to.bar_id := from.hdr(112, 3 bits).asUInt
    to.func_num := from.hdr(104, 8 bits).asUInt
  }
}

class PcieUsIfCc(usAxisConfig: PcieUsAxisIfConfig, tlpConfig: TlpConfig) extends Component {
  val io = new Bundle {
    val cc = master(Stream Fragment PcieUsAxisIf(usAxisConfig))
    val txCpl = slave(Stream Fragment Tlp(tlpConfig))
  }
  
  val hdr = new CplHeader
  hdr.assignFromBits(io.txCpl.hdr)
  val ccHdrBits = Bits(96 bits)
  ccHdrBits(0, 7 bits)    := hdr.lowerAddr.asBits // lower address
  ccHdrBits(7)            := False
  ccHdrBits(8, 2 bits)    := hdr.at // AT
  ccHdrBits(10, 6 bits)   := 0
  ccHdrBits(16, 13 bits)  := hdr.byteCnt.asBits.resized // Byte count
  ccHdrBits(29)           := False // locked read completion
  ccHdrBits(30, 2 bits)   := 0
  ccHdrBits(32, 11 bits)  := hdr.len.asBits.resized // DWORD count
  ccHdrBits(43, 3 bits)   := hdr.cplStatus.asBits // completion status
  ccHdrBits(46)           := hdr.ep // poisoned
  ccHdrBits(47)           := False
  ccHdrBits(48, 16 bits)  := hdr.reqId.asBits // requester ID
  ccHdrBits(64, 8 bits)   := hdr.tag.asBits // tag
  ccHdrBits(72, 16 bits)  := hdr.cplId.asBits // completer ID
  ccHdrBits(88)           := False // completer ID enable
  ccHdrBits(89, 3 bits)   := hdr.tc // TC
  ccHdrBits(92, 3 bits)   := hdr.attr // attr
  ccHdrBits(95)           := False // force ECRC

  val staged = RegNextWhen(io.txCpl.payload, io.txCpl.fire)
  val cleanLast = RegInit(False)
  io.cc.user := 0

  when(cleanLast) {
    // take tail 96 bit
    io.txCpl.ready := False
    io.cc.valid := True

    io.cc.data := staged.data.takeHigh(96).resized
    io.cc.last := True
    io.cc.keep := staged.strb.takeHigh(3).resized

    cleanLast clearWhen io.cc.fire
  } elsewhen(io.txCpl.first) {
    io.cc.arbitrationFrom(io.txCpl)
    
    io.cc.data := io.txCpl.data.dropHigh(96) ## ccHdrBits
    io.cc.last := io.txCpl.last
    io.cc.keep := io.txCpl.strb.dropHigh(3) ## B"111"
  } otherwise {
    // take data and previous data
    io.cc.arbitrationFrom(io.txCpl)
    io.cc.data := io.txCpl.data.dropHigh(96) ## staged.data.takeHigh(96)
    io.cc.last := io.txCpl.last
    io.cc.keep := io.txCpl.strb.dropHigh(3) ## staged.strb.takeHigh(3)
  }
  when(!cleanLast && io.txCpl.last && io.txCpl.strb.takeHigh(3) =/= 0 && io.txCpl.fire) {
    cleanLast := True
    io.cc.last := False
  }
}

class PcieUsIfRq(usAxisConfig: PcieUsAxisIfConfig, tlpConfig: TlpConfig) extends Component {
  val io = new Bundle {
    val txReqRd = slave(Stream Fragment Tlp(tlpConfig))
    val rdSeq = master(Flow(UInt(3 bits)))
    val txReqWr = slave(Stream Fragment Tlp(tlpConfig))
    val wrSeq = master(Flow(UInt(3 bits)))
    val rq = master(Stream Fragment PcieUsAxisIf(usAxisConfig))

    val s_axis_rq_seq_num_0 = slave Flow(UInt(4 bits))
    // val s_axis_rq_seq_num_1 = slave Flow(UInt(4 bits))
  }
  val arbiter = new StreamArbiter(Fragment(Tlp(tlpConfig)), 2)(StreamArbiter.Arbitration.lowerFirst, StreamArbiter.Lock.fragmentLock)
  arbiter.io.inputs(0).arbitrationFrom(io.txReqRd)
  arbiter.io.inputs(0).payload.assignDontCare().allowOverride()
  arbiter.io.inputs(0).payload.assignSomeByName(io.txReqRd.payload)
  arbiter.io.inputs(1).arbitrationFrom(io.txReqWr)
  arbiter.io.inputs(1).payload.assignDontCare().allowOverride()
  arbiter.io.inputs(1).payload.assignSomeByName(io.txReqWr.payload)

  val txReq = arbiter.io.output
  val hdr = MemCmdHeader()
  hdr.assignFromBits(txReq.payload.hdr)

  val rqHdr = Bits(128 bits)
  rqHdr(0, 2 bits) := hdr.at
  rqHdr(2, 62 bits) := hdr.addr.drop(2)
  rqHdr(64, 11 bits) := hdr.len.asBits.resized
  when(hdr.len === 0) {
    rqHdr(64, 11 bits) := 1024
  }
  // todo polish
  rqHdr(75, 4 bits) := PcieUsIf.reqTyp.getReqType(hdr.fmt, hdr.typ)
  rqHdr(79) := hdr.ep
  rqHdr(80, 16 bits) := hdr.reqId.asBits
  rqHdr(96, 8 bits) := hdr.tag.asBits
  rqHdr(104, 16 bits) := 0 // cplId
  rqHdr(120) := False // requester ID enable
  rqHdr(121, 3 bits) := hdr.tc
  rqHdr(124, 3 bits) := hdr.attr
  rqHdr(127) := False //force ece

  val rqUser = cloneOf(io.rq.user)
  rqUser(0, 4 bits) := hdr.firstBe
  rqUser(4, 4 bits) := hdr.lastBe
  rqUser(8, 3 bits) := 0 // addr_offset
  rqUser(11) := False // discontinue
  rqUser(12) := False // tph_present
  rqUser(13, 2 bits) := 0 // tph_type
  rqUser(15) := False // tph_indirect_tag_en
  rqUser(16, 8 bits) := 0 // tph_st_tag
  rqUser(24, 4 bits) := True ## io.wrSeq.payload //seq_num
  rqUser(28, 32 bits) := 0 // parity
  // rqUser(60, 2 bits) := tx_rd_req_tlp_seq_int >> 4; // seq_num

  // the same with cc
  val staged = RegNextWhen(txReq.payload, txReq.fire)
  val cleanLast = RegInit(False)

  when(arbiter.io.chosen === 0) {
    // read
    io.rq.valid := True
    txReq.ready := True

    io.rq.data := rqHdr.resized
    io.rq.last := True
    io.rq.keep := B"111".resized
    io.rq.user := rqUser
  } otherwise {
    // write
    when(cleanLast) {
      io.rq.valid := True
      txReq.ready := False

      io.rq.data := staged.data.takeHigh(128).resized
      io.rq.last := True
      io.rq.keep := staged.strb.takeHigh(4).resized
      io.rq.user := rqUser
      
      cleanLast clearWhen io.rq.fire
    } elsewhen(txReq.first) {
      io.rq.arbitrationFrom(txReq)
      
      io.rq.data := txReq.data.dropHigh(128) ## rqHdr
      io.rq.last := txReq.last
      io.rq.keep := txReq.strb.dropHigh(4) ## B"1111"
      io.rq.user := rqUser
    } otherwise {
      io.rq.arbitrationFrom(txReq)

      io.rq.data := txReq.data.dropHigh(128) ## staged.data.takeHigh(128)
      io.rq.last := txReq.last
      io.rq.keep := txReq.strb.dropHigh(4) ## staged.strb.takeHigh(4)
      io.rq.user := rqUser
    }

    when(!cleanLast && txReq.last && txReq.strb.takeHigh(3) =/= 0 && txReq.fire) {
      io.rq.last := False
      cleanLast := True
    }
  }

  val highBit = io.s_axis_rq_seq_num_0.payload.takeHigh(1) === 1
  io.rdSeq.translateFrom(io.s_axis_rq_seq_num_0.throwWhen(highBit)) {(to, from) =>
    to := from.resized
  }

  io.wrSeq.translateFrom(io.s_axis_rq_seq_num_0.throwWhen(!highBit)) {(to, from) =>
    to := from.resized
  }

}

class PcieUsIfRc(usAxisConfig: PcieUsAxisIfConfig, tlpConfig: TlpConfig) extends Component {
  val io = new Bundle {
    val rc = slave(Stream Fragment PcieUsAxisIf(usAxisConfig))
    val rxReq = master(Stream Fragment Tlp(tlpConfig))
  }

  val hisGen = new Area {
    case class UsRcCmdGen() extends Bundle {
      val data = cloneOf(io.rc.data)
      val hdr = Bits(96 bits)
      val user = cloneOf(io.rc.user)
    }
    val hisGen = new Area {
      val raw = Stream Fragment UsRcCmdGen()
      val staged = io.rc.stage
      when(staged.last) {
        raw.translateFrom(staged) {(to, from) =>
          to.last := True
          to.hdr := from.data.take(96)
          to.user := from.user
          to.data := from.data.drop(96).resized
        }
      } otherwise {
        raw.valid := staged.valid && io.rc.valid
        staged.ready := raw.ready && staged.valid && io.rc.valid
        raw.user := staged.user
        raw.hdr := staged.data.take(96)
        raw.data := (io.rc.data ## staged.data).drop(96).resized
        raw.last := False
      }
    }
    val hdr = new CplHeader
    hdr.fmt := Header.Fmt.DW3_DATA
    when(hisGen.raw.hdr(32, 11 bits) === 0) {
      hdr.fmt := Header.Fmt.DW3
    }
    // cpl or cpl locked
    hdr.typ := (B"0101" ## hisGen.raw.hdr(29)).asUInt
    hdr.tc := hisGen.raw.hdr(89, 3 bits)
    hdr.attr := hisGen.raw.hdr(92, 3 bits)
    hdr.th := False
    hdr.td := False
    hdr.ep := hisGen.raw.hdr(46)
    hdr.at := 0
    hdr.len := hisGen.raw.hdr(32, 11 bits).asUInt.resized
    hdr.cplId := hisGen.raw.hdr(72, 16 bits).asUInt
    hdr.cplStatus := hisGen.raw.hdr(43, 3 bits).asUInt
    hdr.byteCnt := hisGen.raw.hdr(16, 13 bits).asUInt.resized
    hdr.reqId := hisGen.raw.hdr(48, 16 bits).asUInt
    hdr.tag := hisGen.raw.hdr(64, 8 bits).asUInt
    hdr.lowerAddr := hisGen.raw.hdr(0, 7 bits).asUInt
    hdr.bcm := False

    io.rxReq.translateFrom(hisGen.raw) {(to, from) =>
      to.last := from.last
      to.data := from.data
      to.hdr := hdr.asBits
      to.error := from.hdr(12, 4 bits).asUInt
    }
  }


}

