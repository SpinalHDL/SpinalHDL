package spinal.lib.bus.pcie

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba4.axilite._

object PcieAxilMaster {
  def apply() = {
    val axilConfig = AxiLite4Config(32, 32)
    val tlpConfig = TlpConfig(64/32)
    new PcieAxilMaster(axilConfig, tlpConfig)
  }
}

class PcieAxilMaster(axilConfig: AxiLite4Config, tlpConfig: TlpConfig, tlpForce64BitAddr: Boolean = false) extends Component {
  val io = new Bundle {
    val axil = master(AxiLite4(axilConfig))
    val rxReq = slave(Stream Fragment Tlp(tlpConfig))
    val txCpl = master(Stream Fragment Tlp(tlpConfig))

    val cplId = in(Header.Completer_ID())

    val error_cor = out Bool()
    val error_uncor = out Bool()
  }
  assert(axilConfig.dataWidth == 32)

  io.error_cor := False
  io.error_uncor := False


  case class rdCmdInfo() extends Bundle {
    val cplStatus = Header.Completer_Status()
    val byteCnt = Header.Byte_Count()
    val lowerAddr = Header.Lower_Address()
    val reqId = Header.Requester_ID()
    val tag = Header.Tag()
    val tc = Header.TC()
    val attr = Header.Attr()
  }

  val reqLogic = new Area {
    
    val hdr = new MemCmdHeader
    hdr.assignFromBits(io.rxReq.hdr)

    // val zeroLenOp = hdr.len === 1 && hdr.firstBe === 0
    // len must be 1
    val rdCmd = Stream(rdCmdInfo())
    
    val wrCmd = Stream(NoData)

    val tlpRd, tlpWr = cloneOf(io.rxReq)
    val isMRead = !hdr.fmt(1) && hdr.typ === 0
    val isMWrite = hdr.fmt(1) && hdr.typ === 0
    Vec(tlpRd, tlpWr) <> StreamDemuxOh(io.rxReq, Seq(isMRead, isMWrite))
    
    val (arChannel, rdFifo) = StreamFork2(tlpRd)
    io.axil.ar.translateFrom(arChannel){(to, from) => 
      to.addr := hdr.addr.resized
      to.prot := AxiLite4.prot.NON_SECURE_ACCESS
    }
    rdCmd.translateFrom(rdFifo) {(to, from) =>
      to.attr := hdr.attr
      to.cplStatus := Header.Completer_Status.SC
      to.reqId := hdr.reqId
      to.tag := hdr.tag
      to.tc := hdr.tc
      to.byteCnt := CountOne(hdr.firstBe).resized
      when(hdr.firstBe === B(0)) {
        to.byteCnt := 1
      }
      // to.byteCnt := (hdr.firstBe === B(0)) ? U(1, to.byteCnt.getWidth bits) | 
        // (CountOne(hdr.firstBe).resize(to.byteCnt.getWidth bits))
      to.lowerAddr := Cat(hdr.addr(2, 5 bits), OHToUInt(OHMasking.first(hdr.firstBe))).asUInt
    }

    val (awChannel, wChannel, wrFifo) = StreamFork3(tlpWr)
    io.axil.aw.translateFrom(awChannel) {(to, from) =>
      to.addr := hdr.addr.resized
      to.prot := AxiLite4.prot.NON_SECURE_ACCESS
    }
    io.axil.w.translateFrom(wChannel) {(to, from) =>
      to.data := io.rxReq.data.resized
      to.setStrb(hdr.firstBe)
    }
    wrCmd.arbitrationFrom(wrFifo)
  }

  val rdRespLogic = new Area {
    val queuedCmd = reqLogic.rdCmd.queue(16)
    val hdr = new CplHeader

    hdr.fmt := Header.Fmt.DW3_DATA
    hdr.typ := Header.Type.CplD
    hdr.tc := queuedCmd.tc
    hdr.attr := queuedCmd.attr
    hdr.th := False
    hdr.td := False
    hdr.ep := False
    hdr.at := 0
    hdr.len := 1
    hdr.cplId := io.cplId
    hdr.cplStatus := queuedCmd.cplStatus
    hdr.bcm := False
    hdr.byteCnt := queuedCmd.byteCnt
    hdr.reqId := queuedCmd.reqId
    hdr.tag := queuedCmd.tag
    hdr.lowerAddr := queuedCmd.lowerAddr

    val rdCpl = StreamJoin.arg(queuedCmd, io.axil.r)
    io.txCpl.translateFrom(rdCpl) {(to, form) =>
      to.data := io.axil.r.data.resized
      to.last := True
      to.hdr := hdr.asBits
      to.strb := 1
    }
  }

  val wrRespLogic = new Area {
    val queuedCmd = reqLogic.wrCmd.queue(16)
    // ? 有必要吗
    StreamJoin.arg(queuedCmd, io.axil.b).freeRun()
  }

  // val debug = new Area {
  //   val reqTyp = Tlp.TlpType.getAscii(reqLogic.hdr)
  //   val respTyp = Tlp.TlpType.getAscii(rdRespLogic.hdr)
  // }

}
