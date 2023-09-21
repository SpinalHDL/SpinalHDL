package spinal.lib.bus.pcie

import spinal.core._
import spinal.lib._

import scala.collection.Seq
import spinal.lib.bus.misc.BusSlaveFactoryDelayed
import spinal.lib.bus.misc.SingleMapping


case class PcieSlaveFactory(cmd: Stream[Fragment[Tlp]], rsp: Stream[Fragment[Tlp]]) extends BusSlaveFactoryDelayed {
  assert(rsp.config.useStrb && (rsp.config.dwCount == 1 || rsp.config.dwCount == 2), "currently only support 1 dw request")

  val readHaltRequest = False
  val writeHaltRequest = False

  val demux = new Area {
    val cmdHdr = MemCmdHeader()
    cmdHdr.assignFromBits(cmd.hdr)
    
    val lenValid = cmdHdr.len === 1 || cmdHdr.len === 2
    val readReq = (cmdHdr.fmt === Header.Fmt.DW3 || cmdHdr.fmt === Header.Fmt.DW4) && lenValid
    val writeReq = (cmdHdr.fmt === Header.Fmt.DW3_DATA || cmdHdr.fmt === Header.Fmt.DW4_DATA) && lenValid
    
    val readCmd, WriteCmd = cloneOf(cmd)
    (Vec(readCmd, WriteCmd), StreamDemuxOh(cmd, Seq(readReq && cmd.first, writeReq && cmd.first))).zipped map (_ << _)
  }

  val read = new Area {
    val readDataStage = demux.readCmd.stage()
    val hdr = MemCmdHeader()
    hdr.assignFromBits(readDataStage.hdr)
    val askRead = readDataStage.fire
    val doRead = rsp.fire

    val rspData = cloneOf(rsp.payload)
    val rspHdr = CplHeader()
    rspData.hdr := rspHdr.asBits
    rspData.last := True
    when(hdr.len === 1) {
      rspData.strb := 1
    } otherwise {
      rspData.strb := 3
    }
    rspData.data := 0
    rspData.config.useSeq generate {rspData.seq := 0}
    rspData.config.withBarFunc generate {rspData.bar_id := 0; rspData.func_num := 0}
    rspData.config.useError generate {rspData.error := 0}
    rspHdr.assignSimple()
    rspHdr.fmt := Header.Fmt.DW3_DATA
    rspHdr.typ := Header.Type.CplD
    rspHdr.reqId := hdr.reqId
    rspHdr.tag := hdr.tag
    rspHdr.cplId := 0
    rspHdr.lowerAddr := 0
    switch(hdr.firstBe) {
      is(M"0000") {rspHdr.lowerAddr := (hdr.addr##B"00").asUInt.resized}
      is(M"---1") {rspHdr.lowerAddr := (hdr.addr##B"00").asUInt.resized}
      is(M"--10") {rspHdr.lowerAddr := (hdr.addr##B"01").asUInt.resized}
      is(M"-100") {rspHdr.lowerAddr := (hdr.addr##B"10").asUInt.resized}
      is(M"1000") {rspHdr.lowerAddr := (hdr.addr##B"11").asUInt.resized}
    }
    rspHdr.byteCnt := CountOne(hdr.firstBe##hdr.lastBe).resized
    when(hdr.firstBe === 0) {
      rspHdr.byteCnt := 1
    }
    rspHdr.len := hdr.len

    rsp << readDataStage.translateWith(rspData).haltWhen(readHaltRequest)
  
  }

  val write = new Area {
    val askWrite = demux.WriteCmd.haltWhen(writeHaltRequest).freeRun.fire
    val doWrite = askWrite
    val hdr = MemCmdHeader()
    hdr.assignFromBits(demux.WriteCmd.hdr)
  }


  // val debug = new Area {
  //   val errorOnReq = (readDataStage.first && readDataStage.last && readDataStage.fire)
  // }

  override def readAddress()  = read.hdr.addr << 2
  override def writeAddress() = write.hdr.addr << 2

  override def readHalt()  = readHaltRequest := True
  override def writeHalt() = writeHaltRequest := True

  override def busDataWidth   = 64 // bit
  override def wordAddressInc = 4 // byte

  override def writeByteEnable(): Bits = write.hdr.firstBe ## write.hdr.lastBe

  override def build(): Unit = {
    super.doNonStopWrite(demux.WriteCmd.data.resize(64))

    switch(writeAddress()) {
      for ((address, jobs) <- elementsPerAddress ) address match {
        case address : SingleMapping =>
          // assert(address.address % (bus.config.dataWidth/8) == 0)
          is(address.address) {
            doMappedWriteElements(jobs, write.askWrite, write.doWrite, demux.WriteCmd.data)
          }
        case _ =>
      }
    }

    for ((address, jobs) <- elementsPerAddress if !address.isInstanceOf[SingleMapping]) {
      when(address.hit(writeAddress())) {
        doMappedWriteElements(jobs, write.askWrite, write.doWrite, demux.WriteCmd.data)
      }
    }


    switch(readAddress()) {
      for ((address, jobs) <- elementsPerAddress) address match {
        case address : SingleMapping =>
          is(address.address) {
            doMappedReadElements(jobs, read.askRead, read.doRead, read.rspData.data)
          }
        case _ =>
      }
    }

    for ((address, jobs) <- elementsPerAddress if !address.isInstanceOf[SingleMapping]) {
      when(address.hit(readAddress())) {
        doMappedReadElements(jobs, read.askRead, read.doRead, read.rspData.data)
      }
    }
  }

}

// class PcieSlaveFactoryDut extends Component {
//   val rxReqConfig = TlpConfig(64/32, useData = true, useStrb = true)

//   val io = new Bundle {
//     val rxReq = slave(Stream Fragment(Tlp(rxReqConfig)))
//     val txCpl = master(Stream Fragment(Tlp(rxReqConfig)))
//   }
//   val factory = PcieSlaveFactory(io.rxReq, io.txCpl)

//   factory.createReadAndWrite(UInt(64 bits), 0x10000)
// }

// object PcieSlaveFactoryDut extends App {
//   SpinalVerilog(new PcieSlaveFactoryDut)
// }