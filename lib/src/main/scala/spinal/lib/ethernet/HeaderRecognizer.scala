package spinal.lib.ethernet

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba4.axis._

import scala.collection.mutable

case class HeaderRecognizerGenerics(
    SRC_IP_ADDR: String = "c0a80104",
    SRC_MAC_ADDR: String = "fccffccffccf",
    DATA_WIDTH: Int = 256,
    DATA_BYTE_CNT: Int = 32,
    OCTETS: Int = 8,
    DATA_USE_TLAST: Boolean = true,
    DATA_USE_TUSER: Boolean = false,
    DATA_USE_TKEEP: Boolean = true,
    DATA_USE_TSTRB: Boolean = false,
    DATA_TUSER_WIDTH: Int = 1,
    INPUT_BUFFER_DEPTH: Int = 4
)

class HeaderRecognizer(
    HeaderConfig: HeaderRecognizerGenerics
) extends Component {
  val headerAxisOutConfig = Axi4StreamConfig(
    dataWidth = HeaderConfig.DATA_BYTE_CNT,
    userWidth = HeaderConfig.DATA_TUSER_WIDTH,
    useKeep = HeaderConfig.DATA_USE_TKEEP,
    useLast = HeaderConfig.DATA_USE_TLAST,
    useUser = HeaderConfig.DATA_USE_TUSER
  )
  val io = new Bundle {
    val dataAxisIn = slave(Axi4Stream(headerAxisOutConfig))

    val metaOut = master Stream MetaInterface()
    val dataAxisOut = master(Axi4Stream(headerAxisOutConfig))
  }

  val recognizerRunning = Reg(Bool()) init False
  val recognizerStart = recognizerRunning.rise()
  val (packetStream, packetReg) = StreamFork2(io.dataAxisIn)
  val packetStreamReg = packetReg stage ()
  val combineData = packetStream.payload.data ## packetStreamReg.payload.data

  val setMeta = recognizerStart & packetStream.fire

//  need redesign
  val (ethHeaderName, ethHeaderExtract) =
    EthernetHeader.unapply(combineData.takeLow(112))
  val (ipv4HeaderName, ipv4HeaderExtract) =
    IPv4Header.unapply(combineData.dropLow(112).takeLow(160))
  val (udpHeaderName, udpHeaderExtract) =
    UDPHeader.unapply(combineData.dropLow(272).takeLow(64))

  val ethHeaderExtractReg = Array.tabulate(ethHeaderExtract.length) { idx =>
    val dataReg: Bits = RegNextWhen(ethHeaderExtract(idx), setMeta)
    dataReg
  }
  val ipv4HeaderExtractReg = Array.tabulate(ipv4HeaderExtract.length) { idx =>
    val dataReg: Bits = RegNextWhen(ipv4HeaderExtract(idx), setMeta)
    dataReg
  }
  val udpHeaderExtractReg = Array.tabulate(udpHeaderExtract.length) { idx =>
    val dataReg: Bits = RegNextWhen(udpHeaderExtract(idx), setMeta)
    dataReg
  }

  val ethHeader = ethHeaderName.zip(ethHeaderExtract).toMap
  val ipv4Header = ipv4HeaderName.zip(ipv4HeaderExtract).toMap
  val udpHeader = udpHeaderName.zip(udpHeaderExtract).toMap
  val ethHeaderReg = ethHeaderName.zip(ethHeaderExtractReg).toMap
  val ipv4HeaderReg = ipv4HeaderName.zip(ipv4HeaderExtractReg).toMap
  val udpHeaderReg = udpHeaderName.zip(udpHeaderExtractReg).toMap

  val addrCorrect = Reg(Bool()) init False
  val checked = Reg(Bool()) init False
  val dataLen = Reg(UInt(13 bits)) init 0
  val shiftLen = Reg(UInt(5 bits)) init 0
  val isUdp = Reg(Bool()) init False

  val packetLenMax =
    ((1500 + 14) / HeaderConfig.DATA_BYTE_CNT.toFloat).ceil.toInt
  val packetLen = Reg(UInt(log2Up(packetLenMax) bits)) init 0

  when(recognizerStart) {
    checked.set()
    when(ethHeader("dstMAC") === HeaderConfig.SRC_MAC_ADDR.asHex) {
      when(ipv4Header("dstAddr") === HeaderConfig.SRC_IP_ADDR.asHex) {
        when(ethHeader("ethType") === B"16'x08_00") {
          when(ipv4Header("protocol") === B"8'x11") {
            isUdp := True
            when(
              ipv4Header("flags") === B"3'b0" && ipv4Header(
                "fragmentOffset"
              ) === B"13'b0"
            ) {
//              val name instead para
              dataLen := (ipv4Header("ipLen").asUInt - 28).resized
              shiftLen := 22
              packetLen := ((((ipv4Header(
                "ipLen"
              ).asUInt + 14) % HeaderConfig.DATA_BYTE_CNT) =/= 0) ? U(
                1,
                log2Up(packetLenMax) bits
              ) | U(0, log2Up(packetLenMax) bits)) + ((ipv4Header(
                "ipLen"
              ).asUInt + 14) >> 5)
                .takeLow(log2Up(packetLenMax))
                .asUInt - 1
            } // TODO: Fragment not support
          }
        }
        addrCorrect.set()
      } otherwise {
        addrCorrect.clear()
      }
    } otherwise {
      addrCorrect.clear()
    }
  } elsewhen (packetStream.lastFire) {
    checked.clear()
  } elsewhen (io.metaOut.fire) {
    isUdp.clear()
  }

  val metaCfg = Stream(MetaInterface())

  metaCfg.valid := True & isUdp.rise()

  metaCfg.payload.dataLen := dataLen
  metaCfg.payload.MacAddr := ethHeaderReg("srcMAC")
  metaCfg.payload.IpAddr := ipv4HeaderReg("srcAddr")
  metaCfg.payload.srcPort := udpHeaderReg("srcPort")
  metaCfg.payload.dstPort := udpHeaderReg("dstPort")

  io.metaOut << metaCfg.stage()

  when(packetStream.lastFire) {
    recognizerRunning.clear()
  } elsewhen (packetStream.fire) {
    recognizerRunning.set()
  }

  def rotateLeftByte(data: Bits, bias: UInt): Bits = {
    val result = cloneOf(data)
    val byteNum: Int = data.getWidth / HeaderConfig.OCTETS
    switch(bias) {
      for (idx <- 0 until byteNum) {
        is(idx) {
          result := data.takeLow((byteNum - idx) * 8) ## data.takeHigh(idx * 8)
        }
      }
    }
    result
  }

  def rotateLeftBit(data: Bits, bias: UInt): Bits = {
    val result = cloneOf(data)
    val bitWidth = data.getWidth
    switch(bias) {
      for (idx <- 0 until bitWidth) {
        is(idx) {
          result := data.takeLow(bitWidth - idx) ## data.takeHigh(idx)
        }
      }
    }
    result
  }

  def byteMaskData(byteMask: Bits, data: Bits): Bits = {
    val dataWidth = HeaderConfig.DATA_BYTE_CNT
    val maskWidth = byteMask.getWidth
    val sliceWidth = data.getWidth / dataWidth
    require(
      maskWidth == dataWidth,
      s"ByteMaskData maskWidth${maskWidth} != dataWidth${dataWidth}"
    )
    val spiltAsSlices = data.subdivideIn(maskWidth slices)
    val arrMaskedByte = Array.tabulate(spiltAsSlices.length) { idx =>
      byteMask(idx) ? B(0, sliceWidth bits) | spiltAsSlices(idx)
    }
    val maskedData = arrMaskedByte.reverse.reduceLeft(_ ## _)
    maskedData
  }

  def generateByteMask(len: UInt): Bits = {
    val res = Bits(HeaderConfig.DATA_BYTE_CNT bits)
    switch(len) {
      for (idx <- 0 until HeaderConfig.DATA_BYTE_CNT) {
        if (idx == 0) {
          is(idx) {
            res := Bits(HeaderConfig.DATA_BYTE_CNT bits).setAll()
          }
        } else {
          is(idx) {
            res := B(
              HeaderConfig.DATA_BYTE_CNT bits,
              (HeaderConfig.DATA_BYTE_CNT - 1 downto HeaderConfig.DATA_BYTE_CNT - idx) -> true,
              default -> false
            )
          }
        }
      }
    }
    res
  }

  val dataStreamRegValid = (addrCorrect && checked) | addrCorrect
  val dataStreamValid = (addrCorrect && checked)

  val dataStreamReg = packetStreamReg takeWhen (dataStreamRegValid) stage ()
  val dataStream = packetStream takeWhen (dataStreamValid) stage ()

  val dataJoinStream =
    Axi4StreamConditionalJoin(dataStreamReg, dataStream, True, True) stage ()
  val maskStage = packetStream.clone()
  val mask =
    RegNextWhen(generateByteMask(shiftLen), isUdp & dataStreamReg.fire) init 0

  maskStage.arbitrationFrom(dataJoinStream)
  maskStage.data := byteMaskData(
    ~mask,
    dataJoinStream.payload._1.data
  ) | byteMaskData(mask, dataJoinStream.payload._2.data)
  maskStage.keep := byteMaskData(
    ~mask,
    dataJoinStream.payload._1.keep
  ) | byteMaskData(mask, dataJoinStream.payload._2.keep)
  maskStage.last := dataJoinStream.payload._1.last

  val shiftStage = maskStage.clone()
  shiftStage.arbitrationFrom(maskStage)
  shiftStage.data := rotateLeftByte(maskStage.data, shiftLen)
  shiftStage.keep := rotateLeftBit(maskStage.keep, shiftLen)
  shiftStage.last := maskStage.last

  io.dataAxisOut <-< shiftStage
}
