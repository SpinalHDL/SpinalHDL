package spinal.lib.ethernet

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba4.axis._

import EthernetProtocolConstant._
import UserConfiguration._

case class HeaderRecognizerGenerics(
    DATA_WIDTH: Int = 256,
    DATA_BYTE_CNT: Int = 32,
    OCTETS: Int = 8,
    DATA_USE_TLAST: Boolean = true,
    DATA_USE_TUSER: Boolean = true,
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

    val metaOut = master Stream (MetaData())
    val dataAxisOut = master(Axi4Stream(headerAxisOutConfig))
  }

  val recognizerRunning = Reg(Bool()) init False
  val recognizerStart = recognizerRunning.rise()
  val (packetStream, packetReg) = StreamFork2(io.dataAxisIn)
  val packetStreamReg = packetReg m2sPipe() s2mPipe()
  val combineData = packetStream.data ## packetStreamReg.data

  val setMeta = recognizerStart & packetStream.fire

//  need redesign
  val restHeaderWidth = combineData.getWidth - HEADER_TOTAL_LENGTH * BYTE_WIDTH
  val splitHeader = combineData.sliceBy(List(ETH_HEADER_WIDTH, IP_HEADER_WIDTH, UDP_HEADER_WIDTH, restHeaderWidth))
  val (ethHeaderName, ethHeaderExtract) =
    EthernetHeader.unapply(splitHeader(0))
  val (ipv4HeaderName, ipv4HeaderExtract) =
    IPv4Header.unapply(splitHeader(1))
  val (udpHeaderName, udpHeaderExtract) =
    UDPHeader.unapply(splitHeader(2))

  val ethHeaderExtractReg = Array.tabulate(ethHeaderExtract.length) ( idx =>
    RegNextWhen(ethHeaderExtract(idx), setMeta)
  )
  val ipv4HeaderExtractReg = Array.tabulate(ipv4HeaderExtract.length) ( idx =>
    RegNextWhen(ipv4HeaderExtract(idx), setMeta)
    )
  val udpHeaderExtractReg = Array.tabulate(udpHeaderExtract.length) ( idx =>
    RegNextWhen(udpHeaderExtract(idx), setMeta)
  )

  val ethHeader = ethHeaderName.zip(ethHeaderExtract).toMap
  val ipv4Header = ipv4HeaderName.zip(ipv4HeaderExtract).toMap
  val udpHeader = udpHeaderName.zip(udpHeaderExtract).toMap
  val ethHeaderReg = ethHeaderName.zip(ethHeaderExtractReg).toMap
  val ipv4HeaderReg = ipv4HeaderName.zip(ipv4HeaderExtractReg).toMap
  val udpHeaderReg = udpHeaderName.zip(udpHeaderExtractReg).toMap

  val macAddrCorrectReg = Reg(Bool()) init False
  val ipAddrCorrectReg = Reg(Bool()) init False
  val isIpReg = Reg(Bool()) init False
  val isUdpReg = Reg(Bool()) init False
  val headerMatch = macAddrCorrectReg & ipAddrCorrectReg & isIpReg & isUdpReg

  val dataLen = Reg(UInt(IP_LENGTH_WIDTH bits)) init 0
  val shiftLen = Reg(UInt(log2Up(DATA_BYTE_CNT) bits)) init 0

  val packetLenMax =
    ((MTU + ETH_HEADER_LENGTH) / HeaderConfig.DATA_BYTE_CNT.toFloat).ceil.toInt
  val packetLenWidth = log2Up(packetLenMax)
  val packetLenReg = Reg(UInt(log2Up(packetLenMax) bits)) init 0

  val needOneMoreCycle = ((ipv4HeaderReg(IPv4Fields.ipLen).asUInt - (IP_HEADER_LENGTH + UDP_HEADER_LENGTH)) & (DATA_BYTE_CNT - 1)) =/= 0

  when(recognizerStart) {
    macAddrCorrectReg.setWhen(ethHeader(EthernetFields.dstMAC) === B"48'xfccffccffccf") // just use verification
    ipAddrCorrectReg.setWhen(ipv4Header(IPv4Fields.dstAddr) === B"32'xc0a80103") // just use verification
    isIpReg.setWhen(ethHeader(EthernetFields.ethType) === ETH_TYPE)
    isUdpReg.setWhen(ipv4Header(IPv4Fields.protocol) === PROTOCOL)
  }

  macAddrCorrectReg.clearWhen(packetStreamReg.lastFire)
  ipAddrCorrectReg.clearWhen(packetStreamReg.lastFire)
  isIpReg.clearWhen(packetStreamReg.lastFire)
  isUdpReg.clearWhen(packetStreamReg.lastFire)

  when(headerMatch) {
    dataLen := (ipv4HeaderReg(IPv4Fields.ipLen).asUInt - (IP_HEADER_LENGTH + UDP_HEADER_LENGTH)).resized
    shiftLen := DATA_BYTE_CNT - (HEADER_TOTAL_LENGTH % DATA_BYTE_CNT)
    packetLenReg := ((ipv4HeaderReg(IPv4Fields.ipLen).asUInt - (IP_HEADER_LENGTH + UDP_HEADER_LENGTH)) >> log2Up(DATA_BYTE_CNT)
      ).takeLow(packetLenWidth).asUInt -
      (needOneMoreCycle ? U(0, packetLenWidth bits) | U(1, packetLenWidth bits))
  }

  val metaCfg = Stream(MetaData())
  metaCfg.valid := RegNext(headerMatch.rise()) init False
  metaCfg.dataLen := dataLen.resize(metaCfg.dataLen.getWidth)
  metaCfg.srcMacAddr := ethHeaderReg(EthernetFields.srcMAC)
  metaCfg.srcIpAddr := ipv4HeaderReg(IPv4Fields.srcAddr)
  metaCfg.srcPort := udpHeaderReg(UDPFields.srcPort)
  metaCfg.dstPort := udpHeaderReg(UDPFields.dstPort)
  metaCfg.dstMacAddr := ethHeaderReg(EthernetFields.dstMAC)
  metaCfg.dstIpAddr := ipv4HeaderReg(IPv4Fields.dstAddr)

  io.metaOut << metaCfg

  when(packetStream.lastFire) {
    recognizerRunning.clear()
  } elsewhen (packetStream.fire) {
    recognizerRunning.set()
  }


  val dataStreamRegValid = (headerMatch && recognizerRunning) | headerMatch
  val dataStreamValid = (headerMatch && recognizerRunning)

  val dataStreamReg = packetStreamReg takeWhen (dataStreamRegValid) m2sPipe() s2mPipe()
  val dataStream = packetStream takeWhen (dataStreamValid) m2sPipe() s2mPipe()
  val invalidReg = Reg(Bool()) init False
  val withSub = dataStream.valid & dataStreamReg.valid
  val dataJoinStream =
    SubStreamJoin(dataStreamReg, dataStream, withSub) stage() throwWhen (invalidReg)

  val transactionCounter = new StreamTransactionCounter(packetLenWidth)
  transactionCounter.io.ctrlFire := RegNext(headerMatch.rise()) init False
  transactionCounter.io.targetFire := dataJoinStream.fire
  transactionCounter.io.count := packetLenReg // 0 until packetLen
  invalidReg := transactionCounter.io.done


  val maskStage = packetStream.clone()
  val mask =
    RegNextWhen(generateByteMask(shiftLen), headerMatch & dataStreamReg.fire) init 0

  maskStage.arbitrationFrom(dataJoinStream)
  maskStage.data := byteMaskData(~mask, dataJoinStream.payload._1.data) |
    byteMaskData(mask, dataJoinStream.payload._2.data)
  maskStage.keep := byteMaskData(~mask, dataJoinStream.payload._1.keep) |
    byteMaskData(mask, dataJoinStream.payload._2.keep)
  maskStage.last := transactionCounter.io.done

  val maskedStage = maskStage m2sPipe() s2mPipe()

  val shiftStage = maskedStage.clone()
  shiftStage.arbitrationFrom(maskedStage)
  shiftStage.data := rotateLeftByte(maskedStage.data, shiftLen)
  shiftStage.keep := rotateLeftBit(maskedStage.keep, shiftLen)
  shiftStage.user := 0
  shiftStage.last := maskedStage.last

  io.dataAxisOut << shiftStage
}
