package spinal.lib.ethernet

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba4.axis.{Axi4Stream, Axi4StreamConfig}
import spinal.lib.ethernet.EthernetProtocolConstant._

case class HeaderGeneratorGenerics(
    DATA_WIDTH: Int = DATA_WIDTH,
    DATA_BYTE_CNT: Int = DATA_BYTE_CNT,
    DATA_USE_TLAST: Boolean = true,
    DATA_USE_TUSER: Boolean = true,
    DATA_USE_TKEEP: Boolean = true,
    DATA_USE_TSTRB: Boolean = false,
    DATA_TUSER_WIDTH: Int = 1,
    INPUT_BUFFER_DEPTH: Int = 4
)
class HeaderGenerator(
    HeaderGeneratorConfig: HeaderGeneratorGenerics
//    arpCacheConfig: ArpCacheGenerics
) extends Component {
  val headerAxisOutConfig = Axi4StreamConfig(
    dataWidth = HeaderGeneratorConfig.DATA_BYTE_CNT,
    userWidth = HeaderGeneratorConfig.DATA_TUSER_WIDTH,
    useKeep = HeaderGeneratorConfig.DATA_USE_TKEEP,
    useLast = HeaderGeneratorConfig.DATA_USE_TLAST,
    useUser = HeaderGeneratorConfig.DATA_USE_TUSER
  )
  val io = new Bundle {
    val metaIn = slave Stream MetaInterface()

    val headerAxisOut = master(Axi4Stream(headerAxisOutConfig))
  }
  val ipLenReg = Reg(UInt(IP_LENGTH_WIDTH bits)) init 0
  val udpLenReg = Reg(UInt(UDP_LENGTH_WIDTH bits)) init 0
  val ipIdReg = Reg(UInt(IDENTIFICATION_WIDTH bits)) init 0
  val ipChecksumReg = Reg(UInt(IP_HEADER_CHECKSUM_WIDTH bits)) init 0
  val generateDone = Bool()

  val packetLenMax =
    ((MTU + ETH_HEADER_LENGTH) / DATA_BYTE_CNT.toFloat).ceil.toInt
  val packetLen = Reg(UInt(log2Up(packetLenMax) bits)) init 0

  val dataLoaded = Reg(Bool()) init False
  val metaRegs = Reg(MetaInterface())

  val sendHeaderCycle = (HEADER_TOTAL_LENGTH / DATA_BYTE_CNT.toFloat).ceil.toInt
  val sendHeaderLastLeft = HEADER_TOTAL_LENGTH % DATA_BYTE_CNT
  val sendingCnt = Counter(sendHeaderCycle)
  val needFragment = Reg(Bool()) init False

  val metaSetValid = !dataLoaded | generateDone
  io.metaIn.ready := True & metaSetValid
  when(io.metaIn.fire) {
    dataLoaded := True
  } elsewhen (generateDone) {
    dataLoaded := False
  }

  when(io.metaIn.fire) {
    metaRegs.srcPort := io.metaIn.srcPort
    metaRegs.dstPort := io.metaIn.dstPort
    metaRegs.IpAddr := io.metaIn.IpAddr
    metaRegs.MacAddr := io.metaIn.MacAddr
    when(io.metaIn.dataLen > MAX_DATA_NUM) {
      metaRegs.dataLen := io.metaIn.dataLen - (MTU - IP_HEADER_LENGTH - UDP_HEADER_LENGTH)
      needFragment := True
      ipLenReg := IP_LENGTH_MAX
      udpLenReg := UDP_LENGTH_MAX
      packetLen := packetLenMax - 1
    } otherwise {
      needFragment := False
      ipLenReg := io.metaIn.dataLen.resize(
        IP_LENGTH_WIDTH
      ) + IP_HEADER_LENGTH + UDP_HEADER_LENGTH
      udpLenReg := io.metaIn.dataLen.resize(IP_LENGTH_WIDTH) + UDP_HEADER_LENGTH
      packetLen := ((((io.metaIn.dataLen + HEADER_TOTAL_LENGTH) % HeaderGeneratorConfig.DATA_BYTE_CNT) =/= 0) ? U(
        1,
        log2Up(packetLenMax) bits
      ) | U(
        0,
        log2Up(packetLenMax) bits
      )) + ((io.metaIn.dataLen + HEADER_TOTAL_LENGTH) >> 5)
        .takeLow(log2Up(packetLenMax))
        .asUInt - 1
    }
  } elsewhen (io.headerAxisOut.lastFire & needFragment) {
    when(metaRegs.dataLen > MAX_DATA_NUM) {
      needFragment := True
      ipLenReg := IP_LENGTH_MAX
      metaRegs.dataLen := metaRegs.dataLen - MAX_DATA_NUM
      packetLen := packetLenMax - 1
    } otherwise {
      needFragment := False
      ipLenReg := io.metaIn.dataLen.resize(
        IP_LENGTH_WIDTH
      ) + IP_HEADER_LENGTH + UDP_HEADER_LENGTH
      udpLenReg := io.metaIn.dataLen.resize(IP_LENGTH_WIDTH) + UDP_HEADER_LENGTH
      packetLen := ((((io.metaIn.dataLen + HEADER_TOTAL_LENGTH) % HeaderGeneratorConfig.DATA_BYTE_CNT) =/= 0) ? U(
        1,
        log2Up(packetLenMax) bits
      ) | U(
        0,
        log2Up(packetLenMax) bits
      )) + ((io.metaIn.dataLen + HEADER_TOTAL_LENGTH) >> 5)
        .takeLow(log2Up(packetLenMax))
        .asUInt - 1
    }
  }

  when(generateDone) {
    ipIdReg := ipIdReg + 1
  }

  val ethHeader = EthernetHeader(
    Array(
      metaRegs.MacAddr,
      SRC_MAC_ADDR,
      ETH_TYPE
    )
  )

  val ipv4Header = IPv4Header(
    Array(
      IP_VERSION,
      IHL,
      DSCP,
      ECN,
      ipLenReg.asBits,
      ipIdReg.asBits,
      FLAGS,
      FRAGMENT_OFFSET,
      TTL,
      PROTOCOL,
      ipChecksumReg.asBits,
      SRC_IP_ADDR,
      metaRegs.IpAddr
    )
  )
  val udpHeader = UDPHeader(
    Array(
      metaRegs.srcPort,
      metaRegs.dstPort,
      udpLenReg.asBits,
      UDP_CHECKSUM
    )
  )

  val ethIpUdpHeader = generate(Seq(ethHeader, ipv4Header, udpHeader))
  val ethIpHeader = generate(Seq(ethHeader, ipv4Header))
  val udpSent = Reg(Bool()) init False

  io.headerAxisOut.data := sendingCnt.mux(
    0 -> ethIpUdpHeader(sendingCnt),
    1 -> ethIpUdpHeader(sendingCnt).rotateRight(sendHeaderLastLeft * BYTE_WIDTH)
  )

  //  32'xffff_ffff or 32'xffc0_0000
  io.headerAxisOut.keep := sendingCnt.mux(
    0 -> Bits(HeaderGeneratorConfig.DATA_BYTE_CNT bits).setAll(),
    1 -> B(
      HeaderGeneratorConfig.DATA_BYTE_CNT bits,
      (HeaderGeneratorConfig.DATA_BYTE_CNT - 1 downto HeaderGeneratorConfig.DATA_BYTE_CNT - sendHeaderLastLeft) -> true,
      default -> false
    )
  )

  io.headerAxisOut.user := sendingCnt.mux(
    0 -> generateControlSignal(0, True, packetLen),
    1 -> generateControlSignal(sendHeaderLastLeft, False, packetLen)
  )

  io.headerAxisOut.valid := dataLoaded
  when(io.headerAxisOut.fire) {
    sendingCnt.increment()
    when(sendingCnt.lsb) {
      io.headerAxisOut.last := True
    } otherwise {
      io.headerAxisOut.last := False
    }
    when(sendingCnt.lsb & needFragment) {
      udpSent := True
      generateDone := False
    } elsewhen (sendingCnt.lsb & !needFragment) {
      generateDone := True
      udpSent := False
    } otherwise {
      generateDone := False
    }
  } otherwise {
    io.headerAxisOut.last := False
    generateDone := False
  }

  def getHeaderWidth(header: Seq[Array[Bits]]): Int = {
    var len: Int = 0
    header.foreach { protocol =>
      protocol.foreach { data =>
        len += data.getBitsWidth
      }
    }
    len
  }

  def mergeHeader(header: Seq[Array[Bits]]): Vec[Bits] = {
    val headerWidth: Int = getHeaderWidth(header)

    val tmp = header.flatten
      .reduce(_ ## _)
      .subdivideIn(BYTE_WIDTH bits)
      .reduce(_ ## _)
      .resize(
        ((headerWidth / HeaderGeneratorConfig.DATA_WIDTH.toFloat).ceil * HeaderGeneratorConfig.DATA_WIDTH).toInt bits
      )
      .subdivideIn(HeaderGeneratorConfig.DATA_WIDTH bits)

    tmp
  }

  def generate(headers: Seq[Array[Bits]]): Vec[Bits] = {
    val header = mergeHeader(headers)
    header
  }

  /**   31  24      19          13     12      7    0
    *   ┌────┬──────┬───────────┬──────┬───────┬────┐
    *   │0x5A│      │ packetLen │ LSel │ Shift │0xA5│
    *   └────┴──────┴───────────┴──────┴───────┴────┘
    */
  def generateControlSignal(
      shiftLen: Int,
      lastSelect: Bool,
      packetLen: UInt
  ): Bits = {
    val controlHeader = B"8'xa5"
    val controlTail = B"8'x5a"
    val shiftBits = B(shiftLen, log2Up(DATA_BYTE_CNT) bits)
    val ret = controlTail ## B(0, 4 bits) ##
      packetLen ## lastSelect ## shiftBits ## controlHeader
    ret
  }

}
