package spinal.lib.ethernet

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.bus.amba4.axis._
import spinal.core.Mem
import ethernet.PacketMTUEnum
import spinal.core.internals.Operator
import spinal.lib.bus.amba4.axis.Axi4Stream.Axi4StreamBundle
import spinal.lib.fsm._

import java.util.Calendar
import scala.math._

case class TxGenerics(
    IP_ADDR_WIDTH: Int = 32,
    PORT_WIDTH: Int = 16,
    DATA_WIDTH: Int = 256,
    DATA_BYTE_CNT: Int = 32,
    OCTETS: Int = 8,
    DATA_USE_TLAST: Boolean = true,
    DATA_USE_TUSER: Boolean = true,
    DATA_USE_TKEEP: Boolean = true,
    DATA_USE_TSTRB: Boolean = false,
    DATA_TUSER_WIDTH: Int = 1,
    INPUT_BUFFER_DEPTH: Int = 256
)


class TxTop(
    txConfig: TxGenerics,
//    arpCacheConfig: ArpCacheGenerics,
    headerConfig: HeaderGeneratorGenerics
) extends Component {
  val dataAxisCfg = Axi4StreamConfig(
    dataWidth = txConfig.DATA_BYTE_CNT,
    userWidth = txConfig.DATA_TUSER_WIDTH,
    useStrb = txConfig.DATA_USE_TSTRB,
    useKeep = txConfig.DATA_USE_TKEEP,
    useLast = txConfig.DATA_USE_TLAST,
    useUser = txConfig.DATA_USE_TUSER
  )

  val io = new Bundle {
    val metaIn = slave Stream MetaInterface()

    val dataAxisIn = slave(Axi4Stream(dataAxisCfg))
    val dataAxisOut = master(Axi4Stream(dataAxisCfg))
  }

  val dataBuffered = io.dataAxisIn.queue(txConfig.INPUT_BUFFER_DEPTH)
  val metaBuffered = io.metaIn.queue(headerConfig.INPUT_BUFFER_DEPTH)

  val headerGenerator =
    new HeaderGenerator(headerConfig)
  headerGenerator.io.metaIn << metaBuffered
  val headerBuffered = headerGenerator.io.headerAxisOut.queue(16)
  val forkedStream = StreamFork(dataBuffered, 2)

  val dataBufferedReg = forkedStream(0).stage()

  val streamMuxReg = Reg(UInt(1 bits)) init 0
  val streamJoinReg0 = Reg(Bool()) init False
  val streamJoinReg1 = Reg(Bool()) init False

  val invalidData = Bool()

  val selectedStream =
    StreamMux(
      streamMuxReg,
      Vec(headerBuffered, dataBufferedReg)
    ) s2mPipe () throwWhen (invalidData)

  val joinedStream = Axi4StreamConditionalJoin(
    selectedStream,
    forkedStream(1),
    streamJoinReg0,
    streamJoinReg1
  ) m2sPipe ()

  when(forkedStream(1).lastFire) {
    streamJoinReg1 := False
  } elsewhen (selectedStream.fire & !streamMuxReg.asBool) {
    streamJoinReg1 := True
  }

  when(headerBuffered.lastFire) {
    streamMuxReg := 1
  } elsewhen (dataBufferedReg.lastFire | invalidData) {
    streamMuxReg := 0
  }

  when(forkedStream(1).lastFire) {
    streamJoinReg0 := True
  } elsewhen (selectedStream.lastFire | invalidData) {
    streamJoinReg0 := False
  }

  //  redesign
  def rotateLeftByte(data: Bits, bias: UInt): Bits = {
    val result = cloneOf(data)
    val byteNum: Int = data.getWidth / txConfig.OCTETS
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
    val dataWidth = txConfig.DATA_BYTE_CNT
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
    val res = Bits(txConfig.DATA_BYTE_CNT bits)
    switch(len) {
      for (idx <- 0 until txConfig.DATA_BYTE_CNT) {
        if (idx == 0) {
          is(idx) {
            res := Bits(txConfig.DATA_BYTE_CNT bits).setAll()
          }
        } else {
          is(idx) {
            res := B(
              txConfig.DATA_BYTE_CNT bits,
              (txConfig.DATA_BYTE_CNT - 1 downto txConfig.DATA_BYTE_CNT - idx) -> true,
              default -> false
            )
          }
        }
      }
    }
    res
  }

  val mask = Reg(Bits(txConfig.DATA_BYTE_CNT bits)) init 0
  val shiftLen = Reg(UInt(log2Up(txConfig.DATA_BYTE_CNT) bits)) init 0
  val packetLen = selectedStream.user(19 downto 14).asUInt

  val maskStage = dataBufferedReg.clone()
  val cntTrigger = selectedStream.fire && (selectedStream.user.takeLow(
    8
  ) === B"8'xA5") && (selectedStream.user
    .takeHigh(8) === B"8'x5A") && selectedStream.user(13)

  when(selectedStream.fire) {
    when(
      (selectedStream.user.takeLow(8) === B"8'xA5") && (selectedStream.user
        .takeHigh(8) === B"8'x5A")
    ) {
      mask := generateByteMask(selectedStream.user(12 downto 8).asUInt)
      shiftLen := selectedStream.user(12 downto 8).asUInt
    }
  }

  val transactionCounter = new StreamTransactionCounter(6)
  transactionCounter.io.ctrlFire := cntTrigger
  transactionCounter.io.targetFire := maskStage.fire
  transactionCounter.io.count := packetLen

  invalidData := transactionCounter.io.done & streamJoinReg0

  maskStage.arbitrationFrom(joinedStream)
  maskStage.data := byteMaskData(
    ~mask,
    joinedStream.payload._1.data
  ) | byteMaskData(mask, joinedStream.payload._2.data)
  maskStage.keep := byteMaskData(
    ~mask,
    joinedStream.payload._1.keep
  ) | byteMaskData(mask, joinedStream.payload._2.keep)
  maskStage.user := 0
  maskStage.last := transactionCounter.io.last

  val shiftStage = maskStage.clone()
  shiftStage.arbitrationFrom(maskStage)
  shiftStage.data := rotateLeftByte(maskStage.data, shiftLen)
  shiftStage.keep := rotateLeftBit(maskStage.keep, shiftLen)
  shiftStage.user := 0
  shiftStage.last := maskStage.last

  io.dataAxisOut <-< shiftStage
}
