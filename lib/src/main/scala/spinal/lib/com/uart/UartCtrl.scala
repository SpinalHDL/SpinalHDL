package spinal.lib.com.uart

import UartParityType._
import UartStopType._
import spinal.core._
import spinal.lib.FragmentToBitsStates._
import spinal.lib._
import spinal.lib.bus.misc.{BusSlaveFactoryAddressWrapper, BusSlaveFactory}

//All construction parameters of the UartCtrl
case class UartCtrlGenerics( dataWidthMax: Int = 8,
                             clockDividerWidth: Int = 20, // !! baudrate = Fclk / (rxSamplePerBit*clockDividerWidth) !!
                             preSamplingSize: Int = 1,
                             samplingSize: Int = 5,
                             postSamplingSize: Int = 2,
                             ctsGen : Boolean = false,
                             rtsGen : Boolean = false) {
  val rxSamplePerBit = preSamplingSize + samplingSize + postSamplingSize
  if ((samplingSize % 2) == 0)
    SpinalWarning(s"It's not nice to have a even samplingSize value at ${ScalaLocated.short} (because of the majority vote)")
}


case class UartCtrlFrameConfig(g: UartCtrlGenerics) extends Bundle {
  val dataLength = UInt(log2Up(g.dataWidthMax) bits) //Bit count = dataLength + 1
  val stop       = UartStopType()
  val parity     = UartParityType()
}

case class UartCtrlConfig(g: UartCtrlGenerics) extends Bundle {
  val frame        = UartCtrlFrameConfig(g)
  val clockDivider = UInt (g.clockDividerWidth bit) //see UartCtrlGenerics.clockDividerWidth for calculation

  def setClockDivider(baudrate : HertzNumber,clkFrequency : HertzNumber = ClockDomain.current.frequency.getValue) : Unit = {
    clockDivider := (clkFrequency / baudrate / g.rxSamplePerBit).setScale(0, BigDecimal.RoundingMode.HALF_DOWN).toBigInt - 1
  }
}

class UartCtrlIo(g : UartCtrlGenerics) extends Bundle {
  val config = in(UartCtrlConfig(g))
  val write  = slave(Stream(Bits(g.dataWidthMax bit)))
  val read   = master(Stream(Bits(g.dataWidthMax bit)))
  val uart   = master(Uart())
  val readError = out Bool()
  val writeBreak = in Bool()
  val readBreak = out Bool()
}


class UartCtrl(g : UartCtrlGenerics = UartCtrlGenerics()) extends Component {
  val io = new UartCtrlIo(g)
  val tx = new UartCtrlTx(g)
  val rx = new UartCtrlRx(g)

  //Clock divider used by RX and TX
  val clockDivider = new Area {
    val counter = Reg(UInt(g.clockDividerWidth bits)) init(0)
    val tick = counter === 0

    counter := counter - 1
    when(tick) {
      counter := io.config.clockDivider
    }
  }

  tx.io.samplingTick := clockDivider.tick
  rx.io.samplingTick := clockDivider.tick

  tx.io.configFrame := io.config.frame
  rx.io.configFrame := io.config.frame

  tx.io.write << io.write.throwWhen(rx.io.break)
  rx.io.read >> io.read

  io.uart.txd <> tx.io.txd
  io.uart.rxd <> rx.io.rxd

  io.readError := rx.io.error
  tx.io.cts := (if(g.ctsGen) BufferCC(io.uart.cts) else False)
  if(g.rtsGen) io.uart.rts := rx.io.rts
  io.readBreak := rx.io.break
  tx.io.break := io.writeBreak


  def driveFrom(busCtrl : BusSlaveFactory,config : UartCtrlMemoryMappedConfig,baseAddress : Int = 0) = new Area {
    require(busCtrl.busDataWidth == 16 || busCtrl.busDataWidth == 32)
    val busCtrlWrapped = new BusSlaveFactoryAddressWrapper(busCtrl,baseAddress)
    //Manage config
    val uartConfigReg = Reg(io.config)
    uartConfigReg.clockDivider init(0)
    if(config.initConfig != null)config.initConfig.initReg(uartConfigReg)
    if(config.busCanWriteClockDividerConfig)
      busCtrlWrapped.writeMultiWord(uartConfigReg.clockDivider,address = 8)
    else
      uartConfigReg.clockDivider.allowUnsetRegToAvoidLatch
    if(config.busCanWriteFrameConfig){
      busCtrlWrapped.write(uartConfigReg.frame.dataLength,address = 12,bitOffset = 0)
      busCtrlWrapped.write(uartConfigReg.frame.parity,address = 12,bitOffset = 8)
      busCtrl.busDataWidth match {
        case 16 => busCtrlWrapped.write(uartConfigReg.frame.stop,address = 14,bitOffset = 0)
        case 32 => busCtrlWrapped.write(uartConfigReg.frame.stop,address = 12,bitOffset = 16)
      }
    }else{
      uartConfigReg.frame.allowUnsetRegToAvoidLatch
    }
    io.config := uartConfigReg

    def sat255 (that : UInt) = if(widthOf(that) > 8) that.min(255).resize(8 bits) else that
    //manage TX
    val write = new Area {
      val streamUnbuffered = busCtrlWrapped.createAndDriveFlow(Bits(g.dataWidthMax bits), address = 0).toStream
      val (stream, fifoOccupancy) = streamUnbuffered.queueWithOccupancy(config.txFifoDepth)
      io.write << stream
      busCtrl.busDataWidth match {
        case 16 => busCtrlWrapped.read(sat255(config.txFifoDepth - fifoOccupancy),address = 6,bitOffset = 0)
        case 32 => busCtrlWrapped.read(sat255(config.txFifoDepth - fifoOccupancy),address = 4,bitOffset = 16)
      }

      streamUnbuffered.ready.allowPruning()

      busCtrlWrapped.read(stream.valid, address = 4, 15)
    }

    //manage RX
    val read = new Area {
      val (stream, fifoOccupancy) = io.read.queueWithOccupancy(config.rxFifoDepth)
      val streamBreaked = stream.throwWhen(io.readBreak)
      busCtrl.busDataWidth match {
        case 16 =>
          busCtrlWrapped.readStreamNonBlocking(streamBreaked, address = 0, validBitOffset = 15, payloadBitOffset = 0)
          busCtrlWrapped.read(sat255(fifoOccupancy),address = 6, 8)
        case 32 =>
          busCtrlWrapped.readStreamNonBlocking(streamBreaked, address = 0, validBitOffset = 16, payloadBitOffset = 0)
          busCtrlWrapped.read(sat255(fifoOccupancy),address = 4, 24)
      }
      def genCTS(freeThreshold : Int) = RegNext(fifoOccupancy <= config.rxFifoDepth - freeThreshold) init(False)  // freeThreshold => how many remaining space should be in the fifo before allowing transfer
    }

    //manage interrupts
    val interruptCtrl = new Area {
      val writeIntEnable = busCtrlWrapped.createReadAndWrite(Bool, address = 4, 0) init(False)
      val readIntEnable  = busCtrlWrapped.createReadAndWrite(Bool, address = 4, 1) init(False)
      val readInt   = readIntEnable  &  read.streamBreaked.valid
      val writeInt  = writeIntEnable & !write.stream.valid
      val interrupt = readInt || writeInt
      busCtrlWrapped.read(writeInt, address = 4, 8)
      busCtrlWrapped.read(readInt , address = 4, 9)
    }

    val misc = new Area{
      val readError = busCtrlWrapped.createReadAndClearOnSet(Bool, 0x10, 0) init(False) setWhen(io.readError)
      val readOverflowError = busCtrlWrapped.createReadAndClearOnSet(Bool, 0x10, 1) init(False) setWhen(io.read.isStall)
      busCtrlWrapped.read(io.readBreak, 0x10, 8)
      val breakDetected = RegInit(False) setWhen(io.readBreak.rise())
      busCtrlWrapped.read(breakDetected, 0x10, 9)
      busCtrlWrapped.clearOnSet(breakDetected, 0x10, 9)
      val doBreak = RegInit(False)
      busCtrlWrapped.setOnSet(doBreak, 0x10, 10)
      busCtrlWrapped.clearOnSet(doBreak, 0x10, 11)
      io.writeBreak := doBreak
    }
  }

  //Legacy wrappers
  def driveFrom16(busCtrl : BusSlaveFactory,config : UartCtrlMemoryMappedConfig,baseAddress : Int = 0) = {
    require(busCtrl.busDataWidth == 16)
    driveFrom(busCtrl,config,baseAddress)
  }
  //Legacy wrappers
  def driveFrom32(busCtrl : BusSlaveFactory,config : UartCtrlMemoryMappedConfig,baseAddress : Int = 0) = {
    require(busCtrl.busDataWidth == 32)
    driveFrom(busCtrl,config,baseAddress)
  }
}

object UartCtrl {
  def apply(config: UartCtrlInitConfig, readonly: Boolean = false): UartCtrl = {
    val uartCtrl = new UartCtrl()
    uartCtrl.io.config.setClockDivider(config.baudrate Hz)
    uartCtrl.io.config.frame.dataLength := config.dataLength  //8 bits
    uartCtrl.io.config.frame.parity := config.parity
    uartCtrl.io.config.frame.stop := config.stop
    uartCtrl.io.writeBreak := False
    if (readonly) {
      uartCtrl.io.write.valid := False
      uartCtrl.io.write.payload := B(0)
    }
    uartCtrl
  }
}

case class UartCtrlInitConfig(
  baudrate : Int = 0,
  dataLength : Int = 0,
  parity : UartParityType.E = null,
  stop : UartStopType.E = null
){
  def initReg(reg : UartCtrlConfig): Unit ={
    require(reg.isReg)
    if(baudrate != 0) reg.clockDivider init((ClockDomain.current.frequency.getValue / baudrate / reg.g.rxSamplePerBit).toInt-1)
    if(dataLength != 0) reg.frame.dataLength init(dataLength)
    if(parity != null) reg.frame.parity init(parity)
    if(stop != null) reg.frame.stop init(stop)
  }
}

object UartCtrlMemoryMappedConfig{
  def apply(baudrate: Int,
            txFifoDepth: Int,
            rxFifoDepth: Int,
            writeableConfig : Boolean,
            clockDividerWidth : Int) : UartCtrlMemoryMappedConfig = UartCtrlMemoryMappedConfig(
    uartCtrlConfig = UartCtrlGenerics(
      dataWidthMax = 8,
      clockDividerWidth = clockDividerWidth,
      preSamplingSize = 1,
      samplingSize = 3,
      postSamplingSize = 1
    ),
    initConfig = UartCtrlInitConfig(
      baudrate = baudrate,
      dataLength = 7, //7 => 8 bits
      parity = UartParityType.NONE,
      stop = UartStopType.ONE
    ),
    busCanWriteClockDividerConfig = writeableConfig,
    busCanWriteFrameConfig = writeableConfig,
    txFifoDepth = txFifoDepth,
    rxFifoDepth = rxFifoDepth
  )

  def apply(baudrate: Int,
            txFifoDepth: Int,
            rxFifoDepth: Int) : UartCtrlMemoryMappedConfig = apply(
    baudrate = baudrate,
    txFifoDepth = txFifoDepth,
    rxFifoDepth = rxFifoDepth,
    writeableConfig = false,
    clockDividerWidth = 12
  )
}

case class UartCtrlMemoryMappedConfig(
  uartCtrlConfig : UartCtrlGenerics,
  initConfig : UartCtrlInitConfig = null,
  busCanWriteClockDividerConfig : Boolean = true,
  busCanWriteFrameConfig : Boolean = true,
  txFifoDepth : Int = 32,
  rxFifoDepth : Int = 32
){
  require(txFifoDepth >= 1)
  require(rxFifoDepth >= 1)
}



class UartCtrlUsageExample extends Component{
  val io = new Bundle{
    val uart = master(Uart())
    val switchs = in Bits(8 bits)
    val leds = out Bits(8 bits)
  }

  val uartCtrl: UartCtrl = UartCtrl(
    config = UartCtrlInitConfig(
      baudrate = 921600,
      dataLength = 7,  // 8 bits
      parity = UartParityType.NONE,
      stop = UartStopType.ONE
    )
  )
  uartCtrl.io.uart <> io.uart

  //Assign io.led with a register loaded each time a byte is received
  io.leds := uartCtrl.io.read.toFlow.toReg()

  //Write the value of switch on the uart each 4000 cycles
  val write = Stream(Bits(8 bits))
  write.valid := CounterFreeRun(2000).willOverflow
  write.payload := io.switchs
  write >-> uartCtrl.io.write

  //Write the 0x55 and then the value of switch on the uart each 4000 cycles
//  val write = Stream(Fragment(Bits(8 bits)))
//  write.valid := CounterFreeRun(4000).willOverflow
//  write.fragment := io.switchs
//  write.last := True
//  write.m2sPipe().insertHeader(0x55).toStreamOfFragment >> uartCtrl.io.write
}


object UartCtrlUsageExample{
  def main(args: Array[String]) {
    SpinalConfig(
      mode = VHDL,
      defaultClockDomainFrequency=FixedFrequency(50 MHz)
    ).generate(new UartCtrlUsageExample)
  }
}