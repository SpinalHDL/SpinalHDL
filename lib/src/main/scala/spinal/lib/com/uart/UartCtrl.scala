package spinal.lib.com.uart

import UartParityType._
import UartStopType._
import spinal.core._
import spinal.lib.FragmentToBitsStates._
import spinal.lib._
import spinal.lib.bus.misc.BusSlaveFactory

//All construction parameters of the UartCtrl
case class UartCtrlGenerics( dataWidthMax: Int = 8,
                             clockDividerWidth: Int = 20, // !! baudrate = Fclk / (rxSamplePerBit*clockDividerWidth) !!
                             preSamplingSize: Int = 1,
                             samplingSize: Int = 5,
                             postSamplingSize: Int = 2) {
  val rxSamplePerBit = preSamplingSize + samplingSize + postSamplingSize

  assert(isPow2(rxSamplePerBit))
  if ((samplingSize % 2) == 0)
    SpinalWarning(s"It's not nice to have a even samplingSize value at ${ScalaLocated.short} (because of the majority vote)")
}


case class UartCtrlFrameConfig(g: UartCtrlGenerics) extends Bundle {
  val dataLength = UInt(log2Up(g.dataWidthMax) bit) //Bit count = dataLength + 1
  val stop       = UartStopType()
  val parity     = UartParityType()
}

case class UartCtrlConfig(g: UartCtrlGenerics) extends Bundle {
  val frame        = UartCtrlFrameConfig(g)
  val clockDivider = UInt (g.clockDividerWidth bit) //see UartCtrlGenerics.clockDividerWidth for calculation

  def setClockDivider(baudrate : BigDecimal,clkFrequency : BigDecimal = ClockDomain.current.frequency.getValue) : Unit = {
    clockDivider := (clkFrequency / baudrate / g.rxSamplePerBit).toInt
  }
}

class UartCtrlIo(g : UartCtrlGenerics) extends Bundle {
  val config = in(UartCtrlConfig(g))
  val write  = slave(Stream(Bits(g.dataWidthMax bit)))
  val read   = master(Flow(Bits(g.dataWidthMax bit)))
  val uart   = master(Uart())
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

  tx.io.write << io.write
  rx.io.read >> io.read

  io.uart.txd <> tx.io.txd
  io.uart.rxd <> rx.io.rxd


//  def driveFrom(busCtrl : BusSlaveFactory,rxFifoDepth : Int) : Unit = {
//    busCtrl.driveAndRead(io.config.clockDivider,address = 0)
//    busCtrl.driveAndRead(io.config.frame,address = 4)
//    busCtrl.createAndDriveFlow(Bits(g.dataWidthMax bits),address = 8).toStream >-> io.write
//    busCtrl.read(io.write.valid,address = 8)
//    busCtrl.readStreamNonBlocking(io.read.toStream.queue(rxFifoDepth),address = 12,validBitOffset = 31,payloadBitOffset = 0)
//  }
  def driveFrom(busCtrl : BusSlaveFactory,config : UartCtrlMemoryMappedConfig) = new Area {
    //Manage config
    val uartConfigReg = Reg(io.config)
    uartConfigReg.clockDivider init(0)
    if(config.initConfig != null)            config.initConfig.initReg(uartConfigReg)
    if(config.busCanWriteClockDividerConfig) busCtrl.write(uartConfigReg.clockDivider,address = 8)
    if(config.busCanWriteFrameConfig){
      busCtrl.write(uartConfigReg.frame.dataLength,address = 12,bitOffset = 0)
      busCtrl.write(uartConfigReg.frame.parity,address = 12,bitOffset = 8)
      busCtrl.write(uartConfigReg.frame.stop,address = 12,bitOffset = 16)
    }
    io.config := uartConfigReg

    //manage TX
    val write = new Area {
      val streamUnbuffered = busCtrl.createAndDriveFlow(Bits(g.dataWidthMax bits), address = 0).toStream
      val (stream, fifoOccupancy) = streamUnbuffered.queueWithOccupancy(config.txFifoDepth)
      io.write << stream
      busCtrl.read(config.txFifoDepth - fifoOccupancy, 4, 16)
    }

    //manage RX
    val read = new Area {
      val (stream, fofoOccupancy) = io.read.toStream.queueWithOccupancy(config.rxFifoDepth)
      busCtrl.readStreamNonBlocking(stream, address = 0, validBitOffset = 16, payloadBitOffset = 0)
      busCtrl.read(fofoOccupancy, 4, 24)
    }

    //manage interrupts
    val interruptCtrl = new Area {
      val writeIntEnable = busCtrl.createReadWrite(Bool, 4, 0) init(False)
      val readIntEnable  = busCtrl.createReadWrite(Bool, 4, 1) init(False)
      val readInt = readIntEnable & read.stream.valid
      val writeInt = writeIntEnable & write.stream.valid
      val interrupt = readInt || writeInt
      busCtrl.read(writeInt, 4, 8)
      busCtrl.read(readInt , 4, 9)
    }
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
    if(baudrate != 0) reg.clockDivider init((ClockDomain.current.frequency.getValue / baudrate / reg.g.rxSamplePerBit).toInt)
    if(dataLength != 0) reg.frame.dataLength init(dataLength)
    if(parity != null) reg.frame.parity init(parity)
    if(stop != null) reg.frame.stop init(stop)
  }
}

case class UartCtrlMemoryMappedConfig(
  uartCtrlConfig : UartCtrlGenerics,
  initConfig : UartCtrlInitConfig = null,
  busCanWriteClockDividerConfig : Boolean = true,
  busCanWriteFrameConfig : Boolean = true,
  txFifoDepth : Int = 1,
  rxFifoDepth : Int = 1
){
  require(txFifoDepth >= 1)
  require(rxFifoDepth >= 1)

  require(txFifoDepth <= 255)
  require(rxFifoDepth <= 255)
}



class UartCtrlUsageExample extends Component{
  val io = new Bundle{
    val uart = master(Uart())
    val switchs = in Bits(8 bits)
    val leds = out Bits(8 bits)
  }

  val uartCtrl = new UartCtrl()
  uartCtrl.io.config.setClockDivider(921600)
  uartCtrl.io.config.frame.dataLength := 7  //8 bits
  uartCtrl.io.config.frame.parity := UartParityType.NONE
  uartCtrl.io.config.frame.stop := UartStopType.ONE
  uartCtrl.io.uart <> io.uart

  //Assign io.led with a register loaded each time a byte is received
  io.leds := uartCtrl.io.read.toReg()

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
      defaultClockDomainFrequency=FixedFrequency(50e6)
    ).generate(new UartCtrlUsageExample)
  }
}