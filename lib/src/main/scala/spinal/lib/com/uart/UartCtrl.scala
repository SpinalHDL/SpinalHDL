package spinal.lib.com.uart

import UartParityType._
import UartStopType._
import spinal.core._
import spinal.lib.FragmentToBitsStates._
import spinal.lib._

//All construction parameters of the UartCtrl
case class UartCtrlGenerics( dataWidthMax: Int = 8,
                             clockDividerWidth: Int = 20, // !! baudrate = Fclk / (rxSamplePerBit*clockDividerWidth) !!
                             preSamplingSize: Int = 1,
                             samplingSize: Int = 5,
                             postSamplingSize: Int = 2) {
  val rxSamplePerBit = preSamplingSize + samplingSize + postSamplingSize
  assert(isPow2(rxSamplePerBit))
  if ((samplingSize % 2) == 0)
    SpinalWarning(s"It's not nice to have a odd samplingSize value at ${ScalaLocated.short} (because of the majority vote)")
}


case class UartCtrlFrameConfig(g: UartCtrlGenerics) extends Bundle {
  val dataLength = UInt(log2Up(g.dataWidthMax) bit) //Bit count = dataLength + 1
  val stop       = UartStopType()
  val parity     = UartParityType()
}

case class UartCtrlConfig(g: UartCtrlGenerics) extends Bundle {
  val frame        = UartCtrlFrameConfig(g)
  val clockDivider = UInt (g.clockDividerWidth bit) //see UartCtrlGenerics.clockDividerWidth for calculation
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

  io.uart.txd := tx.io.txd
  rx.io.rxd := io.uart.rxd
}


