/*                                                                           *\
**        _____ ____  _____   _____    __                                    **
**       / ___// __ \/  _/ | / /   |  / /   HDL Lib                          **
**       \__ \/ /_/ // //  |/ / /| | / /    (c) Dolu, All rights reserved    **
**      ___/ / ____// // /|  / ___ |/ /___                                   **
**     /____/_/   /___/_/ |_/_/  |_/_____/  MIT Licence                      **
**                                                                           **
** Permission is hereby granted, free of charge, to any person obtaining a   **
** copy of this software and associated documentation files (the "Software"),**
** to deal in the Software without restriction, including without limitation **
** the rights to use, copy, modify, merge, publish, distribute, sublicense,  **
** and/or sell copies of the Software, and to permit persons to whom the     **
** Software is furnished to do so, subject to the following conditions:      **
**                                                                           **
** The above copyright notice and this permission notice shall be included   **
** in all copies or substantial portions of the Software.                    **
**                                                                           **
** THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS   **
** OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF                **
** MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.    **
** IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY      **
** CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT **
** OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR  **
** THE USE OR OTHER DEALINGS IN THE SOFTWARE.                                **
\*                                                                           */

package spinal.lib.peripheral.uart.controller

import spinal.core._
import spinal.lib._
import spinal.lib.FragmentToBitsStates._
import spinal.lib.bus.misc.{BusSlaveFactoryAddressWrapper, BusSlaveFactory}
import spinal.lib.peripheral.uart.{UartParityType, UartStopType}
import spinal.lib.peripheral.uart.{Uart, UartParameter, UartConfig}


case class UartControllerFrameStorage(parameter: UartParameter) extends Bundle {
  val dataLength = UInt(log2Up(parameter.dataWidthMax) bits)
  val stop       = UartStopType()
  val parity     = UartParityType()
}

case class UartControllerStorage(parameter: UartParameter) extends Bundle {
  val frame        = UartControllerFrameStorage(parameter)
  val clockDivider = UInt(parameter.clockDividerWidth bits)
}

class UartControllerIo(parameter: UartParameter) extends Bundle {
  val storage = in(UartControllerStorage(parameter))
  val write   = slave(Stream(Bits(parameter.dataWidthMax bits)))
  val read    = master(Flow(Bits(parameter.dataWidthMax bits)))
  val uart    = master(Uart())
}

class UartController(parameter: UartParameter) extends Component {
  val io = new UartControllerIo(parameter)
  val tx = new UartControllerTx(parameter)
  val rx = new UartControllerRx(parameter)

  //Clock divider used by RX and TX
  val clockDivider = new Area {
    val counter = Reg(UInt(parameter.clockDividerWidth bits)) init(0)
    val tick = counter === 0

    counter := counter - 1
    when(tick) {
      counter := io.storage.clockDivider
    }
  }

  tx.io.samplingTick := clockDivider.tick
  rx.io.samplingTick := clockDivider.tick

  tx.io.frameStorage := io.storage.frame
  rx.io.frameStorage := io.storage.frame

  tx.io.write << io.write
  rx.io.read >> io.read

  io.uart.txd <> tx.io.txd
  io.uart.rxd <> rx.io.rxd

  def driveFrom(busCtrl: BusSlaveFactory, parameter: UartParameter, config: UartConfig, baseAddress: Int = 0) = new Area {
    require(busCtrl.busDataWidth == 16 || busCtrl.busDataWidth == 32)
    val busCtrlWrapped = new BusSlaveFactoryAddressWrapper(busCtrl, baseAddress)

    val uartCtrlStorage = Reg(io.storage)
    if (config.baudrate != 0)
      uartCtrlStorage.clockDivider init(U(value = parameter.getClockDivider(config.baudrate)))
    else
      uartCtrlStorage.clockDivider init(0)
    if (config.dataLength != 0)
      uartCtrlStorage.frame.dataLength init(config.dataLength)
    if (config.parity != null)
      uartCtrlStorage.frame.parity init(config.parity)
    if (config.stop != null)
      uartCtrlStorage.frame.stop init(config.stop)

    if (parameter.busCanWriteClockDividerConfig)
      busCtrlWrapped.writeMultiWord(uartCtrlStorage.clockDivider, address = 0x08)
    else
      uartCtrlStorage.clockDivider.allowUnsetRegToAvoidLatch

    if (parameter.busCanWriteFrameConfig) {
      busCtrlWrapped.write(uartCtrlStorage.frame.dataLength, address = 0x0c, bitOffset = 0)
      busCtrlWrapped.write(uartCtrlStorage.frame.parity, address = 0x0c, bitOffset = 8)
      busCtrl.busDataWidth match {
        case 16 => busCtrlWrapped.write(uartCtrlStorage.frame.stop, address = 0x0e, bitOffset = 0)
        case 32 => busCtrlWrapped.write(uartCtrlStorage.frame.stop, address = 0x0c, bitOffset = 16)
      }
    } else {
      uartCtrlStorage.frame.allowUnsetRegToAvoidLatch
    }
    io.storage := uartCtrlStorage

    //manage TX
    val write = new Area {
      val streamUnbuffered = busCtrlWrapped.createAndDriveFlow(Bits(parameter.dataWidthMax bits), address = 0x00).toStream
      val (stream, fifoOccupancy) = streamUnbuffered.queueWithOccupancy(parameter.txFifoDepth)
      io.write << stream
      busCtrl.busDataWidth match {
        case 16 => busCtrlWrapped.read(parameter.txFifoDepth - fifoOccupancy, address = 0x06,bitOffset = 0)
        case 32 => busCtrlWrapped.read(parameter.txFifoDepth - fifoOccupancy, address = 0x04,bitOffset = 16)
      }
      streamUnbuffered.ready.allowPruning()
    }

    //manage RX
    val read = new Area {
      val (stream, fifoOccupancy) = io.read.queueWithOccupancy(parameter.rxFifoDepth)
      busCtrl.busDataWidth match {
        case 16 =>
          busCtrlWrapped.readStreamNonBlocking(stream, address = 0x00, validBitOffset = 15, payloadBitOffset = 0)
          busCtrlWrapped.read(fifoOccupancy, address = 0x06, 8)
        case 32 =>
          busCtrlWrapped.readStreamNonBlocking(stream, address = 0x00, validBitOffset = 16, payloadBitOffset = 0)
          busCtrlWrapped.read(fifoOccupancy,address = 0x04, 24)
      }
      def genCTS(freeThreshold : Int) = RegNext(fifoOccupancy <= parameter.rxFifoDepth - freeThreshold) init(False)  // freeThreshold => how many remaining space should be in the fifo before allowing transfer
    }

    //manage interrupts
    val interruptCtrl = new Area {
      val writeIntEnable = busCtrlWrapped.createReadAndWrite(Bool, address = 0x04, 0) init(False)
      val readIntEnable  = busCtrlWrapped.createReadAndWrite(Bool, address = 0x04, 1) init(False)
      val readInt   = readIntEnable  & read.stream.valid
      val writeInt  = writeIntEnable & !write.stream.valid
      val interrupt = readInt || writeInt
      busCtrlWrapped.read(writeInt, address = 0x04, 8)
      busCtrlWrapped.read(readInt, address = 0x04, 9)
    }
  }
}
