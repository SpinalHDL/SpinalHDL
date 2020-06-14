package spinal.lib.com.eth

import spinal.core._
import spinal.lib._
import spinal.lib.bus.misc.BusSlaveFactory



case class MacMiiParameter(mii: MiiParameter,
                           rxDataWidth : Int,
                           txDataWidth : Int,
                           rxBufferByteSize : Int,
                           txBufferByteSize : Int){
  val txAvailabilityWidth = log2Up((txBufferByteSize * 8 / 32) + 1)
}

case class MacMiiCtrl(p : MacMiiParameter) extends Bundle{
  val rx = new Bundle {
    val stream = master(Stream(Bits(p.rxDataWidth bits)))
  }
  val tx = new Bundle {
    val stream = slave(Stream(Bits(p.txDataWidth bits)))
    val availability = out UInt(p.txAvailabilityWidth bits)
  }

  val clear = in Bool()
  val interrupt = out Bool()

  def driveFrom(bus: BusSlaveFactory) = new Area{
    bus.drive(clear,   0x00, 0) init(True)
    bus.read(rx.stream.valid, 0x00, 4)
    bus.read(tx.stream.ready, 0x00, 5)

    rx.stream.ready := False
    bus.onRead(0x10){rx.stream.ready := True}
    bus.read(rx.stream.payload, 0x10)


    tx.stream << bus.createAndDriveFlow(Bits(p.txDataWidth bits), 0x20).toStream
    bus.read(tx.availability, 0x24)

    val interruptCtrl = new Area{
      val pending = RegNext(interrupt) init(False)
    }
  }
}

case class MacMii(p : MacMiiParameter) extends Component{
  val io = new Bundle {
    val mii = master(Mii(p.mii))
    val ctrl = MacMiiCtrl(p)
  }


  io.ctrl.interrupt := False

  val ctrlClockDomain = this.clockDomain

  val rxReset = ResetCtrl.asyncAssertSyncDeassert(
    input = ClockDomain.current.isResetActive,
    clockDomain = ClockDomain(io.mii.RX.CLK)
  )
  val rxClockDomain = ClockDomain(io.mii.RX.CLK, rxReset)


  val txReset = ResetCtrl.asyncAssertSyncDeassert(
    input = ClockDomain.current.isResetActive,
    clockDomain = ClockDomain(io.mii.TX.CLK)
  )
  val txClockDomain = ClockDomain(io.mii.TX.CLK, txReset)

  val rxFrontend = rxClockDomain on new Area{
    val clear = BufferCC(io.ctrl.clear)

    val preamble = MacRxPreamble(dataWidth = p.mii.rx.dataWidth)
    preamble.io.phy << io.mii.RX.toRxFlow().throwWhen(clear)

    val checker = MacRxChecker(dataWidth = p.mii.rx.dataWidth)
    checker.io.input << preamble.io.data
    checker.io.clear := clear

    val buffer = MacRxBuffer(
      pushCd = rxClockDomain,
      popCd = ctrlClockDomain,
      pushWidth = p.mii.rx.dataWidth,
      popWidth = p.rxDataWidth,
      byteSize = p.rxBufferByteSize,
      lengthMax = 2000
    )
    buffer.io.push.clear := clear
    buffer.io.push.stream << preamble.io.data.translateWith(preamble.io.data.data)
    buffer.io.push.validate := RegNext(checker.io.validate) init(False)
    buffer.io.push.discard  := RegNext(checker.io.discard) init(False)
  }

  val rxBackend = new Area{
    rxFrontend.buffer.io.pop.clear := io.ctrl.clear
    rxFrontend.buffer.io.pop.stream >> io.ctrl.rx.stream
  }


  val txFrontend = new Area{
    val buffer = MacTxBuffer(
      pushCd = ctrlClockDomain,
      popCd = txClockDomain,
      pushWidth = p.rxDataWidth,
      popWidth = p.mii.tx.dataWidth,
      byteSize = p.txBufferByteSize,
      lengthMax = 2000
    )
    buffer.io.push.stream << io.ctrl.tx.stream
    buffer.io.push.clear := io.ctrl.clear
    buffer.io.push.availability <> io.ctrl.tx.availability
  }

  val txBackend = txClockDomain on new Area{
    val clear = BufferCC(io.ctrl.clear)
    txFrontend.buffer.io.pop.clear := clear

    val padder = MacTxPadder(dataWidth = p.mii.tx.dataWidth)
    padder.io.input << txFrontend.buffer.io.pop.stream
    padder.io.clear := clear

    val crc = MacTxCrc(dataWidth = p.mii.tx.dataWidth) //TODO
    crc.io.input << padder.io.output
    crc.io.clear := clear

    val header = MacTxHeader(dataWidth = p.mii.tx.dataWidth)
    header.io.input << crc.io.output
    header.io.clear := clear

    val tailer = MacTxInterFrame(dataWidth = p.mii.tx.dataWidth)
    tailer.io.input << header.io.output
    tailer.io.clear := clear


    txFrontend.buffer.io.pop.redo := False
    txFrontend.buffer.io.pop.commit := RegNext(tailer.io.output.valid.fall(False))

    tailer.io.output.ready := True
    io.mii.TX.EN := RegNext(tailer.io.output.valid)
    io.mii.TX.D := RegNext(tailer.io.output.fragment)
  }
}