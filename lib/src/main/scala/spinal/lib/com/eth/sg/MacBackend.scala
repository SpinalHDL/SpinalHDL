package spinal.lib.com.eth.sg

import spinal.core._
import spinal.lib._
import spinal.lib.bus.misc.BusSlaveFactory
import spinal.lib.com.eth.{MacRxAligner, MacRxCheckSumChecker, MacRxChecker, MacRxDropper, MacRxPreamble, MacTxCrc, MacTxHeader, MacTxPadder, PhyIo, PhyParameter, PhyRx, PhyTx}



case class MacBackendParam(phy: PhyParameter){

}

case class MacEthSgCtrl(p : MacBackendParam) extends Bundle{
//  val rx = new Bundle {
//    val stream = master(Stream(Bits(p.rxDataWidth bits)))
//    val flush = in Bool()
//    val alignerEnable = in Bool()
//    val stats = new Bundle {
//      val clear = in Bool()
//      val drops, errors = out UInt (8 bits)
//    }
//  }
//  val tx = new Bundle {
//    val stream = slave(Stream(Bits(p.txDataWidth bits)))
//    val availability = out UInt(p.txAvailabilityWidth bits)
//    val flush = in Bool()
//    val alignerEnable = in Bool()
//  }

  def driveFrom(bus: BusSlaveFactory) = new Area{
//    bus.driveAndRead(tx.flush,   0x00, 0) init(True)
//    bus.read(tx.stream.ready, 0x00, 1)
//    bus.driveAndRead(tx.alignerEnable, 0x00, 2) init(False)
//
//    bus.driveAndRead(rx.flush,   0x00, 4) init(True)
//    bus.read(rx.stream.valid, 0x00, 5)
//    bus.driveAndRead(rx.alignerEnable, 0x00, 6) init(False)
//
//    tx.stream << bus.createAndDriveFlow(Bits(p.txDataWidth bits), 0x10).toStream
//    bus.read(tx.availability, 0x14)
//
//    rx.stream.ready := False
//    bus.onRead(0x20){rx.stream.ready := True}
//    bus.read(rx.stream.payload, 0x20)
//
//    rx.stats.clear := False
//    bus.onRead(0x2C){rx.stats.clear := True}
//    bus.read(rx.stats.errors, 0x2C, 0)
//    bus.read(rx.stats.drops, 0x2C, 8)
//
//    val interruptCtrl = new Area{
//      val pending = RegNext(rx.stream.valid) init(False)
//    }
  }
}

case class MacEthPackets(p : PhyParameter) extends Bundle with IMasterSlave {
  val tx = Stream(Fragment(PhyTx(p.txDataWidth)))
  val rx = Stream(Fragment(PhyRx(p.txDataWidth)))

  override def asMaster(): Unit = {
    master(tx)
    slave(rx)
  }
}

case class MacBackend(phyParam: PhyParameter,
                      txCd : ClockDomain,
                      rxCd : ClockDomain) extends Component{
  val io = new Bundle {
    val packets = slave(MacEthPackets(phyParam))
    val phy = master(PhyIo(phyParam))
  }

  val ctrlClockDomain = this.clockDomain

  val rxFrontend = rxCd on new Area{
    val preamble = MacRxPreamble(dataWidth = phyParam.rxDataWidth)
    preamble.io.input << io.phy.rx

    val checker = MacRxChecker(dataWidth = phyParam.rxDataWidth)
    checker.io.input << preamble.io.output

    val dropper = MacRxDropper(dataWidth = phyParam.rxDataWidth)
    dropper.io.input << checker.io.output

    val checksum = MacRxCheckSumChecker()
    checksum.io.input << dropper.io.output

    io.packets.rx << checksum.io.output
  }


  val onTx = txCd on new Area{
    val padder = MacTxPadder(dataWidth = phyParam.txDataWidth, carrierExtension = false)
    padder.io.input.arbitrationFrom(io.packets.tx)
    padder.io.input.data := io.packets.tx.data
    padder.io.input.last := io.packets.tx.last

    val crc = MacTxCrc(dataWidth = phyParam.txDataWidth)
    crc.io.input << padder.io.output

//    val carrierExtender = MacTxPadder(dataWidth = phyParam.txDataWidth, carrierExtension = true)
//    carrierExtender.io.input << crc.io.output

    val header = MacTxHeader(dataWidth = phyParam.txDataWidth)
    header.io.input << crc.io.output
    header.io.output >> io.phy.tx
  }
}