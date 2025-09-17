package spinal.lib.com.eth.sg

import spinal.core._
import spinal.lib.bus.bsb.{Bsb, BsbDownSizerDense, BsbDownSizerSparse, BsbPacketBuffer, BsbParameter, BsbUpSizerDense, BsbUpSizerSparse}
import spinal.lib.bus.misc.BusSlaveFactoryAddressWrapper
import spinal.lib.com.eth.{MacTxLso, PhyIo, PhyParameter, PhyTx}
import spinal.lib.bus.tilelink
import spinal.lib.bus.tilelink.BusParameter
import spinal.lib._
import spinal.lib.system.dma.sg2
import spinal.lib.system.dma.sg2.DmaSgReadOnlyParam

import scala.collection.mutable.ArrayBuffer

object MacSg{
  def getCtrlSupport(proposed: bus.tilelink.M2sSupport) = bus.tilelink.SlaveFactory.getSupported(
    addressWidth = ctrlAddressWidth,
    dataWidth = 32,
    allowBurst = false,
    proposed
  )
  def getCtrlParam() = bus.tilelink.SlaveFactory.getParameter(
    addressWidth = ctrlAddressWidth,
    dataWidth = 32,
    allowBurst = false
  )
  def ctrlAddressWidth = 12
}

case class MacSgParam(val phyParam: PhyParameter,
                      val txDmaParam : sg2.DmaSgReadOnlyParam,
                      val txBufferBytes : Int,
                      val rxDmaParam : sg2.DmaSgWriteOnlyParam,
                      val rxBufferBytes : Int,
                      val rxUpsizedBytes : Int)


class MacSg(val p : MacSgParam,
            val ctrlParam : BusParameter,
            val txMemParam : BusParameter,
            val rxMemParam : BusParameter,
            val ctrlCd : ClockDomain,
            val txCd : ClockDomain,
            val rxCd : ClockDomain) extends Component {
  val io = new Bundle {
    val ctrl = slave(tilelink.Bus(ctrlParam))
    val txMem = master(tilelink.Bus(txMemParam))
    val rxMem = master(tilelink.Bus(rxMemParam))
    val phy = master(PhyIo(p.phyParam))
    val txIrq = out Bool()
    val rxIrq = out Bool()
  }


  val onCtrl = new ClockingArea(ctrlCd){
    val bus = new tilelink.SlaveFactory(io.ctrl, false)
  }

  val backend = MacBackend(p.phyParam, txCd, rxCd)
  backend.io.phy <> io.phy

  val onTx = new ClockingArea(txCd){
    val dma = Bsb(p.txDmaParam.getBsbParameter())

    val serializer = new BsbDownSizerDense(dma.p, p.phyParam.txDataWidth/8)
    serializer.io.input << dma

    val lso = new MacTxLso(p.txBufferBytes, mtuMax = 1500+14)
    lso.io.input.arbitrationFrom(serializer.io.output)
    lso.io.input.data := serializer.io.output.data
    lso.io.input.last := serializer.io.output.last
    val buffered = lso.io.output.stage()

//    val buffer = new BsbPacketBuffer(serializer.io.output.p, p.txBufferBytes, 15)
//    buffer.io.input << serializer.io.output
//    val buffered = buffer.io.output.stage()

    backend.io.packets.tx << buffered
  }

  val txDma = new sg2.DmaSgReadOnly(
    p = p.txDmaParam,
    bsb = onTx.dma,
    mem = io.txMem,
    ctrl = new BusSlaveFactoryAddressWrapper(onCtrl.bus, 0x100),
    pushCd = ctrlCd,
    popCd = txCd
  )

  val onRx = new ClockingArea(rxCd){
    val fromBackend =  Bsb(BsbParameter(p.phyParam.rxDataWidth/8, 0, 0, withError = true))
    fromBackend.arbitrationFrom(backend.io.packets.rx)
    fromBackend.data := backend.io.packets.rx.data
    fromBackend.error := backend.io.packets.rx.error
    fromBackend.last := backend.io.packets.rx.last
    fromBackend.mask := 1

    val upSizer = new BsbUpSizerDense(fromBackend.p, p.rxUpsizedBytes)
    upSizer.io.input << fromBackend
  }

  val rxBuffer = StreamFifoCC(
    dataType = onRx.upSizer.io.output.payloadType,
    depth = p.rxBufferBytes,
    pushClock = rxCd,
    popClock = ctrlCd
  )
  rxBuffer.io.push << onRx.upSizer.io.output

  val onRxCtrl = new ClockingArea(ctrlCd) {
    val rxDma = new sg2.DmaSgWriteOnly(
      p = p.rxDmaParam,
      bsb = rxBuffer.io.pop,
      mem = io.rxMem,
      ctrl = new BusSlaveFactoryAddressWrapper(onCtrl.bus, 0x200)
    )
  }

  io.txIrq := txDma.onCtrl.irq.interrupt
  io.rxIrq := onRxCtrl.rxDma.onCtrl.irq.interrupt
}
