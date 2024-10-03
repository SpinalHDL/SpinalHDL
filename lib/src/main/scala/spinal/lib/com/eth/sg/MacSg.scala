package spinal.lib.com.eth.sg

import spinal.core._
import spinal.lib.bus.bsb.{Bsb, BsbDownSizerDense, BsbDownSizerSparse, BsbPacketBuffer}
import spinal.lib.bus.misc.BusSlaveFactoryAddressWrapper
import spinal.lib.com.eth.{PhyIo, PhyParameter}
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
                      val txBufferBytes : Int)


class MacSg(val p : MacSgParam,
            val ctrlParam : BusParameter,
            val txMemParam : BusParameter,
            val ctrlCd : ClockDomain,
            val txCd : ClockDomain,
            val rxCd : ClockDomain) extends Component {
  val io = new Bundle {
    val ctrl = slave(tilelink.Bus(ctrlParam))
    val txMem = master(tilelink.Bus(txMemParam))
    val phy = master(PhyIo(p.phyParam))
    val interrupt = out Bool()
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

    val buffer = new BsbPacketBuffer(serializer.io.output.p, p.txBufferBytes, 15)
    buffer.io.input << serializer.io.output

    val buffered = buffer.io.output.stage()
    backend.io.packets.tx.arbitrationFrom(buffered)
    backend.io.packets.tx.data := buffered.data
    backend.io.packets.tx.last := buffered.last
  }

  val txDma = new sg2.DmaSgReadOnly(
    p = p.txDmaParam,
    bsb = onTx.dma,
    mem = io.txMem,
    ctrl = new BusSlaveFactoryAddressWrapper(onCtrl.bus, 0x100),
    pushCd = ctrlCd,
    popCd = txCd
  )
  io.interrupt := txDma.onCtrl.irq.interrupt
}
