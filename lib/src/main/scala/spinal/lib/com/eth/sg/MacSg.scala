package spinal.lib.com.eth.sg

import spinal.core._
import spinal.lib.bus.bsb.{Bsb, BsbDownSizerDense, BsbDownSizerSparse}
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


class MacSg(val phyParam: PhyParameter,
            val txDmaParam : sg2.DmaSgReadOnlyParam,
            val ctrlParam : BusParameter,
            val txMemParam : BusParameter,
            val ctrlCd : ClockDomain,
            val txCd : ClockDomain,
            val rxCd : ClockDomain) extends Component {
  val io = new Bundle {
    val ctrl = slave(tilelink.Bus(ctrlParam))
    val txMem = master(tilelink.Bus(txMemParam))
    val phy = master(PhyIo(phyParam))
    val interrupt = out Bool()
  }


  val onCtrl = new ClockingArea(ctrlCd){
    val bus = new tilelink.SlaveFactory(io.ctrl, false)
  }

  val backend = MacBackend(phyParam, txCd, rxCd)
  backend.io.phy <> io.phy

  val onTx = new ClockingArea(txCd){
    val dma = Bsb(txDmaParam.getBsbParameter())

    val serializer = new BsbDownSizerDense(dma.p, phyParam.txDataWidth/8)
    serializer.io.input << dma

    val buffered = serializer.io.output.stage()
    backend.io.packets.tx.arbitrationFrom(buffered)
    backend.io.packets.tx.data := buffered.data
    backend.io.packets.tx.last := buffered.last
  }

  val txDma = new sg2.DmaSgReadOnly(
    p = txDmaParam,
    bsb = onTx.dma,
    mem = io.txMem,
    ctrl = new BusSlaveFactoryAddressWrapper(onCtrl.bus, 0x100),
    pushCd = ctrlCd,
    popCd = txCd
  )
  io.interrupt := txDma.onCtrl.irq.interrupt
}
