package spinal.lib.com.usb.ohci

import spinal.core._
import spinal.lib.bus.bmb.{Bmb, BmbParameter, BmbToTilelink, TilelinkToBmb}
import spinal.lib.bus.tilelink
import spinal.lib.bus.tilelink.{M2sSupport, SlaveFactory}
import spinal.lib.com.usb.phy.UsbHubLsFs
import spinal.lib.{master, slave}

object UsbOhciTilelink {
  def getDmaM2s(p : UsbOhciParameter, name : Nameable) = BmbToTilelink.getTilelinkM2s(UsbOhci.dmaParameter(p).access, name)
  def getCtrlSupport(p : UsbOhciParameter, proposed : M2sSupport) = SlaveFactory.getSupported (
    addressWidth = UsbOhci.ctrlAddressWidth,
    dataWidth = 32,
    allowBurst = false,
    proposed =proposed
  )
}

case class UsbOhciTilelink(p : UsbOhciParameter, ctrlParameter : tilelink.M2sParameters) extends Component {
  val io = new Bundle {
    val ctrl = slave(tilelink.Bus(ctrlParameter))
    val dma = master(tilelink.Bus(UsbOhciTilelink.getDmaM2s(p, UsbOhciTilelink.this)))
    val phy = master(UsbHubLsFs.Ctrl(p.portCount))
    val interrupt = out Bool()
    val interruptBios = out Bool()
  }

  val logic = UsbOhci(p, BmbParameter(TilelinkToBmb.getBmbParam(ctrlParameter)))

  val dmaBridge = new BmbToTilelink(logic.io.dma.p)
  dmaBridge.io.up << logic.io.dma
  io.dma << dmaBridge.io.down

  val ctrlBridge = new TilelinkToBmb(ctrlParameter)
  ctrlBridge.io.up << io.ctrl
  logic.io.ctrl << ctrlBridge.io.down

  io.phy <> logic.io.phy
  io.interrupt <> logic.io.interrupt
  io.interruptBios <> logic.io.interruptBios
}