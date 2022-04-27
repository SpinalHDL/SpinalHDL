package spinal.lib.com.usb.udc

import spinal.core._
import spinal.lib._
import spinal.lib.bus.bmb.BmbParameter
import spinal.lib.bus.wishbone._
import spinal.lib.com.usb.phy.{UsbDevicePhyNative, UsbPhyFsNativeIo}

case class UsbDeviceWithPhyWishbone(p : UsbDeviceCtrlParameter, phyCd : ClockDomain) extends Component {

  val bmbParameter = BmbParameter(
    addressWidth = UsbDeviceCtrl.ctrlAddressWidth,
    dataWidth = 32,
    sourceWidth = 0,
    contextWidth = 0,
    lengthWidth = 2
  )

  val wishboneParameter = WishboneConfig(
    addressWidth = UsbDeviceCtrl.ctrlAddressWidth-2,
    dataWidth = 32,
    selWidth = 4
  )

  val io = new Bundle{
    val wishbone = slave(Wishbone(wishboneParameter))
    val usb = master(UsbPhyFsNativeIo())
    val power = in Bool()
    val interrupt = out Bool()
  }

  val ctrl = new Area {
    val bridge = WishboneToBmb(wishboneParameter)
    val logic = UsbDeviceCtrl(p, bmbParameter)

    bridge.io.input <> io.wishbone
    bridge.io.output <> logic.io.ctrl
    logic.io.interrupt <> io.interrupt
  }

  val phy = phyCd on new Area{
    val logic    = UsbDevicePhyNative()
    val native = logic.io.usb.toNativeIo()
    when(!logic.io.pullup){
      native.dp.writeEnable := True
      native.dm.writeEnable := True
      native.dp.write := False
      native.dm.write := False
    }
    val buffer = native.stage()
    io.usb <> buffer.stage()
    logic.io.power := io.power
  }


  ctrl.logic.io.phy.cc(ClockDomain.current, phyCd) <> phy.logic.io.ctrl
}


object UsbDeviceCtrlWishboneGen extends App{
  SpinalVerilog(
    ClockDomain.external("ctrl") on UsbDeviceWithPhyWishbone(
      UsbDeviceCtrlParameter(
        addressWidth = 12
      ),
      ClockDomain.external("phy", frequency = FixedFrequency(48 MHz))
    )
  )
}