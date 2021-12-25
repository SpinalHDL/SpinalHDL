package spinal.lib.com.usb.udc

import spinal.core._
import spinal.core.fiber._
import spinal.lib.bus.bmb._
import spinal.lib._
import spinal.lib.bus.misc.SizeMapping
import spinal.lib.com.usb.ohci.UsbOhci
import spinal.lib.com.usb.phy.UsbDevicePhyNative

class UsbDeviceBmbGenerator(ctrlOffset : Handle[BigInt] = Unset)
                           (implicit interconnect: BmbInterconnectGenerator, decoder : BmbImplicitPeripheralDecoder = null)  extends Area{

  val parameter = Handle[UsbDeviceCtrlParameter]
  val ctrl = Handle(logic.io.ctrl)
  val interrupt = Handle(logic.io.interrupt)
  val logic = Handle(UsbDeviceCtrl(parameter, ctrlRequirements.toBmbParameter()))

  def createPhyDefault() = new Area{
    val usb = Handle(logic.io.usb)
    val power = Handle(logic.io.power)
    val logic = Handle(UsbDevicePhyNative(sim=false))

    Handle{
      val ctrlCd = UsbDeviceBmbGenerator.this.logic.clockDomain
      val phyCd = logic.clockDomain
      if(ctrlCd == phyCd) {
        UsbDeviceBmbGenerator.this.logic.io.phy <> logic.io.ctrl
      } else {
        UsbDeviceBmbGenerator.this.logic.io.phy.cc(ctrlCd, phyCd) <> logic.io.ctrl
      }
    }

    def createInferableIo(withPower : Boolean = true) = Handle{logic.clockDomain {new Area{
      val native = usb.get.toNativeIo()
      when(!logic.io.pullup){
        native.dp.writeEnable := True
        native.dm.writeEnable := True
        native.dp.write := False
        native.dm.write := False
      }
      val buffer = native.stage()
      val dif = master(buffer.stage())
      val power = if(withPower) in(Bool()) else True
      logic.io.power := power
    }}}
  }

  val ctrlSource       = Handle[BmbAccessCapabilities]
  val ctrlCapabilities = Handle(UsbDeviceCtrl.ctrlCapabilities(ctrlSource))
  val ctrlRequirements = Handle[BmbAccessParameter]


  interconnect.addSlave(
    accessSource       = ctrlSource,
    accessCapabilities = ctrlCapabilities,
    accessRequirements = ctrlRequirements,
    bus                = ctrl,
    mapping            = Handle(SizeMapping(ctrlOffset, 1 << UsbDeviceCtrl.ctrlAddressWidth))
  )
  if(decoder != null) interconnect.addConnection(decoder.bus, ctrl)
}
