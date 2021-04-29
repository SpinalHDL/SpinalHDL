package spinal.lib.com.usb.ohci

import spinal.core._
import spinal.lib._
import spinal.core.fiber._
import spinal.lib.bus.bmb._
import spinal.lib.bus.misc.SizeMapping
import spinal.lib.com.usb.phy.UsbLsFsPhy

class UsbOhciGenerator(ctrlOffset : Handle[BigInt] = Unset)
                      (implicit interconnect: BmbInterconnectGenerator, decoder : BmbImplicitPeripheralDecoder = null) extends Area{

  val parameter = Handle[UsbOhciParameter]

  val dmaRequirements = Handle(UsbOhci.dmaParameter(parameter).access)
  val ctrlSource       = Handle[BmbAccessCapabilities]
  val ctrlCapabilities = Handle(UsbOhci.ctrlCapabilities(ctrlSource))
  val ctrlRequirements = Handle[BmbAccessParameter]

  val dma = Handle(logic.io.dma)
  val ctrl = Handle(logic.io.ctrl)
  val interrupt = Handle(logic.io.interrupt)

  interconnect.addMaster(
    accessRequirements = dmaRequirements,
    bus = dma
  )
  interconnect.addSlave(
    accessSource       = ctrlSource,
    accessCapabilities = ctrlCapabilities,
    accessRequirements = ctrlRequirements,
    bus                = ctrl,
    mapping            = Handle(SizeMapping(ctrlOffset, 1 << 12))
  )

  val logic = Handle(UsbOhci(parameter, ctrlRequirements.toBmbParameter()))

  def createPhyDefault() = new Area{
    val usb = Handle(logic.io.usb.toIo())
    val logic = Handle(UsbLsFsPhy(parameter.portCount, parameter.fsRatio, sim=false))

    Handle{
      UsbOhciGenerator.this.logic.io.phy <> logic.io.ctrl
    }
  }
}
