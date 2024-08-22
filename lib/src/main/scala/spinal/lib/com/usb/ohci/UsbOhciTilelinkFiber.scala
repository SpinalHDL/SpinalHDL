package spinal.lib.com.usb.ohci

import spinal.core._
import spinal.lib._
import spinal.core.fiber._
import spinal.lib.bus.bmb._
import spinal.lib.bus.tilelink
import spinal.lib.bus.misc.SizeMapping
import spinal.lib.bus.tilelink.{M2sSupport, S2mSupport}
import spinal.lib.com.usb.phy.{UsbLsFsPhy, UsbPhyFsNativeIo}
import spinal.lib.misc.InterruptNode

class UsbOhciTilelinkFiber extends Area{
  var parameter : UsbOhciParameter = null

  val cd = ClockDomain.current
  val dma = tilelink.fabric.Node.down()
  val ctrl = tilelink.fabric.Node.up()
  val interrupt = InterruptNode.master()

  val logic = Fiber build new Area{
    val dmaBmb = UsbOhci.dmaParameter(parameter).access

    dma.m2s.forceParameters(UsbOhciTilelink.getDmaM2s(parameter, UsbOhciTilelinkFiber.this))
    dma.s2m.supported.load(S2mSupport.none())

    ctrl.m2s.supported.load(UsbOhciTilelink.getCtrlSupport(parameter, ctrl.m2s.proposed))
    ctrl.s2m.none()

    val ohci = UsbOhciTilelink(parameter, ctrl.m2s.parameters)
    ohci.io.dma >> dma.bus
    ohci.io.ctrl << ctrl.bus
    interrupt.flag := ohci.io.interrupt

//    ohci.io.phy.flatten.filter(_.isInput).foreach(_.clearAll())
  }

  def createPhyDefault() = new Area{
    val usb = Handle(logic.io.usb)
    val management = Handle(logic.io.management)
    val logic = Handle(UsbLsFsPhy(parameter.portCount, sim=false))

    Handle{
      val ctrlCd = UsbOhciTilelinkFiber.this.cd
      val phyCd = logic.clockDomain
      if(ctrlCd == phyCd) {
        UsbOhciTilelinkFiber.this.logic.ohci.io.phy <> logic.io.ctrl
      } else {
        UsbOhciTilelinkFiber.this.logic.ohci.io.phy.cc(ctrlCd, phyCd) <> logic.io.ctrl
      }
    }

    def createInferableIo() = Handle{
      logic.clockDomain {
        management.get.map(e => e.overcurrent := False)
        val native = usb.get.map(_.toNativeIo())
        native.map(e => master(e.bufferized()))
      }
    }

    def createSimIo() = Handle {
      (usb.toIo, management.toIo)
    }
  }
}
