package spinal.lib.com.spi.xdr

import spinal.core._
import spinal.core.fiber.Fiber
import spinal.lib._
import spinal.lib.bus.tilelink._
import spinal.lib.com.spi.ddr.{SpiXdrMaster, SpiXdrMasterCtrl}
import spinal.lib.misc.InterruptNode


object TilelinkSpiXdrMasterCtrl{
  def getSupported(proposed: M2sSupport) = SlaveFactory.getSupported(
    addressWidth = addressWidth,
    dataWidth = 32,
    allowBurst = false,
    proposed = proposed
  )

  def addressWidth = 12
}


case class TilelinkSpiXdrMasterCtrl(p : SpiXdrMasterCtrl.MemoryMappingParameters, ctrlParameter : BusParameter) extends Component{
  val io = new Bundle {
    val ctrl = slave(Bus(ctrlParameter))
    val xip = ifGen(p.xip != null) (slave(SpiXdrMasterCtrl.XipBus(p.xip)))
    val spi = master(SpiXdrMaster(p.ctrl.spi))
    val interrupt = out Bool()
  }

  val ctrl = SpiXdrMasterCtrl(p.ctrl)
  val factory = new SlaveFactory(io.ctrl, false)
  val mapping = SpiXdrMasterCtrl.driveFrom(ctrl, factory)(p)
  if(p.xip != null) io.xip <> mapping.xip.xipBus
  io.spi <> ctrl.io.spi
  io.interrupt <> mapping.interruptCtrl.interrupt
}


class TilelinkSpiXdrMasterFiber(param : SpiXdrMasterCtrl.MemoryMappingParameters) extends Area{
  val ctrl = fabric.Node.up()
  val interrupt = InterruptNode.master()
  val xip = param.withXip generate fabric.Node.up()

  val logic = Fiber build new Area{
    if(param.withXip){
      xip.m2s.supported.load(
        new M2sSupport(
          addressWidth = param.xip.addressWidth,
          dataWidth = 8,
          transfers = xip.m2s.proposed.transfers.intersect(
            M2sTransfers(
              get = SizeRange(1, 1 << param.xip.lengthWidth)
            )
          )
        )
      )
      xip.s2m.none()
    }

    ctrl.m2s.supported.load(TilelinkSpiXdrMasterCtrl.getSupported(ctrl.m2s.proposed))
    ctrl.s2m.none()

    val core = TilelinkSpiXdrMasterCtrl(param, ctrl.bus.p)
    core.io.ctrl <> ctrl.bus
    core.io.interrupt <> interrupt.flag
    if(param.withXip) core.io.xip.fromTilelink(xip.bus.p) << xip.bus
    val spi = core.io.spi.toIo
  }
}

