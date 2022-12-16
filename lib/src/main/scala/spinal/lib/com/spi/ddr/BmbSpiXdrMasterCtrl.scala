package spinal.lib.com.spi.ddr

import spinal.core._
import spinal.lib.bus.bmb.{Bmb, BmbAccessCapabilities, BmbAccessParameter, BmbParameter, BmbSlaveFactory}
import spinal.lib.com.spi.ddr.SpiXdrMasterCtrl.{Cmd, Config, Rsp}
import spinal.lib.{Flow, Stream, master, slave}



object BmbSpiXdrMasterCtrl{
  def getBmbCapabilities(accessSource : BmbAccessCapabilities) = BmbSlaveFactory.getBmbCapabilities(
    accessSource,
    addressWidth = addressWidth,
    dataWidth = 32
  )
  def addressWidth = 12
}


case class BmbSpiXdrMasterCtrl(p : SpiXdrMasterCtrl.MemoryMappingParameters, ctrlParameter : BmbParameter) extends Component{
  val io = new Bundle {
    val ctrl = slave(Bmb(ctrlParameter))
    val xip = ifGen(p.xip != null) (slave(SpiXdrMasterCtrl.XipBus(p.xip)))
    val spi = master(SpiXdrMaster(p.ctrl.spi))
    val interrupt = out Bool()
  }

  val ctrl = SpiXdrMasterCtrl(p.ctrl)
  val factory = BmbSlaveFactory(io.ctrl)
  val mapping = SpiXdrMasterCtrl.driveFrom(ctrl, factory)(p)
  if(p.xip != null) io.xip <> mapping.xip.xipBus
  io.spi <> ctrl.io.spi
  io.interrupt <> mapping.interruptCtrl.interrupt
}
