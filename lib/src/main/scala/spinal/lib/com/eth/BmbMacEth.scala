package spinal.lib.com.eth

import spinal.core._
import spinal.lib._
import spinal.lib.bus.bmb._

object BmbMacEth{
  def getBmbCapabilities(accessSource : BmbAccessCapabilities) = BmbSlaveFactory.getBmbCapabilities(
    accessSource,
    addressWidth = addressWidth,
    dataWidth = 32
  )
  def addressWidth = 6
}

case class BmbMacEth(p : MacEthParameter,
                     bmbParameter: BmbParameter,
                     txCd : ClockDomain,
                     rxCd : ClockDomain) extends Component{
  val io = new Bundle{
    val bus =  slave(Bmb(bmbParameter))
    val phy = master(PhyIo(p.phy))
    val interrupt = out Bool
  }

  val mac = new MacEth(p, txCd, rxCd)
  io.phy <> mac.io.phy

  val busCtrl = BmbSlaveFactory(io.bus)
  val bridge = mac.io.ctrl.driveFrom(busCtrl)
  io.interrupt := bridge.interruptCtrl.pending
}

