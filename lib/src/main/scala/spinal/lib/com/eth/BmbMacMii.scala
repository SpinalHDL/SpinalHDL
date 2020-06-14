package spinal.lib.com.eth

import spinal.core._
import spinal.lib._
import spinal.lib.bus.bmb._

object BmbMacMii{
  def getBmbCapabilities(accessSource : BmbAccessParameter) = BmbSlaveFactory.getBmbCapabilities(
    accessSource,
    addressWidth = addressWidth,
    dataWidth = 32
  )
  def addressWidth = 6
}

case class BmbMacMii(p : MacMiiParameter, bmbParameter: BmbParameter) extends Component{
  val io = new Bundle{
    val bus =  slave(Bmb(bmbParameter))
    val mii = master(Mii(p.mii))
    val interrupt = out Bool
  }

  val mac = new MacMii(p)
  io.mii <> mac.io.mii

  val busCtrl = BmbSlaveFactory(io.bus)
  val bridge = mac.io.ctrl.driveFrom(busCtrl)
  io.interrupt := bridge.interruptCtrl.pending
}

