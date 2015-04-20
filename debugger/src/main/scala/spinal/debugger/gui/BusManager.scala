package spinal.debugger.gui

import scala.collection.mutable.ArrayBuffer


object BusManager {
  sealed trait BusManagerStates
  case object Boot extends BusManagerStates
  case object WaitPassport extends BusManagerStates
  case object Running extends BusManagerStates
}

class BusManager(hal: BytePacketHal, guiTreeViewManager: IGuiTreeViewManager) {
  implicit def b(x: Int) = x.toByte

  val thread = new Thread with IBytePacketHalObserver {
    import BusManager._
    var state : BusManagerStates = Boot
    val passports = ArrayBuffer[Seq[Byte]]()
    hal.setObserver(this)
    hal.open

    override def run {
      state = WaitPassport
      passportCall
      Thread.sleep(400)
      for(passport <- passports){
        guiTreeViewManager.add(Seq(passport.mkString(" ")))
      }
      state = Running
    }
    override def packetHalEvent(in: Seq[Byte]): Unit = {
      state match{
        case Boot =>
        case WaitPassport => {
          passports += in
        }
        case _ =>
      }
    }

    def passportCall: Unit = {
      hal.tx(Seq(0xFF, 0xFF, 0xFF, 0xFF))
    }
  }

  thread.start
}
