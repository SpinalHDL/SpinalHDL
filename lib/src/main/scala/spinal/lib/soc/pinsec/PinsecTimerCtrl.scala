package spinal.lib.soc.pinsec

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba3.apb.{Apb3SlaveFactory, Apb3Config, Apb3}
import spinal.lib.misc.{InterruptCtrl, Timer, Prescaler}

/**
 * Created by PIC32F_USER on 25/08/2016.
 */
object PinsecTimerCtrl{
  def getApb3Config() = new Apb3Config(
    addressWidth = 8,
    dataWidth = 32
  )
}

case class PinsecTimerCtrlExternal() extends Bundle{
  val clear = Bool
  val tick = Bool
}

case class PinsecTimerCtrl() extends Component {
  val io = new Bundle{
    val apb = slave(Apb3(PinsecTimerCtrl.getApb3Config()))
    val external = in(PinsecTimerCtrlExternal())
    val interrupt = out Bool
  }
  val external = BufferCC(io.external)

  val prescaler = Prescaler(16)
  val timerA = Timer(32)
  val timerB,timerC,timerD = Timer(16)

  val busCtrl = Apb3SlaveFactory(io.apb)
  val prescalerBridge = prescaler.driveFrom(busCtrl,0x00)

  val timerABridge = timerA.driveFrom(busCtrl,0x40)(
    ticks  = List(True, prescaler.io.overflow),
    clears = List(timerA.io.full)
  )

  val timerBBridge = timerB.driveFrom(busCtrl,0x50)(
    ticks  = List(True, prescaler.io.overflow, external.tick),
    clears = List(timerB.io.full, external.clear)
  )

  val timerCBridge = timerC.driveFrom(busCtrl,0x60)(
    ticks  = List(True, prescaler.io.overflow, external.tick),
    clears = List(timerC.io.full, external.clear)
  )

  val timerDBridge = timerD.driveFrom(busCtrl,0x70)(
    ticks  = List(True, prescaler.io.overflow, external.tick),
    clears = List(timerD.io.full, external.clear)
  )

  val interruptCtrl = InterruptCtrl(4)
  val interruptCtrlBridge = interruptCtrl.driveFrom(busCtrl,0x10)
  interruptCtrl.io.inputs(0) := timerA.io.full
  interruptCtrl.io.inputs(1) := timerB.io.full
  interruptCtrl.io.inputs(2) := timerC.io.full
  interruptCtrl.io.inputs(3) := timerD.io.full
  io.interrupt := interruptCtrl.io.pendings.orR
}
