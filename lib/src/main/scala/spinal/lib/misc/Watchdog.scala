package spinal.lib.misc

import spinal.core._
import spinal.core.fiber.{Handle, Unset}
import spinal.lib._
import spinal.lib.bus.bmb.{Bmb, BmbAccessCapabilities, BmbAccessParameter, BmbImplicitPeripheralDecoder, BmbInterconnectGenerator, BmbParameter, BmbSlaveFactory}
import spinal.lib.bus.misc.{BusSlaveFactory, SizeMapping}
import spinal.lib.generator.InterruptCtrlGeneratorI

class WatchdogParam(
  var prescalerWidth : Int,
  var timeoutWidth : Int
)

class Watchdog(p : WatchdogParam) extends Area{
  val api = new Area {
    val enable, softPanicEnable, hardPanicEnable = Bool()
    val heartbeat = Bool()
    val softPanic, hardPanic = RegInit(False).randBoot()
  }

  val prescaler = new Prescaler(p.prescalerWidth)
  val softTimer = new Timer(p.timeoutWidth)
  val hardTimer = new Timer(p.timeoutWidth)

  val softPanic, hardPanic = RegInit(False).randBoot()
  api.softPanic := softPanic && api.softPanicEnable
  api.hardPanic := hardPanic && api.hardPanicEnable

  softPanic setWhen(softTimer.io.full)
  hardPanic setWhen(hardTimer.io.full)

  val clear = !api.enable || api.heartbeat
  prescaler.io.clear := clear
  softTimer.io.clear := clear
  hardTimer.io.clear := clear
  softPanic clearWhen(clear)
  hardPanic clearWhen(clear)

  softTimer.io.tick := prescaler.io.overflow
  hardTimer.io.tick := prescaler.io.overflow && api.softPanic

  def driveFrom(busCtrl: BusSlaveFactory, baseAddress: Int) = new Area {
    api.heartbeat := busCtrl.isWriting(baseAddress) && busCtrl.nonStopWrite(Bits(32 bits)) === 0xAD68E70Dl
    List(api.softPanicEnable, api.hardPanicEnable, api.enable).foreach(_.setAsReg() init (False))
    busCtrl.setOnSet(api.enable   ,baseAddress + 0x4, 0)
    busCtrl.setOnSet(api.softPanicEnable,baseAddress + 0x4, 1)
    busCtrl.setOnSet(api.hardPanicEnable,baseAddress + 0x4, 2)
    busCtrl.drive(softTimer.io.limit, baseAddress + 0x10)
    busCtrl.drive(hardTimer.io.limit, baseAddress + 0x14)
    busCtrl.drive(prescaler.io.limit, baseAddress + 0x18)
    busCtrl.read(softTimer.io.value, baseAddress + 0x20)
    busCtrl.read(hardTimer.io.value, baseAddress + 0x24)
  }
}


object BmbWatchdog{
  def getBmbCapabilities(accessSource : BmbAccessCapabilities) = BmbSlaveFactory.getBmbCapabilities(
    accessSource,
    addressWidth = addressWidth,
    dataWidth = 32
  )
  def addressWidth = 6
}

case class BmbWatchdog(p : WatchdogParam, bmbParameter: BmbParameter) extends Component{
  val io = new Bundle{
    val bus =  slave(Bmb(bmbParameter))
    val softPanic, hardPanic = out Bool()
    val heartBeat = in Bool() default(False)
  }
  val wd = new Watchdog(p)
  val busCtrl = BmbSlaveFactory(io.bus)
  wd.driveFrom(busCtrl, 0)
  wd.api.heartbeat setWhen(io.heartBeat)
  io.softPanic <> wd.api.softPanic
  io.hardPanic <> wd.api.hardPanic
}

case class BmbWatchdogGenerator(apbOffset : Handle[BigInt] = Unset)
                               (implicit interconnect: BmbInterconnectGenerator, decoder : BmbImplicitPeripheralDecoder = null) extends Area {
  val parameter          = Handle[WatchdogParam]
  val accessSource       = Handle[BmbAccessCapabilities]
  val accessCapabilities = Handle(BmbWatchdog.getBmbCapabilities(accessSource))
  val accessRequirements = Handle[BmbAccessParameter]

  val logic     = Handle(BmbWatchdog(parameter, accessRequirements.toBmbParameter()))
  val ctrl      = Handle(logic.io.bus)
  val softPanic = Handle(logic.io.softPanic)
  val hardPanic = Handle(logic.io.hardPanic)

  interconnect.addSlave(
    accessSource       = accessSource,
    accessCapabilities = accessCapabilities,
    accessRequirements = accessRequirements,
    bus                = ctrl,
    mapping            = Handle(SizeMapping(apbOffset, 1 << BmbWatchdog.addressWidth))
  )
  sexport(parameter)
  if(decoder != null) interconnect.addConnection(decoder.bus, ctrl)

  def connectInterrupt(ctrl : InterruptCtrlGeneratorI, id : Int): Unit = {
    ctrl.addInterrupt(softPanic, id)
  }
}


