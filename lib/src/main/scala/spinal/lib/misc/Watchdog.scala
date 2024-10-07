package spinal.lib.misc

import spinal.core._
import spinal.core.fiber.{Handle, Unset}
import spinal.lib._
import spinal.lib.bus.bmb.{Bmb, BmbAccessCapabilities, BmbAccessParameter, BmbImplicitPeripheralDecoder, BmbInterconnectGenerator, BmbParameter, BmbSlaveFactory}
import spinal.lib.bus.misc.{BusSlaveFactory, SizeMapping}
import spinal.lib.generator.InterruptCtrlGeneratorI

class WatchdogParam(
  var prescalerWidth : Int,
  var timeoutWidth : Int,
  var counters : Int
)

class Watchdog(p : WatchdogParam) extends Area{
  val api = new Area {
    val enables = Bits(p.counters bits)
    val heartbeat = Bool()
    val panics = Bits(p.counters bits)
  }

  val prescaler = new Prescaler(p.prescalerWidth)
  prescaler.io.clear := api.heartbeat || api.enables === 0

  val counters =  List.tabulate(p.counters)(i => new Area{
    val clear = !api.enables(i) || api.heartbeat

    val timer = Timer(p.timeoutWidth)
    timer.io.tick := prescaler.io.overflow
    timer.io.clear := clear

    val full = RegInit(False) setWhen(timer.io.full) clearWhen(clear)
    api.panics(i) := full
  })



  def driveFrom(busCtrl: BusSlaveFactory, baseAddress: Int) = new Area {
    api.heartbeat := busCtrl.isWriting(baseAddress) && busCtrl.nonStopWrite(Bits(32 bits)) === 0xAD68E70Dl
    api.enables.setAsReg().init(0)
    busCtrl.setOnSet(api.enables, baseAddress + 0x04, 0)

    def lockedDrive[T <: Data](that : T, allowed : Bool, address : Int): T = {
      val reg = Reg(that)
      val driver = busCtrl.nonStopWrite(cloneOf(that))
      busCtrl.onWrite(address) {
        when(allowed) {
          reg := driver
        }
      }
      that := reg
      reg
    }

    lockedDrive(prescaler.io.limit, api.enables === 0, baseAddress + 0x40) init(0)
    for(i <- 0 until p.counters){
      lockedDrive(counters(i).timer.io.limit, !api.enables(i), baseAddress + 0x80 + i*4) init(0)
      busCtrl.read(counters(i).timer.io.value, baseAddress + 0xC0 + i*4)
    }
  }
}


object BmbWatchdog{
  def getBmbCapabilities(accessSource : BmbAccessCapabilities) = BmbSlaveFactory.getBmbCapabilities(
    accessSource,
    addressWidth = addressWidth,
    dataWidth = 32
  )
  def addressWidth = 8
}

case class BmbWatchdog(p : WatchdogParam, bmbParameter: BmbParameter) extends Component{
  val io = new Bundle{
    val bus =  slave(Bmb(bmbParameter))
    val panics = out Bits(p.counters bits)
    val heartBeat = in Bool() default(False)
  }
  val wd = new Watchdog(p)
  val busCtrl = BmbSlaveFactory(io.bus)
  wd.driveFrom(busCtrl, 0)
  wd.api.heartbeat setWhen(io.heartBeat)
  io.panics <> wd.api.panics
}

case class BmbWatchdogGenerator(apbOffset : Handle[BigInt] = Unset)
                               (implicit interconnect: BmbInterconnectGenerator, decoder : BmbImplicitPeripheralDecoder = null) extends Area {
  val parameter          = Handle[WatchdogParam]
  val accessSource       = Handle[BmbAccessCapabilities]
  val accessCapabilities = Handle(BmbWatchdog.getBmbCapabilities(accessSource))
  val accessRequirements = Handle[BmbAccessParameter]

  val logic     = Handle(BmbWatchdog(parameter, accessRequirements.toBmbParameter()))
  val ctrl      = Handle(logic.io.bus)
  val panics    = Handle(logic.io.panics.asBools.map(v => Handle(v)))

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
    for(i <- 0 until parameter.counters) {
      ctrl.addInterrupt(panics(i), id + i)
    }
  }
}


