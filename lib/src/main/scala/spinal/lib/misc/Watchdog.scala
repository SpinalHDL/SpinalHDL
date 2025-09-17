package spinal.lib.misc

import spinal.core._
import spinal.core.fiber.{Fiber, Handle, Unset}
import spinal.lib._
import spinal.lib.bus.bmb.{Bmb, BmbAccessCapabilities, BmbAccessParameter, BmbImplicitPeripheralDecoder, BmbInterconnectGenerator, BmbParameter, BmbSlaveFactory}
import spinal.lib.bus.misc.{BusSlaveFactory, SizeMapping}
import spinal.lib.bus.tilelink.fabric
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
    def checkKey(key : Long) : Bool = busCtrl.isWriting(baseAddress) && busCtrl.nonStopWrite(Bits(32 bits)) === key
    val unlocked = RegInit(True)
    unlocked setWhen(checkKey(0x3C21B925L))
    unlocked clearWhen(checkKey(0x3C21B924L))

    api.heartbeat := checkKey(0xAD68E70Dl)

    {// Drive enables
      api.enables.setAsReg().init(0)
      val bitSets = busCtrl.nonStopWrite(Bits(widthOf(api.enables) bits), 0)
      when(busCtrl.isWriting(baseAddress + 0x04) && unlocked) {
        for (i <- 0 until p.counters) {
          api.enables(i).setWhen(bitSets(i))
        }
      }
      when(busCtrl.isWriting(baseAddress + 0x08) && unlocked) {
        for (i <- 0 until p.counters) {
          api.enables(i).clearWhen(bitSets(i))
        }
      }
    }

    def lockedDrive[T <: Data](that : T, address : Int): T = {
      val reg = Reg(that)
      val driver = busCtrl.nonStopWrite(cloneOf(that))
      busCtrl.onWrite(address) {
        when(unlocked) {
          reg := driver
        }
      }
      that := reg
      reg
    }

    lockedDrive(prescaler.io.limit, baseAddress + 0x40) init(0)
    for(i <- 0 until p.counters){
      lockedDrive(counters(i).timer.io.limit, baseAddress + 0x80 + i*4) init(0)
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
  val driver = wd.driveFrom(busCtrl, 0)
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

import spinal.lib.bus.tilelink

object TilelinkWatchdog{
  def getSupported(proposed: tilelink.M2sSupport) = tilelink.SlaveFactory.getSupported(
    addressWidth = addressWidth,
    dataWidth = 32,
    allowBurst = false,
    proposed = proposed
  )
  def addressWidth = 8
}


case class TilelinkWatchdog(p : WatchdogParam, tlParam: tilelink.BusParameter) extends Component{
  val io = new Bundle{
    val bus =  slave(tilelink.Bus(tlParam))
    val panics = out Bits(p.counters bits)
    val heartBeat = in Bool() default(False)
  }
  val wd = new Watchdog(p)
  val busCtrl = new tilelink.SlaveFactory(io.bus, false)
  wd.driveFrom(busCtrl, 0)
  wd.api.heartbeat setWhen(io.heartBeat)
  io.panics <> wd.api.panics
}

case class TilelinkWatchdogFiber(p : WatchdogParam) extends Area{
  val ctrl = fabric.Node.up()
  val interrupts = List.fill(p.counters)(InterruptNode.master())

  val logic = Fiber build new Area{
    ctrl.m2s.supported.load(TilelinkWatchdog.getSupported(ctrl.m2s.proposed))
    ctrl.s2m.none()

    val core = TilelinkWatchdog(p, ctrl.bus.p)
    core.io.bus <> ctrl.bus
    for(i <- 0 until p.counters) interrupts(i).flag := core.io.panics(i)
  }
}



