package spinal.lib.misc

import spinal.core._
import spinal.core.fiber.Fiber
import spinal.lib._
import spinal.lib.bus.amba3.apb.{Apb3, Apb3SlaveFactory}
import spinal.lib.bus.amba4.axilite.{AxiLite4, AxiLite4Config, AxiLite4SlaveFactory}
import spinal.lib.bus.bmb.{Bmb, BmbAccessCapabilities, BmbAccessParameter, BmbParameter, BmbSlaveFactory}
import spinal.lib.bus.misc.BusSlaveFactory
import spinal.lib.bus.wishbone.{Wishbone, WishboneConfig, WishboneSlaveFactory}

import scala.collection.mutable.ArrayBuffer

object Clint{
  def getWisboneConfig() = WishboneConfig(
    addressWidth = addressWidth-2,
    dataWidth = 32
  )

  def getBmbCapabilities(accessSource : BmbAccessCapabilities) = BmbSlaveFactory.getBmbCapabilities(
    accessSource,
    addressWidth = addressWidth,
    dataWidth = 32
  )
  def getTilelinkSupport(proposed : bus.tilelink.M2sSupport) = bus.tilelink.SlaveFactory.getSupported(
    addressWidth = addressWidth,
    dataWidth = 32,
    proposed
  )
  def addressWidth = 16
}

case class Clint(hartCount : Int) extends Area{
  val stop = False
  val time = Reg(UInt(64 bits)) init(0)
  when(!stop){
    time := time + 1
  }

  val harts = for(hartId <- 0 until hartCount) yield new Area{
    val cmp = Reg(UInt(64 bits))
    val timerInterrupt = RegNext(time >= cmp)
    val softwareInterrupt = RegInit(False)
  }

  def driveFrom(bus : BusSlaveFactory, bufferTime : Boolean = false) = new Area{
    val IPI_ADDR = 0x0000
    val CMP_ADDR = 0x4000
    val TIME_ADDR = 0xBFF8

    bufferTime match {
      case false => bus.readMultiWord(time, TIME_ADDR)
      case true => new Composite(this){
        assert(bus.busDataWidth == 32)

        val timeMsb = RegNextWhen(time(63 downto 32), bus.isReading(TIME_ADDR))
        bus.read(time(31 downto 0), TIME_ADDR)
        bus.read(timeMsb, TIME_ADDR + 4)
      }
    }

    val hartsMapping = for(hartId <- 0 until hartCount) yield new Area{
      bus.writeMultiWord(harts(hartId).cmp, CMP_ADDR + 8*hartId)
      bus.readAndWrite(harts(hartId).softwareInterrupt, IPI_ADDR + 4*hartId, bitOffset = 0)
    }
  }
}


case class Apb3Clint(hartCount : Int) extends Component{
  val io = new Bundle {
    val bus = slave(Apb3(16, 32))
    val timerInterrupt = out Bits(hartCount bits)
    val softwareInterrupt = out Bits(hartCount bits)
    val time = out UInt(64 bits)
  }

  val factory = Apb3SlaveFactory(io.bus)
  val logic = Clint(hartCount)
  logic.driveFrom(factory)

  for(hartId <- 0 until hartCount){
    io.timerInterrupt(hartId) := logic.harts(hartId).timerInterrupt
    io.softwareInterrupt(hartId) := logic.harts(hartId).softwareInterrupt
  }

  io.time := logic.time
}


case class WishboneClint(hartCount : Int) extends Component{
  val io = new Bundle {
    val bus = slave(Wishbone(WishboneConfig(16-2, 32)))
    val timerInterrupt = out Bits(hartCount bits)
    val softwareInterrupt = out Bits(hartCount bits)
    val time = out UInt(64 bits)
  }

  val factory = WishboneSlaveFactory(io.bus)
  val logic = Clint(hartCount)
  logic.driveFrom(factory)

  for(hartId <- 0 until hartCount){
    io.timerInterrupt(hartId) := logic.harts(hartId).timerInterrupt
    io.softwareInterrupt(hartId) := logic.harts(hartId).softwareInterrupt
  }

  io.time := logic.time
}

case class AxiLite4Clint(hartCount : Int, bufferTime : Boolean = false) extends Component{
  val io = new Bundle {
    val bus = slave(AxiLite4(AxiLite4Config(16, 32)))
    val timerInterrupt = out Bits(hartCount bits)
    val softwareInterrupt = out Bits(hartCount bits)
    val time = out UInt(64 bits)
  }

  val factory = new AxiLite4SlaveFactory(io.bus)
  val logic = Clint(hartCount)
  logic.driveFrom(factory, bufferTime)

  for(hartId <- 0 until hartCount){
    io.timerInterrupt(hartId) := logic.harts(hartId).timerInterrupt
    io.softwareInterrupt(hartId) := logic.harts(hartId).softwareInterrupt
  }

  io.time := logic.time
}

case class BmbClint(bmbParameter : BmbParameter, hartCount : Int) extends Component{
  val io = new Bundle {
    val bus = slave(Bmb(bmbParameter))
    val timerInterrupt = out Bits(hartCount bits)
    val softwareInterrupt = out Bits(hartCount bits)
    val time = out UInt(64 bits)
    val stop = in Bool() default(False)
  }

  val factory = BmbSlaveFactory(io.bus)
  val logic = Clint(hartCount)
  logic.driveFrom(factory)
  logic.stop setWhen(io.stop)

  for(hartId <- 0 until hartCount){
    io.timerInterrupt(hartId) := logic.harts(hartId).timerInterrupt
    io.softwareInterrupt(hartId) := logic.harts(hartId).softwareInterrupt
  }

  io.time := logic.time
}

class MappedClint[T <: spinal.core.Data with IMasterSlave](hartCount : Int,
                                                           bufferTime : Boolean,
                                                           busType: HardType[T],
                                                           factoryGen: T => BusSlaveFactory) extends Component{
  val io = new Bundle {
    val bus = slave(busType())
    val timerInterrupt = out Bits(hartCount bits)
    val softwareInterrupt = out Bits(hartCount bits)
    val time = out UInt(64 bits)
    val stop = in Bool() default(False)
  }

  val factory = factoryGen(io.bus)
  val logic = Clint(hartCount)
  logic.driveFrom(factory, bufferTime)
  logic.stop setWhen(io.stop)

  for(hartId <- 0 until hartCount){
    io.timerInterrupt(hartId) := logic.harts(hartId).timerInterrupt
    io.softwareInterrupt(hartId) := logic.harts(hartId).softwareInterrupt
  }

  io.time := logic.time
}

case class TilelinkClint(hartCount : Int, p : bus.tilelink.BusParameter) extends MappedClint[bus.tilelink.Bus](
  hartCount,
  true,
  new bus.tilelink.Bus(p),
  new bus.tilelink.SlaveFactory(_)
)

case class TilelinkFabricClint() extends Area{
  val node = bus.tilelink.fabric.Node.slave()

  var harts = ArrayBuffer[Any]()
  def bindHart(cpu : Any) = {
    //TODO
    harts += cpu
  }

  val thread = Fiber build new Area{
    node.m2s.supported.load(Clint.getTilelinkSupport(node.m2s.proposed))
    node.s2m.none()

    val core = TilelinkClint(harts.size, node.bus.p)
    core.io.bus <> node.bus
  }
}