package spinal.lib.misc.plic

import spinal.core._
import spinal.core.fiber._
import spinal.lib.bus
import spinal.lib.bus.amba4.axilite._
import spinal.lib._
import spinal.lib.bus.misc.BusSlaveFactory
import spinal.lib.misc.InterruptNode

import scala.collection.mutable.ArrayBuffer
import scala.collection.Seq

class MappedPlic[T <: spinal.core.Data with IMasterSlave](sourceIds : Seq[Int],
                                                          targetIds : Seq[Int],
                                                          busType: HardType[T],
                                                          factoryGen: T => BusSlaveFactory) extends Component{
  val priorityWidth = 2
  val plicMapping = PlicMapping.sifive

  import plicMapping._

  val io = new Bundle {
    val bus = slave(busType())
    val sources = in Bits (sourceIds.size bits)
    val targets = out Bits (targetIds.size bits)
  }

  val gateways = (for ((source, id) <- (io.sources.asBools, sourceIds).zipped) yield PlicGatewayActiveHigh(
    source = source,
    id = id,
    priorityWidth = priorityWidth
  )).toSeq

  val targets = for (i <- targetIds) yield PlicTarget(
    id = i,
    gateways = gateways,
    priorityWidth = priorityWidth
  )

  io.targets := targets.map(_.iep).asBits

  val factory = factoryGen(io.bus)
  val mapping = PlicMapper(factory, plicMapping)(
    gateways = gateways,
    targets = targets
  )
}

object TilelinkPlic{
  def getTilelinkSupport(proposed: bus.tilelink.M2sSupport) = bus.tilelink.SlaveFactory.getSupported(
    addressWidth = addressWidth,
    dataWidth = 32,
    allowBurst = false,
    proposed
  )

  def addressWidth = 22
}



class TilelinkPlic(p : bus.tilelink.BusParameter,
                   sourceIds : Seq[Int],
                   targetIds : Seq[Int]) extends MappedPlic[bus.tilelink.Bus](
  sourceIds,
  targetIds,
  new bus.tilelink.Bus(p),
  new bus.tilelink.SlaveFactory(_, false)
)

trait InterruptCtrlFiber {
  val lock = Lock()

  def createInterruptMaster(id: Int): InterruptNode
  def createInterruptSlave(id: Int): InterruptNode

  def retain() = lock.retain()
  def release() = lock.release()
}

case class TilelinkPlicFiber() extends Area with InterruptCtrlFiber{
  val node = bus.tilelink.fabric.Node.slave()


  case class TargetSpec(node : InterruptNode, id : Int)
  case class GatewaySpec(node: InterruptNode, id: Int)

  val targetsSpecs = ArrayBuffer[TargetSpec]()
  val gatewaySpecs = ArrayBuffer[GatewaySpec]()


  override def createInterruptMaster(id : Int) : InterruptNode = {
    val spec = node.clockDomain on TargetSpec(InterruptNode.master(), id)
    targetsSpecs += spec
    spec.node
  }

  override def createInterruptSlave(id: Int) : InterruptNode = {
    val spec = node.clockDomain on GatewaySpec(InterruptNode.slave(), id)
    gatewaySpecs += spec
    spec.node
  }

  val thread = Fiber build new Area{
    lock.await()

    node.m2s.supported.load(TilelinkPlic.getTilelinkSupport(node.m2s.proposed))
    node.s2m.none()

    val logic = new TilelinkPlic(
      node.bus.p,
      gatewaySpecs.map(_.id),
      targetsSpecs.map(_.id)
    )

    logic.io.bus <> node.bus
    (logic.io.sources.asBools, gatewaySpecs).zipped.foreach(_ := _.node.flag)
    (targetsSpecs, logic.io.targets.asBools).zipped.foreach(_.node.flag := _)
  }
}