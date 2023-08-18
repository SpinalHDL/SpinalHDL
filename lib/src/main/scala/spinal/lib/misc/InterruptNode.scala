package spinal.lib.misc

import spinal.core._
import spinal.lib._
import spinal.core.fiber._
import spinal.lib.BufferCC
import spinal.lib.bus.fabric.UpDown

import scala.collection.mutable.ArrayBuffer

object InterruptNode{
  def slave() = new InterruptNode().setSlaveOnly()
  def master() = new InterruptNode().setMasterOnly()
}

class InterruptNode extends Area with UpDown[InterruptNode]{
  val flag = Bool()
  val cd = ClockDomain.current
  val lock = Lock()

  def <<(source: InterruptNode): Unit = {
    this.ups += source
    source.downs += this
  }
  def >>(sink: InterruptNode): Unit = sink << this

  val thread = Fiber.build(new Area {
    lock.await()

    assertUpDown()

    val gateways = for (up <- ups) yield new Area {
      val flag = Bool()
      ClockDomain.areSynchronous(cd, up.cd) match {
        case true => flag := up.flag
        case false => BufferCC(up.flag, init = False)
      }
    }
    if(ups.nonEmpty) flag := gateways.map(_.flag).orR
  })
}
