package spinal.lib.bus.tilelink.fabric

import spinal.core._
import spinal.core.fiber._
import spinal.lib._
import spinal.lib.bus.tilelink._
import spinal.lib.bus.tilelink.coherent.OrderingCmd
import spinal.lib.system.tag.PMA

class RamFiber(var bytes : BigInt) extends Area{
  val up = Node.up()
  up.addTag(PMA.MAIN)
  up.addTag(PMA.EXECUTABLE)

  val thread = Fiber build new Area{
    up.m2s.supported load up.m2s.proposed.intersect(M2sTransfers.allGetPut).copy(addressWidth = log2Up(bytes))
    up.s2m.none()

    val logic = new Ram(up.bus.p.node, bytes toInt)
    logic.io.up << up.bus
  }
}
