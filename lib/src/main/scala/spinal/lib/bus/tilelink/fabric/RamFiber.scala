package spinal.lib.bus.tilelink.fabric

import spinal.core._
import spinal.core.fiber._
import spinal.lib._
import spinal.lib.bus.tilelink._
import spinal.lib.bus.tilelink.coherent.OrderingCmd
import spinal.lib.system.tag.PMA

class RamFiber() extends Area{
  val up = Node.up()
  up.addTag(PMA.MAIN)

  val thread = Fiber build new Area{
    up.m2s.supported load up.m2s.proposed.intersect(M2sTransfers.allGetPut)
    up.s2m.none()

    val bytes = up.ups.map(e => e.mapping.value.highestBound - e.mapping.value.lowerBound + 1).max.toInt
    val logic = new Ram(up.bus.p.node, bytes)
    logic.io.up << up.bus
  }
}
