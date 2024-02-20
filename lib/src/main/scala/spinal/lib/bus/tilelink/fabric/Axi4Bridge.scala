package spinal.lib.bus.tilelink.fabric

import spinal.core._
import spinal.core.fiber._
import spinal.lib.bus.amba4.axi.Axi4
import spinal.lib.bus.misc.SizeMapping
import spinal.lib.bus.tilelink
import spinal.lib.bus.tilelink.{Axi4Bridge, S2mSupport}
import spinal.lib.system.tag.{MappedNode, MemoryConnection, MemoryTransferTag, MemoryTransfers}


class Axi4Bridge() extends Area{
  val up = Node.slave()
  val down = new Handle[Axi4] with SpinalTagReady

  new MemoryConnection {
    override def up = Axi4Bridge.this.up
    override def down = Axi4Bridge.this.down
    override def transformers = Nil
    override def mapping = SizeMapping(0, BigInt(1) << Axi4Bridge.this.up.m2s.parameters.addressWidth)
    populate()
  }

  down.addTag(new MemoryTransferTag {
    override def get = up.m2s.parameters.emits
  })


  val logic = Fiber build new Area{
    up.m2s.supported load tilelink.Axi4Bridge.getSupported(up.m2s.proposed)
    up.s2m.none()

    val bridge = new tilelink.Axi4Bridge(up.bus.p.node)
    bridge.io.up << up.bus
    down.load(cloneOf(bridge.io.down))
    bridge.io.down >> down
  }
}
