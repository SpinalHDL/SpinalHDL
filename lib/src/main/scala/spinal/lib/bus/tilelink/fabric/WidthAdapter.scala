package spinal.lib.bus.tilelink.fabric

import spinal.core._
import spinal.core.fiber.Fiber
import spinal.lib.bus.misc.SizeMapping
import spinal.lib.bus.tilelink
import spinal.lib.bus.tilelink._
import spinal.lib.system.tag.{MappedNode, MemoryConnection, MemoryTransfers}

object WidthAdapter{
  def apply() : WidthAdapter = new WidthAdapter()
}
class WidthAdapter() extends Area{
  val up = Node.slave()
  val down = Node.master()

  new MemoryConnection {
    override def up = WidthAdapter.this.up
    override def down = WidthAdapter.this.down
    override def transformers = Nil
    override def mapping = SizeMapping(0, BigInt(1) << WidthAdapter.this.up.m2s.parameters.addressWidth)
    populate()
  }

  val logic = Fiber build new Area{
    down.m2s.proposed.load(up.m2s.proposed)
    up.m2s.supported load down.m2s.supported.copy(
      dataWidth = up.m2s.proposed.dataWidth
    )
    down.m2s.parameters load up.m2s.parameters.copy(
      dataWidth = down.m2s.supported.dataWidth
    )

    up.s2m.from(down.s2m)

    val bridge = new tilelink.WidthAdapter(up.bus.p, down.bus.p)
    bridge.io.up << up.bus
    bridge.io.down >> down.bus
  }
}
