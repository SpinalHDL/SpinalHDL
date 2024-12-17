package spinal.lib.bus.tilelink.fabric

import spinal.core._
import spinal.core.fiber._
import spinal.lib.bus.amba3.apb._
import spinal.lib.bus.tilelink
import spinal.lib.system.tag.{MemoryConnection, MemoryTransferTag}


class Apb3BridgeFiber(var withAxi3 : Boolean = false) extends Area{
  val up = Node.slave()
  val down = new Handle[Apb3] with SpinalTagReady

  new MemoryConnection {
    override def up = Apb3BridgeFiber.this.up
    override def down = Apb3BridgeFiber.this.down
    override def transformers = Nil
    populate()
  }

  down.addTag(new MemoryTransferTag {
    override def get = up.m2s.parameters.emits
  })

  val logic = Fiber build new Area{
    up.m2s.supported load tilelink.Apb3Bridge.getSupported(up.m2s.proposed)
    up.s2m.none()

    val bridge = new tilelink.Apb3Bridge(up.bus.p.node)
    bridge.io.up << up.bus
    down.load(cloneOf(bridge.io.down))
    bridge.io.down >> down
  }
}
