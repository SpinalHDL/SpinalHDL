package spinal.lib.misc.pipeline

import spinal.core._


object S2mConnector{
  def apply(up : Node, down : Node) = new S2mConnector(up, down)
}

class S2mConnector(val up : Node, val down : Node) extends Connector {
  down.up = this
  up.down = this

  override def ups: Seq[Node] = List(up)
  override def downs: Seq[Node] = List(down)

  override def propagateDown(): Unit = propagateDownAll()
  override def propagateUp(): Unit = {
    propagateUpAll()
    if(down.ctrl.removeSeed.nonEmpty){
      up.ctrl.removeSeed = Some(Bool())
      up.ctrl.nameRemoveSeed()
    }
  }

  override def build(): Unit = {
    val matches = down.fromUp.payload.intersect(up.fromDown.payload)
    val s = down
    val m = up

    val rValid = RegInit(False) setWhen (m.valid) clearWhen (s.ready) setCompositeName(this, "rValid")
    val rData = matches.map(e => RegNextWhen(m(e), m.ready).setCompositeName(this, "s2mBuffer"))

    m.ready := !rValid

    s.valid := m.valid || rValid
    when(rValid) {
      (matches, rData).zipped.foreach(s(_) := _)
    } otherwise {
      matches.foreach(e => s(e) := m(e))
    }

    s.ctrl.removeSeed.foreach { cond =>
      rValid clearWhen(cond)
      m.ctrl.removeSeed.get := cond && !rValid
    }
  }
}