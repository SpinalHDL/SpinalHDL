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

    val rValid = RegInit(False) setWhen (up.valid) clearWhen (down.ready) setCompositeName(this, "rValid")
    val rData = matches.map(e => RegNextWhen(up(e), up.ready).setCompositeName(this, "s2mBuffer"))

    up.ready := !rValid

    down.valid := up.valid || rValid
    when(rValid) {
      (matches, rData).zipped.foreach(down(_) := _)
    } otherwise {
      matches.foreach(e => down(e) := up(e))
    }

    down.ctrl.removeSeed.foreach { cond =>
      rValid clearWhen(cond)
      up.ctrl.removeSeed.get := cond && !rValid
    }
  }
}