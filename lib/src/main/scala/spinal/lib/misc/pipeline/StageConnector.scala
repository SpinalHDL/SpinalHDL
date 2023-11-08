package spinal.lib.misc.pipeline

import spinal.core._
import spinal.idslplugin.Location
import spinal.lib._

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer


object StageConnector{
  def apply(up : Node, down : Node) = new StageConnector(up, down)
}

class StageConnector(val up : Node, val down : Node) extends Connector {
  down.up = this
  up.down = this

  var holdPayload = false
  var collapseBubble = true

  override def ups: Seq[Node] = List(up)
  override def downs: Seq[Node] = List(down)

  override def propagateDown(): Unit = {
    propagateDownAll()
  }
  override def propagateUp(): Unit = {
    propagateUpAll()
    if(down.alwaysReady) up.setAlwaysReady()
  }

  override def build(): Unit = {
    val matches = down.fromUp.payload.intersect(up.fromDown.payload)
    val s = down
    val m = up
    if(!s.alwaysValid) s.valid.setAsReg() init (False)
    matches.foreach(p => s(p).setAsReg())

    down.ctrl.removeSeed foreach  { cond =>  s.valid clearWhen(cond) }

    m.alwaysReady match {
      case true =>
        if(!s.alwaysValid) s.valid := m.valid
        matches.foreach(p => s(p) := m(p))
      case false => {
        if(!s.alwaysValid) when(m.ready) {
          s.valid := m.valid
        }
        when(if (holdPayload) m.valid && m.ready else m.ready) {
          matches.foreach(p => s(p) := m(p))
        }
      }
    }

    if (!m.alwaysReady) {
      m.ready := s.ready
      if (collapseBubble) m.ready setWhen (!s.valid)
    }
  }
}