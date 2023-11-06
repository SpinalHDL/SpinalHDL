package spinal.lib.misc.pipeline

import spinal.core._
import spinal.idslplugin.Location
import spinal.lib._

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class CombStage(val up : Node, val down : Node) extends Connector {
  down.up = this
  up.down = this

  override def ups: Seq[Node] = List(up)
  override def downs: Seq[Node] = List(down)

  override def propagateDown(): Unit = propagateDownAll()
  override def propagateUp(): Unit = {
    propagateUpAll()
    up.ctrl.removeSeed = down.ctrl.removeSeed
  }

  override def build(): Unit = {
    down.valid := up.valid
    up.ready := down.ready
    val matches = down.fromUp.payload.intersect(up.fromDown.payload)
    for(m <- matches){
      down(m) := up(m)
    }
  }
}