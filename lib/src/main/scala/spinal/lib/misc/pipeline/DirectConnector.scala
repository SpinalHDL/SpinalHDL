package spinal.lib.misc.pipeline

import spinal.core._
import spinal.idslplugin.Location
import spinal.lib._

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object DirectConnector{
  def apply(up : Node, down : Node) : DirectConnector = new DirectConnector(up, down)
}
class DirectConnector(val up : Node, val down : Node) extends Connector {
  down.up = this
  up.down = this

  override def ups: Seq[Node] = List(up)
  override def downs: Seq[Node] = List(down)

  override def propagateDown(): Unit = {
    propagateDownAll()
    if(up.alwaysValid) down.setAlwaysValid()
  }
  override def propagateUp(): Unit = {
    propagateUpAll()
    down.ctrl.removeSeed.foreach(_ => up.ctrl.removeSeed = Some(Bool()))
    up.ctrl.nameRemoveSeed()

    if(down.alwaysReady) up.setAlwaysReady()
  }

  override def build(): Unit = {
    if(!down.alwaysValid) down.valid := up.valid
    if(!up.alwaysReady) up.ready := down.ready
    val matches = down.fromUp.payload.intersect(up.fromDown.payload)
    for(m <- matches){
      down(m) := up(m)
    }
  }
}