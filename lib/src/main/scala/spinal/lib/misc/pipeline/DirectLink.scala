package spinal.lib.misc.pipeline

import spinal.core._
import spinal.idslplugin.Location
import spinal.lib._

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object DirectLink{
  def apply(up : Node, down : Node) : DirectLink = new DirectLink(up, down)
}
class DirectLink(val up : Node, val down : Node) extends Link {
  down.up = this
  up.down = this

  override def ups: Seq[Node] = List(up)
  override def downs: Seq[Node] = List(down)

  override def propagateDown(): Unit = {
    propagateDownAll()
    if(up.alwaysValid) down.setAlwaysValid()
    up.ctrl.forgetOneSupported = true
  }
  override def propagateUp(): Unit = {
    propagateUpAll()
    down.ctrl.forgetOne.foreach(_ => up.ctrl.forgetOneCreate())
    if(down.ctrl.cancel.nonEmpty) up.cancel
    if(down.alwaysReady) up.setAlwaysReady()
  }

  override def build(): Unit = {
    if(!down.alwaysValid) down.valid := up.valid
    if(!up.alwaysReady) up.ready := down.ready
    up.ctrl.forgetOne.foreach(_ := down.ctrl.forgetOne.get)
    up.ctrl.cancel.foreach(_ := down.ctrl.cancel.get)
    Link.connectDatas(up, down)
  }
}