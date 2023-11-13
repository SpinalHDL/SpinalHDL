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
    up.ctrl.forgetOneSupported = true
  }
  override def propagateUp(): Unit = {
    propagateUpAll()
    down.ctrl.forgetOne.foreach(_ => up.ctrl.forgetOneCreate())
    down.ctrl.cancel.foreach(_ => up.ctrl.cancelCreate())
    if(down.alwaysReady) up.setAlwaysReady()
  }

  override def build(): Unit = {
    if(!down.alwaysValid) down.valid := up.valid
    if(!up.alwaysReady) up.ready := down.ready
    up.ctrl.forgetOne.foreach(_ := down.ctrl.forgetOne.get)
    up.ctrl.cancel.foreach(_ := down.ctrl.cancel.get)
    Connector.connectDatas(up, down)
  }
}