package spinal.lib.misc.pipeline

import spinal.core._
import spinal.lib._

object JoinConnector{
  def apply(ups : Seq[Node], down : Node) : JoinConnector = new JoinConnector(ups, down)
}
class JoinConnector(override val ups : Seq[Node], val down : Node) extends Connector {
  this.ups.foreach(_.down = this)
  down.up = this

  override def downs: Seq[Node] = List(down)

  override def propagateDown(): Unit = propagateDownAll()
  override def propagateUp(): Unit = propagateUpAll()

  override def build(): Unit = {
    assert(down.ctrl.removeSeed.isEmpty)

    down.valid := ups.map(_.valid).andR
    ups.foreach(_.ready := down.isFireing)
    for(key <- down.fromUp.payload){
      val filtred = ups.filter(up => up.keyToData.contains(key) || up.fromUp.payload.contains(key))
      filtred.size match {
        case 1 => down(key) := filtred(0)(key)
      }
    }
  }
}
