package spinal.lib.misc.pipeline

import spinal.core._
import spinal.lib._

object JoinLink{
  def apply(ups : Seq[Node], down : Node) : JoinLink = new JoinLink(ups, down)
}
class JoinLink(override val ups : Seq[Node], val down : Node) extends Link {
  this.ups.foreach(_.down = this)
  down.up = this

  override def downs: Seq[Node] = List(down)

  override def propagateDown(): Unit = propagateDownAll()
  override def propagateUp(): Unit = propagateUpAll()

  override def build(): Unit = {
    assert(down.ctrl.forgetOne.isEmpty)

    down.valid := ups.map(_.valid).andR
    ups.foreach(_.ready := down.valid && down.ready)
    for(key <- down.fromUp.payload){
      val filtred = ups.filter(up => up.keyToData.contains(key) || up.fromUp.payload.contains(key))
      filtred.size match {
        case 1 => down(key) := filtred(0)(key)
      }
    }
  }
}
