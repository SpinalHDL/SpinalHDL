package spinal.lib.misc.pipeline

import spinal.core._
import spinal.idslplugin.Location
import spinal.lib._

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object ForkLink{
  def apply(up : Node, downs : Seq[Node], synchronous: Boolean = false) : ForkLink = new ForkLink(up, downs, synchronous)
}

class ForkLink(val up : Node, override val downs : Seq[Node], synchronous: Boolean = false) extends Link {
  this.downs.foreach(_.up = this)
  up.down = this

  override def ups: Seq[Node] = List(up)

  override def propagateDown(): Unit = {
    propagateDownAll()
    downs.foreach(_.valid)
    if(!synchronous) downs.foreach(_.ctrl.forgetOneSupported = true)
  }
  override def propagateUp(): Unit = {
    propagateUpAll()
    up.ready
  }

  override def build(): Unit = {
    for(down <- downs) {
      for (m <- down.fromUp.payload.intersect(up.fromDown.payload)) {
        down(m) := up(m)
      }
    }

    val linkEnable = (!synchronous) generate Vec(RegInit(True), downs.size).setCompositeName(this, "linkEnable")
    if (synchronous) {
      up.ready := downs.map(_.ready).reduce(_ && _)
      downs.foreach(_.valid := up.isValid && up.ready)
      assert(downs.forall(_.ctrl.forgetOne.isEmpty))
    } else {
      /* Ready is true when every output stream takes or has taken its value */
      up.ready := True
      for (i <- 0 until downs.size) {
        when(!downs(i).ready && linkEnable(i)) {
          up.ready := False
        }
      }

      /* downs are valid if the up is valid and they haven't taken their value yet.
       * When an output fires, mark its value as taken. */
      for (i <- 0 until downs.size) {
        downs(i).valid := up.isValid && linkEnable(i)
        when(downs(i).isMoving) {
          linkEnable(i) := False
        }
      }

      /* Reset the storage for each new value */
      when(up.ready) {
        linkEnable.foreach(_ := True)
      }
    }
  }
}
