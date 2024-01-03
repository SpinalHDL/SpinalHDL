package spinal.lib.misc.pipeline

import spinal.core._
import spinal.idslplugin.Location
import spinal.lib._

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer


object StageLink{
  def apply(up : Node, down : Node) = new StageLink(up, down)
}

class StageLink(val up : Node, val down : Node) extends Link {
  down.up = this
  up.down = this

  var holdPayload = false
  var collapseBubble = true

  def withoutCollapse() : this.type = {
    collapseBubble = false
    this
  }
  def withPayloadHold() : this.type = {
    holdPayload = true
    this
  }

  override def ups: Seq[Node] = List(up)
  override def downs: Seq[Node] = List(down)

  override def propagateDown(): Unit = {
    propagateDownAll()
    if(up.ctrl.valid.nonEmpty) down.valid
    down.ctrl.forgetOneSupported = true
  }
  override def propagateUp(): Unit = {
    propagateUpAll()
    if(down.ctrl.ready.nonEmpty) up.ready
  }

  override def build(): Unit = {
    val matches = down.fromUp.payload.intersect(up.fromDown.payload)
    if(down.ctrl.valid.nonEmpty) down.valid.setAsReg() init (False)
    matches.foreach(p => down(p).setAsReg())

    down.ctrl.forgetOne foreach  { cond =>  down.valid clearWhen(cond) }

    up.ctrl.ready.isEmpty match {
      case true =>
        if(down.ctrl.valid.nonEmpty) down.valid := up.isValid
        matches.foreach(p => down(p) := up(p))
      case false => {
        if(down.ctrl.valid.nonEmpty) when(up.isReady) {
          down.valid := up.isValid
        }
        when(if (holdPayload) up.isValid && up.isReady else up.isReady) {
          matches.foreach(p => down(p) := up(p))
        }
      }
    }

    if (up.ctrl.ready.nonEmpty) {
      up.ready := down.ready
      if (collapseBubble) up.ready setWhen (!down.isValid)
    }
  }
}