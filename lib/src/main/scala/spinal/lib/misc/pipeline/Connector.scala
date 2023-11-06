package spinal.lib.misc.pipeline

import spinal.core._

trait Connector extends Area{
  def ups : Seq[Node]
  def downs : Seq[Node]

  def propagateDown(): Unit
  def propagateUp(): Unit
  def build() : Unit

  def propagateDownAll(): Unit = {
    assert(ups.size == 1)
    assert(downs.size == 1)
    val up = ups(0)
    val down = downs(0)
    down.fromUp.payload ++= up.fromUp.payload
    down.fromUp.payload ++= up.keyToData.keys
    down.alwaysValid = up.alwaysValid
  }
  def propagateUpAll(): Unit = {
    assert(ups.size == 1)
    assert(downs.size == 1)
    val up = ups(0)
    val down = downs(0)
    up.fromDown.payload ++= down.fromDown.payload
    up.fromDown.payload ++= down.keyToData.keys
    up.alwaysReady = down.alwaysReady
  }
}