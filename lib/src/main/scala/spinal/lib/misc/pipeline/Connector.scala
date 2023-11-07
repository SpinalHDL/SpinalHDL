package spinal.lib.misc.pipeline

import spinal.core._

trait Connector extends Area{
  def ups : Seq[Node]
  def downs : Seq[Node]

  def propagateDown(): Unit
  def propagateUp(): Unit
  def build() : Unit

  def propagateDownAll(): Unit = {
    for(up <- ups; down <- downs) {
      down.fromUp.payload ++= up.fromUp.payload
      down.fromUp.payload ++= up.keyToData.keys
      down.alwaysValid = up.alwaysValid
    }
  }
  def propagateUpAll(): Unit = {
    for (up <- ups; down <- downs) {
      up.fromDown.payload ++= down.fromDown.payload
      up.fromDown.payload ++= down.keyToData.keys
      up.alwaysReady = down.alwaysReady
    }
  }
}