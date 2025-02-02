package spinal.core.formal

import spinal.core.{Bool, Component, Composite, SpinalTag, True, assert, assume, when}
import spinal.idslplugin.Location

import scala.collection.mutable
import scala.ref.WeakReference
import scala.util.Try

object FormalDut{
  def apply[T <: Component](dut : T) = {
    val c = Component.current
    if(c != null) {
      c.withAutoPull()
      c.setFormalTester()
    }

    dut.asFormalDut()
  }
}
