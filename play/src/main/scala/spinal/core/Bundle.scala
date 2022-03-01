package spinal.core

import spinal.idslplugin.{PostInitCallback, ValCallback}

class Bundle
class Area extends ValCallback{
  override def valCallback[T](ref: T, name: String) = {
    println(s"Area got val $name = $ref")
    ref
  }
}
class Component extends PostInitCallback with ValCallback {
  override def postInitCallback() = {
    println("Component post init")
    this
  }
  override def valCallback[T](ref: T, name: String) = {
    println(s"Area got val $name = $ref")
    ref
  }

}
