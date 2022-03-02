package spinal.core

import spinal.idslplugin.{PostInitCallback, ValCallback}
import scala.reflect.Selectable
class Namespace extends Selectable
abstract class Bundle{

}
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
