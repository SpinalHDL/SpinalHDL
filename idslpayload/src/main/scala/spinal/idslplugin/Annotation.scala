package spinal.idslplugin

//import scala.annotation.StaticAnnotation
//
//class postInitCallback extends StaticAnnotation
//class valCallback extends StaticAnnotation

trait PostInitCallback {
  def postInitCallback(): this.type
}

trait ValCallback {
  def valCallback[T](ref: T, name: String): T
}
