package spinal.dev

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import spinal.core._
import spinal.idslplugin.{PostInitCallback, ValCallback}


//class Bundle extends ValCallback with PostInitCallback{
//  println("Bundle constructor start")
////  def valCallback(ref: Any, name: String): Unit = {
////    println(ref.getClass + " : " + name + " = " + ref)
////  }
//  def postInitCallback(): this.type = {
//    println("miaou")
//    this
//  }
//
//  override def valCallback[T](ref: T, name: String): T = {
//    println(ref.getClass + " :: " + name + " = " + ref)
//    ref
//  }
//
//  val miaou = "wuff"
//  println("Bundle constructor enda")
//}
//
//class RGB(val v : String) extends Bundle {
//
////  new RGB2()
//  def this(v : Int) {
//    this((v*v).toString)
//    println(v)
//  }
//
//  println("RGB constructor start3     ")
//  val r = 3
//  @dontName val g = 4
//  val b = 5
//  println("hello")
//  val x, y, z = 44
//  var t = 3
//  t = 4
//  println("RGB constructor endaaaaa")
//}
//
//object Play {
//  def main(args: Array[String]): Unit = {
//    println("START")
//    new RGB(6368)
//    println("DONE2a")
//  }
//}

//trait WUUU{
////  def y : Int
//}
//class IO extends Bundle with WUUU{
//
//  val x = 443335252
//}
//class Base
class Toplevel extends Component {
  val io = new Bundle{


  }
}








//object Play {
//  def main(args: Array[String]): Unit = {
//    println("START")
//    new RGB(668)
//    println("DONE2a")
//  }
//}