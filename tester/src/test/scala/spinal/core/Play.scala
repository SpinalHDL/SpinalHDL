package spinal.core

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import spinal.core._
import spinal.idslplugin.{PostInitCallback, ValCallback}

import scala.annotation.{StaticAnnotation, TypeConstraint}


class miaou extends StaticAnnotation

object IdslPlay{

  def chocolat[T](body : => T@miaou) = {
    body
  }

  val z = 3
  chocolat{
    val x = 1
    val y = 2
    x+y+z+2
  }

//  def main(args: Array[String]): Unit = {
//    val xxx = chocolat{
//      43
//    }
//  }


}





//
////import scala.annotation.StaticAnnotation
////
////class postInitCallback extends StaticAnnotation
////class valCallback extends StaticAnnotation
////@valCallback @postInitCallback


//class Bundle(val v : String) extends PostInitCallback{
//  println("Bundle constructor start")
//  def postInitCallback(): this.type = {
//    println("miaou")
//    this
//  }
//  def this(v : Int) {
//    this((v*v).toString)
//    println(v)
//  }
//  println("Bundle constructor end")
//}
//
//object Play {
//  def main(args: Array[String]): Unit = {
//    println("START")
//    new Bundle(2)
//    println("DONE")
//  }
//}

//class RGB2 extends Bundle {
//  println("A")
//  def this(v : Int) {
//    this()
//    println("B")
//  }
//  new ArrayBuffer[Int]()
//  class Yolo(){
//    println(RGB2.this)
//  }
//
//  new Yolo()
//}
//
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
//    SpinalVerilog(new Component{
//      val a = Bool()
//      val b = UInt(2 bits)
//      val c = Bits(5 bits)
//
//      val raw = B"00001011"
//
////      Vec(a,b,c).assignFromBits(raw)
//    })
//  }
//}
