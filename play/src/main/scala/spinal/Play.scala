package spinal

import spinal.core.*
import spinal.idslplugin.*

import scala.languageFeature.postfixOps



//class Wuff extends PostInitCallback{
//  println("Wuff init")
//  override def postInitCallback() = {
//    println("WUFF postInitCallback")
//    this
//  }
//}
//
//class Rawrr(arg : Int) extends Wuff{
//  println("Rawrr init")
//}
//
//object Miaou {
//  def main(args: Array[String]): Unit = {
//    val x = new Rawrr(2)
//  }
//}

//class IoBundle extends Bundle
//
//class CCCCCCCCCC1 {
//  val x = 4
//  val io = new Bundle
//  val y = 6
//}

//object Main{
//  def main(args: Array[String]): Unit = {
//    new CCCCCCCCCC1
//  }
//}






//object Main{
//  def main(args: Array[String]): Unit = {
//    new CCCCCCCCCC1(2,3)
////    def miaou(x : Int)(implicit loc : Location): Unit ={
////      println(s"LOC = ${loc.line} ${loc.file}")
////    }
////
////    miaou(1)
////
////
////    miaou(2)
//  }
//}
//
//
//class CCCCCCCCCC1(val aaaa : Int, bbbb : Int) extends Component {
//  val io = new Bundle{
//
//  }
//  val x = 1
//  val y = 2
//  val z = new {
//    val aa = 5
//  }
//
//
//  println("CCCCCCCCCC1 constructor done")
//}
object Bool{
  def apply(x : Boolean) = println(x)
  def apply() = new Bool()
}

object in{
  def apply(that : Bool) : Unit = println("in")
  def Bool(dummy : Unit = 2) : Unit = println("in Bool")
}

class Bool{
  println("Miaou")
}


object Main{
  def main(args: Array[String]): Unit = {
    val x = Bool()
    Bool(true)

    in.Bool()
    in Bool()
    in(Bool())

    import spinal.core._
//    import scala.language.postfixOps
    4.bits
    4 bits

    val aa = new Bundle {
      val aaaa = 2
    }

//    println(aa.aaaa)
  }
}




object ColorSummingTester {

  def main(args: Array[String]) {
//    class Namespace
//    val x = new Namespace with {
//      val y = 1
//    }

//    trait Namespace
//    val miaou = new Object with Namespace{
//      val x = 2
//    }
//    println(miaou.x)

//    object miaou extends Namespace{
//      val xx = 2
//    }
//    println(miaou.xx)

//    def rawrrr() = {
//      object miaou extends Namespace {
//        val xx = 2
//      }
//      val miaou = new Namespace{
//
//      }
//    }


//    def builder(arg : Int) = new Namespace{
//      val x = arg
//    }
//
//    val wuff = builder(2)
//    wuff.x

//    def builder(arg : Int) = {
//      object Miaou extends Namespace{
//        val x = arg
//      }
//      Miaou
//    }
//
//    val wuff = builder(2)
//    wuff.x

//    val x = new Namespace {
//      val r = 0
//      val x = new Namespace {
//        val r = 1
//      }
//    }
//    println(x.x.r)

//    class Record(elems: (String, Any)*) extends Selectable:
//      private val fields = elems.toMap
//      def selectDynamic(name: String): Any = fields(name)
//
//    type Person = Record { val name: String; val age: Int }


//    object Miaou{
//      println("Miaou created")
//      def hi = println("hi")
//    }
//
//    println("after Miaou def")
//    Miaou.hi


  }
}