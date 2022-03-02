package spinal

import spinal.core._
import spinal.idslplugin._



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
object Main{
  def main(args: Array[String]): Unit = {
    new CCCCCCCCCC1(2,3)
//    def miaou(x : Int)(implicit loc : Location): Unit ={
//      println(s"LOC = ${loc.line} ${loc.file}")
//    }
//
//    miaou(1)
//
//
//    miaou(2)
  }
}


class CCCCCCCCCC1(val aaaa : Int, bbbb : Int) extends Component {
  val io = new Bundle{

  }
  val x = 1
  val y = 2
  val z = new {
    val aa = 5
  }


  println("CCCCCCCCCC1 constructor done")
}

