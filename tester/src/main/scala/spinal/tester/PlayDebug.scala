package spinal.tester

import spinal.core._
import spinal.lib._


object PlayDebug{



  def main(args: Array[String]): Unit = {
    try{
      println("throw")
      throw new Exception("asd")
    }/* catch {
      case e : Exception => {
        println("exception")
        throw new Exception("Rawrrr")
      }
    } */finally {
      println("finaly")
    }
  }
}

object PlayBitsSlice extends App{
  class Top extends Component{
    val io = new Bundle{
      val x = in Bits(32 bits)
      val y = in SInt(32 bits)
      val z = in UInt(32 bits)
    }

    val a = io.x.take(10)
    val b = io.y.takeHigh(10)
    val c = io.z.sliceBy(10,10,12)
    val d = io.y.drop(10)
    val e = io.y.dropHigh(10)
    val f = io.z.drop(10)
    val g = io.z.dropHigh(10)
    val h = io.y.sliceBy(List(2,3,4,1,22))
    val h0 = io.y.take(10)
    val h1 = io.y.takeLow(10)
    val h2 = io.y.drop(10)
    val h3 = io.y.dropHigh(10)
  }
  SpinalVerilog(new Top)
}