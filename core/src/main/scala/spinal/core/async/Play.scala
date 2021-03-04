package spinal.core.async

import spinal.core._
import spinal.core.async._

object Test1 extends App{

  def doIt() = {
    new Area {
      println("body start")
      val a, b = Handle[Int]

      val c = async {
        async {
          b.get
          println("Got B inner")
        }

        println("c start")
        val x = a.get
        println("got A")
        val y = b.get
        println("got B")
        val sum = x + y
        println("+ done")
        sum
      }

      val printC = async {
        println("print start")
        println(c.get)
      }

      val loadA = async {
        println("load a")
        a.load(3)
      }
      println("load b")
      b.load(4)
    }
  }
  Engine create doIt()
}


object Test2 extends App{
  SpinalVerilog(new Component{
    val a,b = in UInt(8 bits)

    val sub = new Component{
      val c = out(U(0x0C, 8 bits))
    }
    val x = Handle[Int]
    async(println(x.get))
    async(async(x.load(42)))

    val d = async(U(0x0D, 8 bits))


    val sum = out(a + b + sub.c + d.get)

    setDefinitionName("toplevel")
  })
}
