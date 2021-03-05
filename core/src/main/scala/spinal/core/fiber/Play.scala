package spinal.core.fiber

import spinal.core._
import spinal.core.fiber._

object Test1 extends App{

  def doIt() = {
    new Area {
      println("body start")
      val a, b = Handle[Int]

      val c = hardFork {
        hardFork {
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

      val printC = hardFork {
        println("print start")
        println(c.get)
      }

      val loadA = hardFork {
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
//    val x = Handle[Int]
//    hardFork(println(x.get))
//    hardFork(hardFork(x.load(42)))

    val sum = out(a + b + sub.c)

    setDefinitionName("toplevel")
  })
}


object Test3 extends App{
  SpinalVerilog(new Component{
    val x = Handle[Int] //x defined here

    val a,b = in UInt(8 bits)

    val sub = new Component{
      val c = out(U(0x0C, 8 bits))
    }

    val d = hardFork(U(0x0D, 8 bits))

    val sub2 = new Component{
      val e = hardFork(out(RegNext(U(0x0e, x.get bits))))  //x used here
      val f = ClockDomain.external("miaou") on hardFork(out(RegNext(U(0x0f, x.get bits)))) //x used here
    }


    val rawrrr = hardFork(println(x.get))
    x.load(8) //x set here
    val sum = out(a + b + sub.c + d.get + sub2.e.get + sub2.f.get)

    setDefinitionName("toplevel")
  })
}