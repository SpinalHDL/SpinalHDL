package spinal.core

import scala.collection.mutable.ArrayBuffer

/**
 * Created by PIC32F_USER on 14/08/2017.
 */

object PlayReg{

  class TopLevel extends Component {
    val a, b, c = in Bool()
    val d, e, f = out Bool()
    val g, h, i, j = Bool()
    val x,y,z = Reg(Bool())

    val l,m,n = Reg(Bool)
//    n init(False)
//    l := a
    when(a || b || c) {
      m := a || b  || a || b
    }
//    n := m || l
  }

  def main(args: Array[String]) {
    val toplevel = SpinalVhdl(new TopLevel()).toplevel

  }
}



object PlaySimple{

  class TopLevel extends Component {
    val a, b, c = in Bool()
    val d, e, f = out Bool()
    val g, h, i, j = Bool()
    val x,y,z = Reg(Bool())

    def useless(): Unit ={
      a || b || c
      c || b
      True
      False
    }

    useless()
  }

  def main(args: Array[String]) {
    val toplevel = SpinalVhdl(new TopLevel()).toplevel

  }
}


object PlayScope{
  class SubSubA extends Component{
    val a,b = in Bool()
    val result = out Bool()
    val temp = RegNext(a || b)
    result := temp
  }
  class SubA extends Component{
    val a,b = in Bool()
    val result = out Bool()
    val subSubA = new SubSubA()
    subSubA.a := a
    subSubA.b := b
    assert(a,"MIAOU")
    result := subSubA.result
  }
  class TopLevel extends Component {
    val a, b, c = in Bool()
    val d, e, f = out Bool()
    val g, h, i, j = Bool()
    val x,y,z = Reg(Bool())
    d := e
    d := a
    y.init(True)
    when(a){
      z init(False)
    }
    h := True
    e := a || c
    x := d || y
    y := b

    when(c) {
      e := d
      when(d) {
        assert(b,"MIAOU")
        f := e
        e := f
      }
      e := f
    }.elsewhen(a) {
      val x = Bool()
      val y = Reg(Bool())
      x := a || b
      y := True
      i := g || x
      z := False
    } otherwise {
      d := j
    }

    when(a){
      i := c
    }

    when(b){
      d := False
    }

    when(c) {
      val subA = new SubA()
      subA.a := x
      subA.b := j
      subA.b := y || z
      i := subA.result

      d := subA.a
    }

    val l,m,n = Reg(Bool)
    l := a
    m := a || b
    n := m || l
  }

  def main(args: Array[String]) {
    val toplevel = SpinalVhdl(new TopLevel()).toplevel

    var statementCount, expressionCount = 0
    toplevel.dslBody.walkStatements(s => {
      statementCount += 1
      s.walkExpression(e => {
        expressionCount += 1
      })
    })
    print("DONE " + toplevel.getName() + " " + statementCount + " " + expressionCount)
  }
}


object PlayComponent{
  class SubA extends Component{
    val a = in Bool()
    val result = out Bool()

    result := a
  }

  class SubB extends Component{
    val a,b = in Bool()
    val result = out Bool()

    result := a || b
  }

  class TopLevel extends Component {
    val a, b = in Bool()
    val result = out Bool()

    val subA = new SubA
    subA.a := a

    val subB = new SubB
    subB.a := subA.result
    subB.b := b

    result := subB.result
  }

  def main(args: Array[String]) {
    val toplevel = SpinalVhdl(new TopLevel()).toplevel

    var statementCount, expressionCount = 0
    toplevel.dslBody.walkStatements(s => {
      statementCount += 1
      s.walkExpression(e => {
        expressionCount += 1
      })
    })
    print("DONE " + toplevel.getName() + " " + statementCount + " " + expressionCount)
  }
}

object PlayHeavyload {

  class Logic {
    val a, b, c, d, e, f, g, h, i, j = Bool()
    val x,y,z = Reg(Bool())
    b := True
    a := a || c
    x := d || y
    c := b

    when(c) {
      e := d
      when(d) {
        f := e
        e := f
      }
      b := f
    }.elsewhen(a) {
      val x = Bool()
      x := a || b
      i := g || x

    } otherwise {
      b := j
    }
  }
  
  
  class TopLevel(size : Int) extends Component{
    var l = ArrayBuffer[Logic]()
    l.sizeHint(1100000)
    SpinalProgress("TOP START")
    var idx = 0
    while(idx < size) {
      idx += 1
      l += new Logic
      if(idx % 10000 == 0) println(idx)
    }
    l = null
    SpinalProgress("TOP END")
  }

  def main(args: Array[String]) {
    var toplevel = SpinalVhdl(new TopLevel(10000)).toplevel
    toplevel = SpinalVhdl(new TopLevel(50000)).toplevel
    toplevel = SpinalVhdl(new TopLevel(10000)).toplevel
//
//    var statementCount, expressionCount = 0
//    toplevel.dslBody.walkStatements(s => {
//      statementCount += 1
//      s.walkExpression(e => {
//        expressionCount += 1
//      })
//    })
//     print("DONE " + toplevel.getName() + " " + statementCount + " " + expressionCount)
  }
}




object PlayDebug{
  object Miaou{
    def unapply(yolo : Yolo) : Option[Int] = yolo.x match {
      case v : Int => Some(v)
      case _ => None
    }
  }

  case class Yolo(var a : Any){
    val x = a
    println(x)
    def getX = x
  }

  def main(args: Array[String]) {
    val l : List[Any] = List(1,2,"asd", Yolo("miaou"), Yolo(1), Yolo(1), Yolo(Yolo(1)), Yolo(Yolo(1)))
    println("START")
    l.foreach(_ match {
      case Miaou(x: Int) => println(x)
      case _ =>
    })



    println("START A")
    for(i <- 0 until 10) {
      {
        val startAt = System.nanoTime()
        var count = 0
        var idx = 0
        while (idx < 5000000) {
          idx += 1
          l.foreach(_ match {
            case Yolo(x: Int) => count += x
            case _ =>
          })
        }
        val endAt = System.nanoTime()
        println(s"${(endAt - startAt) / 1e6}      $count")
      }
    }
    println("START B")

    for(i <- 0 until 5) {
      {
        val startAt = System.nanoTime()
        var count = 0
        var idx = 0
        while (idx < 5000000) {
          idx += 1
          l.foreach(_ match {
            case yolo : Yolo => if(yolo.a.isInstanceOf[Int]) count += yolo.a.asInstanceOf[Int]
            case _ =>
          })
        }
        val endAt = System.nanoTime()
        println(s"${(endAt - startAt) / 1e6}      $count")
      }
    }

    println("START C")

    for(i <- 0 until 5) {
      {
        val startAt = System.nanoTime()
        var count = 0
        var idx = 0
        while (idx < 5000000) {
          idx += 1
          l.foreach(_ match {
            case yolo : Yolo =>  yolo.a match {
              case i : Int => count += i
              case _ =>
            }
            case _ =>
          })
        }
        val endAt = System.nanoTime()
        println(s"${(endAt - startAt) / 1e6}      $count")
      }
    }
    println("START D")
    for(i <- 0 until 5) {
      {
        val startAt = System.nanoTime()
        var count = 0
        var idx = 0
        while (idx < 5000000) {
          idx += 1
          l.foreach(_ match {
            case Yolo(Yolo(x : Int)) => count += x
            case _ =>
          })
        }
        val endAt = System.nanoTime()
        println(s"${(endAt - startAt) / 1e6}      $count")
      }
    }

    println("START E")
    for(i <- 0 until 5) {
      {
        val startAt = System.nanoTime()
        var count = 0
        var idx = 0
        while (idx < 5000000) {
          idx += 1
          l.foreach(_ match {
            case Miaou(x) => count += x
            case _ =>
          })
        }
        val endAt = System.nanoTime()
        println(s"${(endAt - startAt) / 1e6}      $count")
      }
    }
  }
}