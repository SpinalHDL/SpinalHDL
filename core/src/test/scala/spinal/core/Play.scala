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
    n init(False)
    l := a
    m := a || b
    n := m || l
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
    result := subSubA.result
  }
  class TopLevel extends Component {
    val a, b, c = in Bool()
    val d, e, f = out Bool()
    val g, h, i, j = Bool()
    val x,y,z = Reg(Bool())
    d := a
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
