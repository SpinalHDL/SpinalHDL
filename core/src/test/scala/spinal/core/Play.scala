package spinal.core

import scala.collection.mutable.ArrayBuffer

/**
 * Created by PIC32F_USER on 14/08/2017.
 */


object PlayScope{
  class SubSubA extends Component{
    val a,b = in Bool()
    val result = out Bool()
    val temp = a || b
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
      x := a || b
      i := g || x

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
      subA.b := y || z
      i := subA.result
    }

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
  
  
  class TopLevel extends Component{
    var l = ArrayBuffer[Logic]()
    l.sizeHint(1100000)
    SpinalProgress("TOP START")
    var idx = 0
    while(idx < 500000) {
      idx += 1
      l += new Logic
      if(idx % 10000 == 0) println(idx)
    }
    l = null
    SpinalProgress("TOP END")
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
