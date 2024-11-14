package spinal.lib.misc

import spinal.core._
import spinal.core.internals.{BaseNode, Expression}

import scala.collection.mutable

object PathTracer {
  case class Node(node : BaseNode){
    var hits = 0
    val ups = mutable.LinkedHashSet[Node]()

    def report() : String = {
      val str = new StringBuilder()
      def rec(node : Node, prefix : String): Unit = {
        str ++= prefix
        str ++= node.toString
        str ++= "\n"
        for(up <- node.ups; if up.hits != 0) {
          rec(up, "  " + prefix)
        }
      }
      rec(this, "- ")
      str.toString()
    }
  }
  def impl(from: Expression, to: Expression): Node = {
    val walkedId = GlobalData.get.allocateAlgoIncrementale()

    val root = new Node(null)
    rec(to, root)

    def rec(that : BaseNode, tree : Node): Boolean = {
      val node = new Node(that)
      tree.ups += node
      if(that == from){
        node.hits += 1
        return true
      }
      foreach(that){ (input, latency) =>
        if(latency == 0 || that == to){
          if(rec(input, node)){
            node.hits += 1
          }
        }
      }
      node.hits != 0
    }

    def foreach(that: BaseNode)(onUp : (BaseNode, Int) => Unit): Unit = {
      if(that.algoIncrementale == walkedId)
        return
      that.algoIncrementale = walkedId
      if(that == from)
        return

      that match{
        case that : Mem[_] => {
          that.foreachStatements{
            case port : MemWrite =>
              port.foreachDrivingExpression(input => {
                onUp(input, 1)
              })
            case port : MemReadWrite =>
              port.foreachDrivingExpression(input => {
                onUp(input, 1)
              })
            case port : MemReadSync =>
            case port : MemReadAsync =>
            //TODO other ports
          }
        }
        case that : BaseType => { //TODO IR when conds
          def foreachInputs(func : (BaseNode) => Unit) = {
            that.foreachStatements(s => {
              s.foreachDrivingExpression(input => {
                func(input)
              })
              s.walkParentTreeStatementsUntilRootScope(tree => tree.foreachDrivingExpression(input => {
                func(input)
              }))
            })
          }
          if(that.isReg){
            foreachInputs(input => onUp(input, 1))
          } else {
            foreachInputs(input => {
              onUp(input, 0)
            })
          }
        }
        case that : MemReadSync =>
          that.foreachDrivingExpression(input => onUp(input, 1))
          onUp(that.mem, 1)
        case that : MemReadWrite =>
          that.foreachDrivingExpression{input =>
            val lat = if(input == that.data || input == that.mask) 2 else 1
            onUp(input, 1)
          }
          onUp(that.mem, 1)
        case that : MemReadAsync =>
          that.foreachDrivingExpression(input => {
            onUp(input, 0)
          })
          onUp(that.mem,0)
        case that : Expression => {
          that.foreachDrivingExpression(input => {
            onUp(input, 0)
          })
        }
      }
    }
    root.ups.head
  }
}


object PathTracerDemo extends App{
  SpinalVerilog(new Component{
    val a,b,c = in Bool()
    val x = a || b
    val x2 = !a
    when(c){
      x := x2
    }
    val y = !x
    val z = CombInit(y)

    val path = PathTracer.impl(a, z)
    println(path.report())
  })
}