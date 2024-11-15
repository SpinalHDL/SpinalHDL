package spinal.lib.misc

import spinal.core
import spinal.core._
import spinal.core.internals.{BaseNode, Expression}

import scala.collection.mutable

object PathTracer {
  class Node(val node : BaseNode){
    override def toString = node.toString

    var hits = 0
    val ups = mutable.LinkedHashSet[Node]()
    val downs = mutable.LinkedHashSet[Node]()

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

    def reportPaths() : String = {
      val str = new StringBuilder()
      def rec(node : Node, stack : List[Node]): Unit = {
        if(node.ups.nonEmpty) {
          for(up <- node.ups; if up.hits != 0) {
            rec(up, up :: stack)
          }
        } else {
          str ++= "######\n"
          for(e <- stack){
            str ++= ("- " + e.node + "\n")
          }
        }
      }
      rec(this, List(this))
      str.toString()
    }

    def reportNodes() : String = {
      val set = mutable.LinkedHashSet[Node]()
      val str = new StringBuilder()
      def rec(node : Node, stack : List[Node]): Unit = {
        set += node
        if(node.ups.nonEmpty) {
          for(up <- node.ups; if up.hits != 0) {
            rec(up, up :: stack)
          }
        }
      }
      rec(this, List(this))
      val strings = set.toArray.filter(_.node.isInstanceOf[core.Nameable]).map(e => s"- ${e.node}\n")
      val sorted = strings.sorted
      sorted.foreach(str ++= _)
      str.toString()
    }
  }
  def impl(from: Expression, to: Expression): Node = {
    val walkedId = GlobalData.get.allocateAlgoIncrementale()
    val keyToNode = mutable.LinkedHashMap[BaseNode, Node]()

    val toNode = new Node(to)
    var fromNode : Node = null
    rec(toNode)
    if(fromNode != null){
      flag(fromNode)
      def flag(that : Node): Unit = {
        that.hits += 1
        that.downs.foreach(flag)
      }
    }


    def rec(tree : Node) : Unit = {
      if(tree.node == from && tree != toNode) return
      foreach(tree.node){ (input, latency) =>
        if(latency == 0 || tree == toNode){
          val node = keyToNode.get(input) match {
            case Some(value) => value
            case None => {
              val n = new Node(input)
              keyToNode(input) = n
              rec(n)
              n
            }
          }
          node.downs += tree
          tree.ups += node
          if(input == from){
            fromNode = node
          }
        }
      }
    }

    def foreach(that: BaseNode)(onUp : (BaseNode, Int) => Unit): Unit = {
//      if(that.algoIncrementale == walkedId)
//        return
//      that.algoIncrementale = walkedId
//      if(that == from)
//        return

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
    toNode
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

    val l = Bool()
    val m = CombInit(l)
    val n = CombInit(m)
    val o = RegNext(n)
    l := o

    val path = PathTracer.impl(o, o)
    println(path.report())
    println(path.reportPaths())
    println(path.reportNodes())
  })
}