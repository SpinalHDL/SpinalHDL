package spinal.lib.misc.pipeline

import spinal.core._
import spinal.lib._
import spinal.lib.misc.pipeline._

class ToplevelA extends Component{
  val a,b,c = new Node()
  val ab = new StageLink(a, b)
  val bc = new StageLink(b, c)

  val xIn = in UInt(8 bits)
  val X = a.insert(xIn)

  val X_PLUS_TWO = Payload(UInt(8 bits))
  b(X_PLUS_TWO) := b(X) + 2

//  val cXPlusOne = c(X) + 1
//  val cXPlusTwo = c(X_PLUS_TWO) + 1
  val onC = new c.Area{
    val xPlusOne = X + 1
    val xPlusThree = X_PLUS_TWO + 1
  }
  Builder(ab, bc)
}

object DemoA extends App{
  SpinalVerilog(new ToplevelA)
}



class ToplevelB extends Component{
  val a,b,c = new Node()
  val ab = new StageLink(a, b)
  val bc = new StageLink(b, c)

  val xIn = UInt(8 bits)
  val X = a.insert(xIn)
  val Y = a.insert(xIn+1)

//  def cube(ARG : Payload[UInt], squareNode : Node, cubeNode : Node) = new Area{
//    val SQUARE = squareNode.insert(squareNode(ARG)*squareNode(ARG))
//    val CUBE = cubeNode.insert(cubeNode(SQUARE)*cubeNode(SQUARE))
//  }

  def cube(ARG: Payload[UInt], squareNode: Node, cubeNode: Node) = new Area {
    val square = new squareNode.Area{
      val RESULT = insert(ARG * ARG)
    }
    val cube = new cubeNode.Area {
      val RESULT = insert(square.RESULT * square.RESULT)
    }
  }


  val cubeA = cube(X, b, c)
  val cubeB = cube(Y, b, c)

  val resultA = c(cubeA.cube.RESULT)
  val resultB = c(cubeB.cube.RESULT)

  Builder(ab, bc)
}

object DemoB extends App{
  SpinalVerilog(new ToplevelB)
}


class ToplevelC extends Component{
  val plusTwoAt = 1
  val nodes = Array.fill(3)(new Node())
  val links = for (i <- 0 until 2) yield new StageLink(nodes(i), nodes(i+1))
  class NodeArea(at : Int) extends NodeMirror(nodes(at))

  val xIn = in UInt(8 bits)
  val X = nodes(0).insert(xIn)

  val X_PLUS_TWO = Payload(UInt(8 bits))
  nodes(plusTwoAt)(X_PLUS_TWO) := nodes(plusTwoAt)(X) + 2

  val onC = new NodeArea(plusTwoAt) {
    val xPlusOne = X + 1
    val xPlusThree = X_PLUS_TWO + 1
  }

//  val lastNode = nodes(2)
//  val onC = new lastNode.Area{
//    val xPlusOne = X + 1
//    val xPlusThree = X_PLUS_TWO + 1
//  }
  Builder(links)
}

object DemoC extends App{
  SpinalVerilog(new ToplevelC)
}


class ToplevelD extends Component{
  val ctrls = Array.fill(3)(CtrlLink())
  val links = for (i <- 0 until 2) yield new StageLink(ctrls(i).down, ctrls(i+1).up)

  val xIn = UInt(8 bits)
  val X = ctrls(0).up.insert(xIn)
  ctrls(1).throwWhen(ctrls(1)(X) === 42)
  ctrls(1).haltWhen(ctrls(1)(X) === 84)

  Builder(links ++ ctrls)
}

object DemoD extends App{
  SpinalVerilog(new ToplevelD)
}
