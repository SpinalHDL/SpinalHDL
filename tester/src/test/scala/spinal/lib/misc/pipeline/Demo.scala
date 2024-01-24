package spinal.lib.misc.pipeline

import spinal.core._
import spinal.lib._
import spinal.lib.graphic.Rgb
import spinal.lib.misc.pipeline._

object Utils{
  def cube(ARG: Payload[UInt], squareNode: Node, cubeNode: Node) = new Area {
    val square = new squareNode.Area {
      val VALUE = insert(ARG * ARG)
    }
    val cube = new cubeNode.Area {
      val VALUE = insert(square.VALUE * square.VALUE)
    }
  }
}

//class ToplevelA extends Component{
//  val io = new Bundle {
//    val up = slave Stream(Rgb(8,8,8))
//    val down = master Stream(Rgb(8,8,8))
//  }
//
//  val a,b,c = new Node()
//  val ab = new StageLink(a,b)
//  val bc = new StageLink(b,c)
//
//  a.arbitrateFrom(io.up)
//  val RED = a.insert(io.up.r)
//  val GREEN = Payload(UInt(8 bits))
//  a(GREEN) := io.up.g
//
//
//  val blueLogic = Utils.cube(a.insert(io.up.b), b, c)
//
//  c.arbitrateTo(io.down)
//
//  val pop = new c.Area {
//    io.down.r := RED
//    io.down.g := GREEN
//    io.down.b := blueLogic.cube.VALUE.resized
//  }
//
//
//  Builder(ab, bc)
//}

//class ToplevelA(squareAt : Int, cubeAt : Int) extends Component{
//  val io = new Bundle {
//    val up = slave Stream(Rgb(8,8,8))
//    val down = master Stream(Rgb(8,8,8))
//  }
//
//  val nodes = List.fill(3)(new Node())
//  val links = for(i <- 0 to 1) yield new StageLink(nodes(i), nodes(i+1))
//
//  val a = nodes(0)
//  val c = nodes(2)
//
//  a.arbitrateFrom(io.up)
//  val RED = a.insert(io.up.r)
//  val GREEN = Payload(UInt(8 bits))
//  a(GREEN) := io.up.g
//
//
//  val blueLogic = Utils.cube(a.insert(io.up.b), nodes(squareAt), nodes(cubeAt))
//
//  c.arbitrateTo(io.down)
//
//  val pop = new c.Area {
//    io.down.r := RED
//    io.down.g := GREEN
//    io.down.b := blueLogic.cube.VALUE.resized
//  }
//
//
//  Builder(links)
//}

class ToplevelA(squareAt : Int, cubeAt : Int) extends Component{
  val io = new Bundle {
    val up = slave Stream(Rgb(8,8,8))
    val down = master Stream(Rgb(8,8,8))
  }

  val ctrls = List.fill(3)(CtrlLink())
  val links = for(i <- 0 to 1) yield new StageLink(ctrls(i).down, ctrls(i+1).up)

  val a = ctrls(0).up
  val c = ctrls(2).down

  a.arbitrateFrom(io.up)
  val RED = a.insert(io.up.r)
  val GREEN = Payload(UInt(8 bits))
  a(GREEN) := io.up.g


  ctrls(1).throwWhen(ctrls(1)(RED) === 2)

  val blueLogic = Utils.cube(a.insert(io.up.b), ctrls(squareAt).down, ctrls(cubeAt).down)

  c.arbitrateTo(io.down)

  val pop = new c.Area {
    io.down.r := RED
    io.down.g := GREEN
    io.down.b := blueLogic.cube.VALUE.resized
  }


  Builder(links ++ ctrls)
}

object DemoA extends App{
  SpinalVerilog(new ToplevelA(1, 2))
}



class ToplevelB extends Component{
  val io = new Bundle {
    val up = slave Stream (Rgb(8, 8, 8))
    val down0, down1 = master Stream (Rgb(8, 8, 8))
  }

  val a = new Node()
  val f0a, f1a = new Node()
  val f0b, f1b = new Node()
  val forker = ForkLink(a, List(f0a, f1a))
  val f0ab = CtrlLink(f0a, f0b)
  val f1ab = CtrlLink(f1a, f1b)

  a.arbitrateFrom(io.up)
  val RGB = a.insert(io.up.payload)

  f0b.arbitrateTo(io.down0)
  f1b.arbitrateTo(io.down1)

  io.down0.payload := f0b(RGB)
  io.down1.payload := f1b(RGB)

  f0ab.throwWhen(f0a(RGB).b === 42)


  Builder(forker, f0ab, f1ab)
}

object DemoB extends App{
  SpinalVerilog(new ToplevelB)
}


class ToplevelC extends Component{
  val nodes = Array.fill(3)(new Node())
  for (i <- 0 until 2) yield new StageLink(nodes(i), nodes(i+1))

//  val plusTwoAt = 1
//  val nodes = Array.fill(3)(new Node())
//  val links = for (i <- 0 until 2) yield new StageLink(nodes(i), nodes(i+1))
//  class NodeArea(at : Int) extends NodeMirror(nodes(at))
//
//  val xIn = in UInt(8 bits)
//  val X = nodes(0).insert(xIn)
//
//  val X_PLUS_TWO = Payload(UInt(8 bits))
//  nodes(plusTwoAt)(X_PLUS_TWO) := nodes(plusTwoAt)(X) + 2
//
//  val onC = new NodeArea(plusTwoAt) {
//    val xPlusOne = X + 1
//    val xPlusThree = X_PLUS_TWO + 1
//  }
//
////  val lastNode = nodes(2)
////  val onC = new lastNode.Area{
////    val xPlusOne = X + 1
////    val xPlusThree = X_PLUS_TWO + 1
////  }
//  Builder(links)
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
