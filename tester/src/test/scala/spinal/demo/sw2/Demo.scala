package spinal.demo.sw2

import spinal.core._
import spinal.lib._
import spinal.lib.graphic.Rgb
import spinal.lib.misc.pipeline._

object Utils{
  def cube(ARG : Payload[UInt], squareNode : Node, cubeNode : Node) = new Area{
    val square = new squareNode.Area{
      val VALUE = insert(ARG*ARG)
    }
    val cube = new cubeNode.Area {
      val VALUE = insert(square.VALUE * square.VALUE)
    }
  }
}

case class Toplevel(squareAt : Int, cubeAt : Int) extends Component{
  val io = new Bundle {
    val up = slave Stream(Rgb(8, 8, 8))
    val down = master Stream(Rgb(8, 8, 8))
  }

  val ctrls = List.fill(3)(CtrlLink())
  val links = for(i <- 0 to 1) yield new StageLink(ctrls(i).down, ctrls(i+1).up)

  val a = ctrls(0)
  val b = ctrls(1)
  val c = ctrls(2)

  val SOMETHING = Payload(Bool())
  b(SOMETHING) := False

  a.up.arbitrateFrom(io.up)
  val RED = a.insert(io.up.r)
  val BLUE = a.insert(io.up.b)

  val blueCube = Utils.cube(BLUE, ctrls(squareAt).down, ctrls(cubeAt).down)
//  val SQUARE = b.insert(b(RED) * b(RED))
//  val CUBE = c.insert(c(SQUARE) * c(SQUARE))

  val GREEN = Payload(UInt(8 bits))
  a(GREEN) := io.up.g

  ctrls(1).throwWhen(ctrls(1)(GREEN) === 42)
  when(ctrls(1)(RED) === 0x54){
    ctrls(1).bypass(GREEN) := 0
  }

  val onC = new c.Area {
    down.arbitrateTo(io.down)
    io.down.r := RED
    io.down.g := GREEN
    io.down.b := blueCube.cube.VALUE.resize(8 bits)
  }

  Builder(links ++ ctrls)
}

object ToplevelGen extends App{
  SpinalVerilog(Toplevel(1, 2))
}

