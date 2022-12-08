package spinal.tester.code

import spinal.core._

class CarryAdder(size : Int) extends Component{
  val io = new Bundle{
    val a = in UInt(size bit)
    val b = in UInt(size bit)
    val result = out UInt(size bit)
  }

  var c = False
  for (i <- 0 until size) {
    val a = io.a(i)
    val b = io.a(i)
    io.result(i) := a ^ b ^ c
    c \= (a & b) | (a & c) | (b & c);
  }
}


object CarryAdderProject {
  def main(args: Array[String]) {
    SpinalVhdl(new CarryAdder(4))
  }
}