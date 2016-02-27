package spinal.tester.code

import spinal.core._

//
//class AdderCell extends Component {
//  val io = new Bundle {
//    val a, b, cin = in Bool
//    val sum, cout = out Bool
//  }
//  io.sum := io.a ^ io.b ^ io.cin
//  io.cout := (io.a & io.b) | (io.a & io.cin) | (io.b & io.cin)
//}
//
//class Adder(width: Int) extends Component {
//  val io = new Bundle {
//    val a, b = in UInt (width bit)
//    val cin = in Bool
//    val sum = out UInt (width bit)
//    val cout = out Bool
//  }
//  val cells = Array.fill(width)(new AdderCell)
//  for(i <- 0 until width){
//    cells(i).io.a := io.a(i)
//    cells(i).io.b := io.b(i)
//    io.sum(i) := cells(i).io.sum
//  }
//
//  cells(0).io.cin := io.cin
//  for(i <- 1 until width){
//    cells(i).io.cin := cells(i-1).io.cout
//  }
//  io.cout := cells(width-1).io.cout
//}
//
//object Adder{
//  def main(args: Array[String]): Unit = {
//    SpinalVhdl(new Adder(4))
//  }
//}
//
//object AssignementTrash{
//  class TopLevel extends Component{
//
//    var x = UInt(4 bit)
//    val y,z = out UInt(4 bit)
//    x := 0
//    y := x      //y read x with the value 0
//    x \= x + 1
//    z := x      //z read x with the value 1
//  }
//  def main(args: Array[String]): Unit = {
//    SpinalVhdl(new TopLevel)
//  }
//}
//
//
//object CatTrash{
//  class TopLevel extends Component{
//
//    val output = out(Cat(Seq(True,False,False)))
//  }
//  def main(args: Array[String]): Unit = {
//    SpinalVhdl(new TopLevel)
//  }
//}




object toto {
  import spinal.core._
  class CarryAdder(size: Int) extends Component {
    val io = new Bundle {
      val a = in UInt (size bit)
      val b = in UInt (size bit)
      val result = out UInt (size bit) //result = a + b
    }

    var c = False //Carry, like a VHDL variable
    for (i <- 0 until size) {
      //Create some intermediate value in the loop scope.
      val a = io.a(i)
      val b = io.b(i)

      //The carry adder's asynchronous logic
      io.result(i) := a ^ b ^ c
      c \= (a & b) | (a & c) | (b & c); //variable assignment
    }
  }


  //object Main {
    def main(args: Array[String]) {
      SpinalVhdl(new CarryAdder(4))
    }
 // }
}


// spinal.core contain all basics (Bool, UInt, Bundle, Reg, Component, ..)
import spinal.core._

//A simple component definition
class MyTopLevel extends Component {
  //Define some input/output. Bundle like a VHDL record or a verilog struct.
  val io = new Bundle {
    val a = in Bool
    val b = in Bool
    val c = out Bool
  }

  //Define some asynchronous logic
  io.c := io.a & io.b
}

//This is the main of the project. It create a instance of MyTopLevel and
//call the SpinalHDL library to flush it into a VHDL file.
object MyMain {
  def main(args: Array[String]) {
    SpinalVhdl(new MyTopLevel)
  }
}